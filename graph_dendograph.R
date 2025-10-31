library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(grid)
library(tidyr)
library(readr)
library(dplyr)
library(stringr)

data <- read_csv("names.csv")

to_edges <- function(src, dst, relation) {
  df <- data.frame(src = src, dst = dst, relation = relation, stringsAsFactors = FALSE)
  df$dst <- trimws(df$dst)
  df <- df[!is.na(df$dst) & nzchar(df$dst), , drop = FALSE]
  has_multi <- grepl(";", df$dst, fixed = TRUE)
  if (any(has_multi)) {
    expanded <- do.call(rbind, lapply(which(has_multi), function(i) {
      parts <- strsplit(df$dst[i], ";", fixed = TRUE)[[1]]
      data.frame(
        src = df$src[i],
        dst = trimws(parts),
        relation = df$relation[i],
        stringsAsFactors = FALSE
      )
    }))
    df <- rbind(df[!has_multi, , drop = FALSE], expanded)
  }
  df
}

# Build edge lists from the CSV columns
edges_mentee <- to_edges(data$Name, data$Mentee, "Mentee")
edges_intern <- to_edges(data$Name, data$Intern, "Intern")
edges_all <- rbind(edges_mentee, edges_intern)

# Create graph and convert to tidygraph
g <- graph_from_data_frame(edges_all, directed = TRUE)
g_tbl <- as_tbl_graph(g)

# ---- attach Year to nodes ----
years_by_name <- data %>%
  dplyr::group_by(Name) %>%
  dplyr::summarise(Year = dplyr::first(na.omit(Year)), .groups = "drop")

# Make a named lookup vector: name -> year
year_map <- years_by_name$Year
names(year_map) <- years_by_name$Name

# Assign Year to each vertex by name; unmatched names become NA
g_tbl <- g_tbl %>%
  activate(nodes) %>%
  mutate(Year = unname(year_map[name]))

# Optional: if *all* Years are NA, keep layout from breaking by inventing a fallback year
yr_vec <- g_tbl %>% activate(nodes) %>% as_tibble() %>% pull(Year)
yr_fallback <- if (all(is.na(yr_vec))) 0 else max(yr_vec, na.rm = TRUE) + 1
yr_layers <- ifelse(is.na(yr_vec), yr_fallback, yr_vec)

# ---- LAYOUT: layered dendrogram by Year ----
lay <- create_layout(
  g_tbl,
  layout = "sugiyama",     # layered DAG layout
  circular = FALSE,
  layers = yr_layers       # force lanes by Year
)

# (keep your label formatting)
lay$label2 <- gsub(" ", "\n", lay$name, fixed = TRUE)

# ---- PLOT (same as before, but with scale_y_reverse to "go down by years") ----
p <- ggraph(lay) +
  geom_edge_diagonal2(
    aes(color = relation),
    strength=1,
    # arrow = arrow(length = unit(5, "mm")),
    start_cap = circle(6, "mm"),
    end_cap   = circle(6, "mm"),
    linewidth = 1.5
  ) +
  geom_node_label(
    aes(label = label2),
    fill = "lightgoldenrod",
    color = "black",
    label.padding = unit(0.15, "lines"),
    label.r = unit(0.1, "lines"),
    size = 4.5
  ) +
  scale_edge_color_manual(values = c("Mentee" = "steelblue", "Intern" = "tomato")) +
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(40, 40, 40, 40),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.key.size = unit(25, "mm"),
    legend.text = element_text(size = 30)
  )

ggsave(
  filename = "dendro_plot.pdf",
  plot = p,
  device = cairo_pdf,
  width = 370, height = 160,
  dpi = 300,
  limitsize = FALSE,
  scale = 0.07
)
