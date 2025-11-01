# ----- Libraries -----
library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(grid)
library(tidyr)
library(readr)
library(dplyr)
library(stringr)

# ----- Read data -----
# CSV needs columns: Name, Year, Mentee, Intern
data <- read_csv("names.csv", show_col_types = FALSE)

# ----- Helper: expand multi-dst edge lists & clean -----
to_edges <- function(src, dst, relation) {
  # Normalize separators to ';' then split
  dst_norm <- str_replace_all(dst %||% NA_character_, "[,|\\u2013\\u2014]", ";")
  df <- tibble(src = src, dst = dst_norm, relation = relation)
  
  # Trim, drop NA/empty
  df <- df %>%
    mutate(dst = str_squish(dst)) %>%
    filter(!is.na(dst), nzchar(dst))
  
  # Split on ';' if present
  has_multi <- str_detect(df$dst, fixed(";"))
  if (any(has_multi)) {
    expanded <- map_dfr(which(has_multi), function(i) {
      tibble(
        src = df$src[i],
        dst = str_squish(unlist(str_split(df$dst[i], fixed(";")))),
        relation = df$relation[i]
      )
    })
    df <- bind_rows(df[!has_multi, ], expanded)
  }
  
  df %>%
    mutate(dst = str_squish(dst))
}

# ----- Build edges from the two relationship columns -----
edges_all <- bind_rows(
  to_edges(data$Name,   data$Mentee, "Mentee"),
  to_edges(data$Name,   data$Intern, "Intern")
) %>%
  filter(!is.na(dst), nzchar(dst)) %>%
  distinct() %>%                                 # drop duplicates
  mutate(relation = factor(relation, c("Mentee","Intern")))

# Early exit if no edges
if (nrow(edges_all) == 0) stop("No edges found from Mentee/Intern columns.")

# ----- Graph -> tidygraph -----
g <- graph_from_data_frame(edges_all, directed = TRUE)
g_tbl <- as_tbl_graph(g)

# ----- Attach Year to nodes (by joining known Years from 'Name' rows) -----
years_by_name <- data %>%
  group_by(Name) %>%
  summarise(Year = suppressWarnings(as.integer(first(na.omit(Year)))), .groups = "drop")

# Join onto existing node table; nodes not in Name will keep NA
node_years <- g_tbl %>%
  activate(nodes) %>%
  as_tibble() %>%
  left_join(years_by_name, by = c("name" = "Name"))

g_tbl <- g_tbl %>%
  activate(nodes) %>%
  mutate(Year = node_years$Year)

# ----- Layers from Year (fallback pushes NA to one layer below the latest year) -----
yr_vec <- g_tbl %>% activate(nodes) %>% as_tibble() %>% pull(Year)
yr_fallback <- if (all(is.na(yr_vec))) 0L else max(yr_vec, na.rm = TRUE) + 1L
yr_layers <- ifelse(is.na(yr_vec), yr_fallback, yr_vec) %>% as.integer()

# Optional: warn if there are cycles (Sugiyama prefers DAGs)
if (!igraph::is_dag(g)) {
  message("Note: Graph has cycles; Sugiyama may produce crossings. Consider reviewing edges.")
}

# ----- Layout (deterministic) -----
set.seed(1)
lay <- create_layout(
  g_tbl,
  layout   = "sugiyama",
  circular = FALSE,
  layers   = yr_layers
)

# Better label wrapping (keeps short names on one line)
lay$label2 <- str_wrap(lay$name, width = 5)

# ----- Plot -----
# ----- Plot -----
p <- ggraph(lay) +
  # Mentee edges = elbow (orthogonal)
  geom_edge_bend(
    aes(color = relation, filter = relation == "Mentee"),
    start_cap  = circle(4, "mm"),
    end_cap    = circle(4, "mm"),
    linewidth  = 1.4,
    strength   = 0.5
  ) +
  # Intern edges = diagonal (smooth)
  geom_edge_link(
    aes(color = relation, filter = relation == "Intern"),
    start_cap  = circle(4, "mm"),
    end_cap    = circle(4, "mm"),
    linewidth  = 1,
    linetype   = "dashed"
  ) +
  geom_node_label(
    aes(label = label2),
    fill          = "lightgoldenrod",
    color         = "black",
    label.padding = unit(0.1, "lines"),
    label.r       = unit(0.1, "lines"),
    size          = 4
  ) +
  scale_edge_color_manual(values = c("Mentee" = "steelblue", "Intern" = "tomato")) +
  theme_void() +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin     = margin(30, 30, 30, 30),
    legend.position = "bottom",
    legend.title    = element_blank(),
    legend.key.size = unit(20, "mm"),
    legend.text     = element_text(size = 20)
  )


# ----- Export (PDF, large but manageable) -----
ggsave(
  filename  = "dendro_plot.pdf",
  plot      = p,
  device    = cairo_pdf,
  width     = 28,    # inches
  height    = 10,     # inches
  dpi       = 300
)
