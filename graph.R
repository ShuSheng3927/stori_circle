library(igraph)
library(ggraph)
library(tidygraph)
library(ggplot2)
library(grid)
library(tidyr)

library(readr)
data <- read_csv("names.csv")


to_edges <- function(src, dst, relation) {
  df <- data.frame(src = src, dst = dst, relation = relation, stringsAsFactors = FALSE)
  # drop NA / empty targets
  df$dst <- trimws(df$dst)
  df <- df[!is.na(df$dst) & nzchar(df$dst), , drop = FALSE]
  
  # expand semicolon-separated targets in a single cell
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

# Combine both edge types
edges_all <- rbind(edges_mentee, edges_intern)

# Create a directed graph
g <- graph_from_data_frame(edges_all, directed = TRUE)

# Convert to tidygraph
g_tbl <- as_tbl_graph(g)

library(stringr)

lay <- create_layout(g_tbl, layout = "linear", circular = TRUE)
R <- 3
lay$x <- lay$x * R
lay$y <- lay$y * R

lay$label2 <- sub(" ", "\n", lay$name, fixed = TRUE)

# ---- PLOT ----
p <- ggraph(lay) +
  # draw base edges without arrows
  geom_edge_arc2(
    aes(color = relation),
    arrow = arrow(length = unit(5, "mm")),
    start_cap = circle(10, "mm"),
    end_cap   = circle(10, "mm"),
    width = 1.5
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
  theme(plot.margin = margin(40, 40, 40, 40), legend.position = "bottom",legend.title = element_blank(), legend.key.size=unit(25, "mm"),legend.text=element_text(size=30))

ggsave(
  filename = "network_plot.pdf",
  plot = p,
  device = cairo_pdf,       # better text rendering
  width = 120, height = 100,  # inches — adjust as needed
  dpi = 300,                # not needed for vector, but helps previews
  limitsize = FALSE,
  scale = 0.18
)
