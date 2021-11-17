library(tidyverse)
library(igraph)
library(ggraph)
library(edgebundle)
library(sf)
airport <- vroom::vroom("~/Documents/data/flights/airports.dat", col_names = FALSE)
routes <- vroom::vroom("~/Documents/data/flights/routes.dat", col_names = FALSE)

nodes <- airport[, c(5, 7, 8)] %>%
  distinct(X5, .keep_all = TRUE) %>%
  dplyr::filter(!is.na(X5))

names(nodes) <- c("name", "lat", "long")
edges <- routes[, c(3, 5)] %>% dplyr::filter(!is.na(X3), !is.na(X5))
names(edges) <- c("source", "target")
edges <- edges %>% dplyr::filter(source %in% nodes$name, target %in% nodes$name)

g <- graph_from_data_frame(edges, F, nodes)

world <- rnaturalearth::ne_countries(scale = "medium", type = "countries", returnclass = "sf")

pbundle <- edgebundle::edge_bundle_path(g, cbind(V(g)$long, V(g)$lat), max_distortion = 20)

pbundle_sf <- pbundle %>%
  st_as_sf(coords = c("x", "y"), agr = "constant") %>%
  group_by(group) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

st_crs(pbundle_sf) <- st_crs(world)
idx <- unique(pbundle$group[(pbundle$x <= -173)])
idx <- pbundle %>%
  group_by(group) %>%
  dplyr::summarise(abs = max(x) - min(x)) %>%
  arrange(-abs) %>%
  pull(group) %>%
  .[1:86]
p <- ggplot() +
  geom_sf(data = world, fill = "#fbedee", col = "#f0b8be") +
  geom_sf(data = pbundle_sf[-idx, ], col = "#db4f5d", size = 0.05) +
  geom_sf(data = pbundle_sf[-idx, ], col = "white", size = 0.001) +
  coord_sf(crs = "+proj=robin") +
  labs(
    title = "Global Flight Network",
    caption = "··· data:openflights.org ··· @schochastics ···"
  ) +
  theme_void() +
  theme(
    plot.title = element_text(color = "#db4f5d", size = 104, hjust = 0.5, family = "Chopin Script"),
    plot.caption = element_text(color = "#db4f5d", size = 28, hjust = 0.5, family = "Open Sans"),
    panel.background = element_rect(fill = "white", colour = "white")
  )

ragg::agg_png("09_monochrome.png", width = 2500, height = 2500)
p
dev.off()