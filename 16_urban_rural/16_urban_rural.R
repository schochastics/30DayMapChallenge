# https://www.gislounge.com/new-gis-dataset-on-the-interconnection-between-urban-centers-and-rural-areas/
library(sf)
library(tidyverse)
library(terra)
library(stars)

# colors
cols_lvls <- c(
  "#252525", "#636363", "#969696", "#CCCCCC", "#08519C", "#3182BD",
  "#6BAED6", "#BDD7E7", "#E6550D", "#FD8D3C", "#FDBE85", "#FEEDDE",
  "#31a354", "#a1d99b"
)

urb_tif <- rast("Urban-Rural Catchment Areas (URCA).tif")
urb_tif_small <- terra::aggregate(urb_tif, fact = 2, fun = "min")

terra::writeRaster(urb_tif_small, "urban_small.tif", overwrite = TRUE)
x <- stars::read_stars("urban_small.tif")
urca <- st_as_sf(x, as_points = FALSE, merge = TRUE)
saveRDS(urca, "urban_shp.RDS")
urca <- readRDS("urban_shp.RDS")
names(urca)[1] <- "lev"
urca$lev <- round(urca$lev)
urca$col <- case_when(
  urca$lev %in% c(1, 2) ~ "Large city (>1 mil.)",
  urca$lev %in% c(3, 4) ~ "Intermediate city (0.25 - 1 mil.)",
  urca$lev %in% c(5, 6, 7) ~ "Small cities and towns (0.02 - 0.25 mil.)",
  urca$lev %in% c(8, 9) ~ "<1 hour to large city",
  urca$lev %in% c(10, 11) ~ "<1 hour to intermediate city",
  urca$lev %in% c(12, 13, 14) ~ "<1 hour to small city or town",
  urca$lev %in% c(15, 16) ~ "1-2 hour to large city",
  urca$lev %in% c(17, 18) ~ "1-2 hour to intermediate city",
  urca$lev %in% c(19, 20, 21) ~ "1-2 hour to small city or town",
  urca$lev %in% c(22, 23) ~ "2-3 hour to large city",
  urca$lev %in% c(24, 25) ~ "2-3 hour to intermediate city",
  urca$lev %in% c(26, 27, 28) ~ "2-3 hour to small city or town",
  urca$lev %in% c(29) ~ "Dispersed towns",
  urca$lev %in% c(30) ~ "Hinterland"
)

levels <- c(
  "Large city (>1 mil.)",
  "<1 hour to large city",
  "1-2 hour to large city",
  "2-3 hour to large city",
  "Intermediate city (0.25 - 1 mil.)",
  "<1 hour to intermediate city",
  "1-2 hour to intermediate city",
  "2-3 hour to intermediate city",
  "Small cities and towns (0.02 - 0.25 mil.)",
  "<1 hour to small city or town",
  "1-2 hour to small city or town",
  "2-3 hour to small city or town",
  "Dispersed towns", "Hinterland"
)

urca$col <- factor(urca$col, levels = levels)

ragg::agg_png("16_urban_rural.png", width = 8000, height = 4000, units = "px")
ggplot() +
  geom_sf(data = urca, aes(fill = col, col = col), size = 0.1) +
  scale_fill_manual(values = cols_lvls, name = "") +
  scale_color_manual(values = cols_lvls, name = "", guide = "none") +
  coord_sf(crs = "+proj=eck1") +
  theme_void() +
  theme(
    panel.grid.major = element_line(color = "black"),
    plot.background = element_rect(fill = "white", color = "black", size = 2),
    legend.position = "bottom",
    legend.text = element_text(
      family = "Oxygen", margin = margin(r = 30),
      color = "black",
      size = 42 * 2
    ),
    legend.key.size = unit(5, "lines"),
    plot.title = element_text(hjust = 0.5, size = 116 * 2, family = "Lobster Two", margin = margin(t = 54)),
    plot.subtitle = element_text(hjust = 0.5, size = 66 * 2, family = "Oxygen"),
    plot.caption = element_text(hjust = 0.5, size = 46 * 2, family = "Oxygen", margin = margin(t = 54))
  ) +
  labs(
    title = "Urban–Rural Catchment Areas",
    subtitle = "Urban centers classified by population size",
    caption = "data: 10.1073/pnas.2011990118 · visualized by @schochastics"
  ) +
  guides(fill = guide_legend(ncol = 4, byrow = TRUE))
dev.off()

cmd <- paste0("convert ", '"', "16_urban_rural.png", '"', ' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)