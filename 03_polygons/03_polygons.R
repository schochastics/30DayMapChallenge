# http://www.statsmapsnpix.com/2016/12/creating-3d-city-model-using-open-data.html
# https://gridreferencefinder.com/
# https://osdatahub.os.uk/downloads/open/OpenMapLocal
library(tidyverse)
library(sf)
library(ggfx)

sj <- st_read("data/opmplc_essh_sj/OS OpenMap Local (ESRI Shape File) SJ/data/SJ_Building.shp")
sd <- st_read("data/opmplc_essh_sd/OS OpenMap Local (ESRI Shape File) SD/data/SD_Building.shp")

sj_crop <- st_crop(sj, xmin = 383893 - 5500, xmax = 383893 + 5500, 
                   ymin = 398077 - 5500, ymax = 398077 + 5500) 
sd_crop <- st_crop(sd, xmin = 383893 - 5500, xmax = 383893 + 5500, 
                   ymin = 398077 - 5500, ymax = 398077 + 5500) 

gm_crop <- bind_rows(sj_crop, sd_crop)
neon <- c(
  "#C724B1", "#4D4DFF", "#E0E722", "#FFAD00",
  "#D22730", "#DB3EB1", "#44D62C"
)
gm_crop$col <- sample(neon, nrow(gm_crop), replace = TRUE)
circle <- data.frame(
  x = 383893 + 5000 * cos(seq(0, 2 * pi, length.out = 360)),
  y = 398077 + 5000 * sin(seq(0, 2 * pi, length.out = 360))
)

p <- ggplot() +
  as_reference(
    geom_polygon(aes(x = x, y = y), circle),
    id = "circle"
  ) +
  with_mask(
    geom_sf(data = gm_crop, aes(fill = col), color = "grey66", size = 0.01),
    mask = ch_alpha("circle")
  ) +
  geom_polygon(data = circle, aes(x = x, y = y), col = "white", fill = NA, size = 1.5) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.title = element_text(color = neon[3], family = "Xanh Mono", size = 58, hjust = 0.5),
    plot.subtitle = element_text(color = neon[3], family = "Xanh Mono", size = 46, hjust = 0.5),
    plot.caption = element_text(color = "white", family = "Xanh Mono", size = 28),
    plot.background = element_rect(fill = "black")
  ) +
  labs(title = "Manchester", subtitle = "(53.479167, -2.2441643)", caption = "@schochastics")
# p
ragg::agg_png("03_polygons.png", width = 1500, height = 1500, units = "px")
p
dev.off()
cmd <- paste0("convert ", '"', "03_polygons.png", '"', ' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)