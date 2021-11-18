# https://www.hydrosheds.org/downloads
library(tidyverse)
library(sf)
sf_use_s2(FALSE)

# bas30_files <- list.files("hydrosheds/raw",full.names = TRUE,pattern = "bas_30s")
# sapply(bas30_files,unzip,exdir = "basins")
# riv30_files <- list.files("hydrosheds/raw",full.names = TRUE,pattern = "riv_30s")
# sapply(riv30_files,unzip,exdir = "rivers")

bas30_shps <- list.files("basins", full.names = TRUE, pattern = "shp$")[5]
base30_geo <- map_dfr(bas30_shps, st_read)
riv30_shps <- list.files("rivers", full.names = TRUE, pattern = "shp$")[5]
riv30_geo <- map_dfr(riv30_shps, st_read)

bigbasin20 <- which(base30_geo$AREA_SQKM > 20)
base30_geo_big <- base30_geo[bigbasin20, ]

idx <- readRDS("basinMap.RDS") # st_intersects(riv30_geo,base30_geo_big)
idx1 <- map_dbl(idx, function(x) ifelse(length(x) == 0, 0, x))
riv30_geo$basin <- idx1
cols <- c("white", sample(rep(viridisLite::turbo(35), 242)))
riv30_geo$basin_col <- cols[riv30_geo$basin + 1]
ragg::agg_png("18_water.png", width = 5000, height = 5000)
ggplot() +
  geom_sf(data = riv30_geo, size = 1, col = riv30_geo$basin_col, fill = NA) +
  annotate("text", x = 10, y = 25, label = "River Network\nand\nBasins", 
           size = 94, family = "Xanh Mono", color = "#39FF14") +
  annotate("text", x = 10, y = 16, 
           label = "data from hydrosheds.org · visualized by @schochastics", 
           size = 24, family = "Xanh Mono", color = "#39FF14") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#282828"))
dev.off()

cmd <- paste0("convert ", '"', "18_water.png", '"', ' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)
