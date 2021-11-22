# https://ghsl.jrc.ec.europa.eu/download.php?ds=smod
# https://ghsl.jrc.ec.europa.eu/ghs_bu2019.php
library(raster)
library(rayshader)
world <- raster("GHS_BUILT_1K/GHS_BUILT_LDS2014_GLOBE_R2018A_54009_1K_V2_0.tif")
e <- as(extent(-15.820313,29.355469,35.460670,59.445075), 'SpatialPolygons')
crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
e <- projectExtent(e,"+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
europe <- crop(world, e)

eu_mat <- raster_to_matrix(europe)
eu_mat <- resize_matrix(eu_mat,0.5)
eu_mat[is.na(eu_mat)] <- 0
eu_mat %>%
  sphere_shade(texture = rayshader::create_texture("#000000", "#000000", "#000000", "#000000", "#ffffff")) %>%
  plot_3d(eu_mat,
          zscale = 5,
          fov = 0, 
          theta = 6.42,
          zoom = 0.65,
          phi = 23.20,
          soliddepth = -10, solidcolor = "white", shadow = TRUE, shadowdepth = -12,
          shadowcolor = "black", background = "white",
          windowsize = c(1200, 1200)
  )

render_snapshot("23_ghsl.png",software_render = FALSE, vignette = FALSE)
