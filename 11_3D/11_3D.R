# https://www.worldpop.org
library(rayshader)
library(raster)

swe_pop <- raster("swe_pd_2020_1km.tif")

swe <- raster_to_matrix(swe_pop)

swe %>% 
  sphere_shade(texture = rayshader::create_texture("#FCD02D","#FCD02D","#FCD02D","#FCD02D","#10588E")) %>% 
  plot_3d(swe, zscale = 25, 
          fov = 0, theta = 0, 
          zoom = 0.85, 
          phi = 45, 
          soliddepth = -20,solidcolor = "grey66", shadow = TRUE, shadowdepth = -50,
          shadowcolor = "white",background = "black",
          windowsize = c(1200, 1000))


render_snapshot("12_population.png", software_render = TRUE,width = 3000,height = 1500,cache_filename = "tmp",
                vignette = FALSE)  

nor_pop <- raster("nor_pd_2020_1km.tif")

nor <- raster_to_matrix(nor_pop)

nor %>% 
  sphere_shade(texture = rayshader::create_texture("#00205B","#00205B","#00205B","#00205B","#BA0C2F")) %>% 
  plot_3d(nor, zscale = 5, 
          fov = 0, theta = 0, 
          zoom = 0.75, 
          phi = 45, 
          soliddepth = -20,solidcolor = "grey66", shadow = TRUE, shadowdepth = -50,
          shadowcolor = "white",background = "black",
          windowsize = c(3000, 1500))


render_snapshot("12_supplement_nor.png", software_render = FALSE,vignette = FALSE)  

fin_pop <- raster("fin_pd_2020_1km.tif")

fin <- raster_to_matrix(fin_pop)

fin %>% 
  sphere_shade(texture = rayshader::create_texture("#002F6C","#002F6C","#002F6C","#002F6C","white")) %>% 
  plot_3d(fin, zscale = 5, 
          fov = 0, theta = 0, 
          zoom = 0.95, 
          phi = 45, 
          soliddepth = -20,solidcolor = "grey66", shadow = TRUE, shadowdepth = -50,
          shadowcolor = "white",background = "black",
          windowsize = c(3000, 1500))


render_snapshot("12_supplement_fin.png", software_render = FALSE,vignette = FALSE)  

ice_pop <- raster("isl_pc_2020_1km.tif")

ice <- raster_to_matrix(ice_pop)

Reykjavik <- c(-21.9426,64.1466)
ice %>% 
  sphere_shade(texture = rayshader::create_texture("#02529C","#02529C","#02529C","#02529C","white")) %>% 
  plot_3d(ice, zscale = 1.5, 
          fov = 0, theta = 0, 
          zoom = 1.0, 
          phi = 45, 
          soliddepth = -40,solidcolor = "grey66", shadow = TRUE, shadowdepth = -70,
          windowsize = c(1200, 1000))

render_snapshot("12_population.png",
                title_text = "",
                title_offset = c(60,50),
                title_color = "black",
                title_size = 40,
                title_font = "Oxygen",
                vignette = FALSE)  
