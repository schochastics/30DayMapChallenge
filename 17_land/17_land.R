library(raster)
library(tmap)

forest <- raster("FOR_2000.asc")
crs(forest) <- "+proj=longlat"

map1 <- tm_shape(forest, projection = "+proj=robin", raster.warp = FALSE) +
  tm_raster(
    n = 5, palette = c("white", "#cbe368", "#719b25", "#2e6409", "#2c4928"), 
    breaks = c(1, 20, 40, 60, 80, 100),
    legend.show = TRUE
  ) +
  tm_layout(
    sepia.intensity = 0, saturation = 1, frame = TRUE,
    main.title = "Share of Forest Land", title = "(data from https://webarchive.iiasa.ac.at/)",
    title.position = c("right", "top"), title.size = 0.4, legend.position = c("left", "bottom"),
    main.title.fontfamily = "Lobster Two"
  ) +
  tm_credits(text = "@schochastics", fontfamily = "Lobster Two")

map1
tmap_save(map1, filename = "17_land1.png")
