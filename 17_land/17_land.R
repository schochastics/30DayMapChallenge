library(raster)
library(tmap)
# +proj=urm5 +n=0.9 +alpha=2 +q=4
forest <- raster("FOR_2000.asc")
crs(forest) <- "+proj=longlat" 
# relief <- raster("GRAY_LR_SR_W.tif")
# names(forest) <- "Percentage"
# raster::projectRaster
map1 <- tm_shape(forest,projection = "+proj=robin",raster.warp = FALSE) +
  tm_raster(n = 5,palette = c("white","#cbe368","#719b25","#2e6409","#2c4928"),breaks = c(1,20,40,60,80,100),
            legend.show = TRUE)+
  tm_layout(sepia.intensity	=0,saturation = 1,frame = TRUE,
            main.title = "Share of Forest Land",title = "(data from https://webarchive.iiasa.ac.at/)", 
            title.position = c("right","top"),title.size = 0.4,legend.position = c("left","bottom"),
            main.title.fontfamily = "Lobster Two")+
            # bg.color = "#84240c",frame.double.line = TRUE,frame.lwd = 2
  tm_credits(text = "@schochastics",fontfamily = "Lobster Two")
map1
tmap_save(map1,filename = "17_land1.png")

map1+
  tm_shape(relief,projection = "+proj=robin",raster.warp = FALSE)+
  tm_raster(legend.show = FALSE)

tm_shape(relief)+
  tm_raster(palette = "Greys",legend.show = FALSE)+
tm_shape(forest)+
  tm_raster(n = 5,palette = c(NA,"#cbe368","#719b25","#2e6409","#2c4928"),breaks = c(1,20,40,60,80,100),
            legend.show = FALSE)
