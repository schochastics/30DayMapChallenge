# https://earthobservatory.nasa.gov/images/3581/topography-of-south-america
library(terra)
library(tmap)
SA <- rast("PIA03388.tif")#NE2_HR_LC_SR_W.tif")#NE2_HR_LC_SR.tif")

map1 <- tm_shape(SA) +
  tm_rgb(r=1,g=2,b=3)+
  tm_layout(sepia.intensity	=1,saturation = 1,frame = TRUE, 
            main.title = "Topology of South America",title = "(data from earthobservatory.nasa.gov)", 
            title.position = c("LEFT","bottom"),title.size = 1.2, title.fontfamily = "VonDutch",
            main.title.fontfamily = "VonDutch", main.title.size = 4,
            bg.color = "#84240c",frame.double.line = TRUE,frame.lwd = 2)+
  tm_credits(text = "@schochastics",fontfamily = "VonDutch",align = "right",size = 1.2)+
  tm_compass(position = c("right","top"),type = "8star")

tmap_save(map1,filename = "10_raster_blank.png")

