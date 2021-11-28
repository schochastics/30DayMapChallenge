library(sf)

world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")

world <- st_cast(world,"POLYGON")
world$fill <-  c("#F9D8D6", "#CDF5F6", "#F9EBDF", "#EFD2AC", "#f9d6f7", "#d6f9d8", "#f7f9d6")[world$mapcolor7]
world$fillweight <- 1.5
graticules_old <- rnaturalearth::ne_download(scale="small",type="graticules_30",category = "physical", returnclass = "sf")
graticules_old$color <- "#E5E5E5"


k <- 0
for(x in -180:180){
  cat(x,"\n")
  k <- k+1
  fout <- paste0("out/rotation",k,".png")
  proj <- paste0("+proj=ortho +lat_0=20 +lon_0=",x," +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs")
  graticules <- st_transform(graticules_old,proj)
  graticules <- graticules[as.numeric(st_length(graticules))!=0,]
  world1 <- st_transform(world,crs = proj)
  world1 <- world1[as.numeric(st_area(world1))!=0,]
  pst <- roughsf::roughsf(list(graticules,world1),width = 700,height = 700)
  roughsf::save_roughsf(pst,fout,wait = 0.5)
}

av::av_encode_video(glue::glue("out/rotation{1:356}.png"), 
                    output = "rotation.mp4", framerate = 60)
