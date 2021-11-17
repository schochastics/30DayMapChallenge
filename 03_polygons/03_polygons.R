# http://www.statsmapsnpix.com/2016/12/creating-3d-city-model-using-open-data.html
# https://gridreferencefinder.com/
# https://osdatahub.os.uk/downloads/open/OpenMapLocal
library(tidyverse)
library(sf)
library(ggfx)

sj <- st_read("data/opmplc_essh_sj/OS OpenMap Local (ESRI Shape File) SJ/data/SJ_Building.shp")
sd <- st_read("data/opmplc_essh_sd/OS OpenMap Local (ESRI Shape File) SD/data/SD_Building.shp")

sj_crop <- st_crop(sj,xmin = 383893-5500,xmax = 383893+5500,ymin=398077 - 5500,ymax=398077 + 5500)#st_crop(sj,y = box_gm)
sd_crop <- st_crop(sd,xmin = 383893-5500,xmax = 383893+5500,ymin=398077 - 5500,ymax=398077 + 5500)#st_crop(sd,y = box_gm)

gm_crop <- bind_rows(sj_crop,sd_crop)
neon <- c("#C724B1","#4D4DFF","#E0E722","#FFAD00",
          "#D22730","#DB3EB1","#44D62C")
gm_crop$col <- sample(neon,nrow(gm_crop),replace = TRUE)
circle <- data.frame(
  x = 383893 + 5000 * cos(seq(0, 2*pi, length.out = 360)),
  y = 398077 + 5000 * sin(seq(0, 2*pi, length.out = 360))
)

p <- ggplot()+
  as_reference(
    geom_polygon(aes(x = x, y = y), circle),
    id = 'circle'
  ) +
  with_mask(
    geom_sf(data=gm_crop,aes(fill=col),color="grey66",size=0.01),
    mask = ch_alpha('circle')
  )+
  geom_polygon(data = circle,aes(x = x, y = y),col="white",fill=NA,size=1.5)+
  theme_void()+
  theme(
    legend.position = "none",
    plot.title = element_text(color=neon[3],family = "Xanh Mono",size=58,hjust=0.5),
    plot.subtitle = element_text(color=neon[3],family = "Xanh Mono",size=46,hjust=0.5),
    plot.caption = element_text(color="white",family="Xanh Mono",size=28),
    plot.background = element_rect(fill="black"))+
  labs(title = "Manchester",subtitle="(53.479167, -2.2441643)",caption = "@schochastics")
# p
ragg::agg_png("03_polygons.png",width = 1500,height = 1500,units = "px")
p
dev.off()
cmd <- paste0('convert ','"',"03_polygons.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)


# ggplot()+
#     geom_polygon(aes(x = x, y = y), circle)+
#   geom_polygon(data = circle,aes(x = x, y = y),col="white",fill=NA,size=1.5)+
#   theme_void()+
#   theme(
#     legend.position = "none",
#     plot.title = element_text(color=neon[3],family = "Xanh Mono",size=36,hjust=0.5),
#     plot.subtitle = element_text(color=neon[3],family = "Xanh Mono",size=20,hjust=0.5),
#     plot.caption = element_text(color="white",family="Xanh Mono",size=18),
#     plot.background = element_rect(fill="black"))+
#   labs(title = "Manchester",subtitle="(53.479167, -2.2441643)",caption = "@schochastics")
circle <- data.frame(
  x = 383893 + 2500 * cos(seq(0, 2*pi, length.out = 360)),
  y = 398077 + 2500 * sin(seq(0, 2*pi, length.out = 360))
)
circle_poly <- st_sfc(st_polygon(list(as.matrix(circle))))
st_crs(circle_poly) <- st_crs(gm_crop)
gm_circle <- st_crop(gm_crop,circle_poly,)
ggplot()+geom_sf(data=circle_poly)+geom_sf(data=gm_circle)

gm_circle <- st_cast(gm_circle,"POLYGON")
gm_circle$fill <- "black"
gm_circle$fillweight <- 0.1
gm_circle$stroke <- 0.2
gm_circle$hachuregap <- 3
roughsf::roughsf(gm_circle,width=3000,height=3000)
