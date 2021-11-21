# https://www.opendem.info/opendem_client.html
library(tidyverse)
library(ggfx)
library(sf)
library(rmapshaper)

cols <- c("#F55C47", "#9FE6A0", "#4AA96C", "#564A4A")
# round_any  <-  function(x, accuracy, f=round){f(x/ accuracy) * accuracy}
data <- sf::st_read("N28W018/N28W018.shp")
isl <- data[data$elevation%in%c(0,500,1000,1500,2500),]
isl <- st_cast(isl,"POLYGON")
isl <- ms_simplify(isl,keep = 0.01)
p <- ggplot()+
  with_outer_glow(geom_sf(data=isl[isl$elevation==500,],fill="#4AA96C"),expand = 4)+
  with_outer_glow(geom_sf(data=isl[isl$elevation==1000,],fill="#9FE6A0"),expand = 4)+
  with_outer_glow(geom_sf(data=isl[isl$elevation==1500,],fill="#F55C47"),expand = 4)+
  scale_fill_manual(values=rev(cols)[-1])+
  coord_sf(xlim = c(-18.0,-17.7),ylim=c(28.48,28.85))+
  theme_void()+
  labs(caption="@schochastics",title="Stylized Elevation of La Palma")+
  theme(plot.background = element_rect(fill="#006994"),
        plot.title = element_text(hjust=0.5,colour="white",size=44,family="EB Garamond 08",face="italic"),
        plot.caption = element_text(colour="white",size=22,family="EB Garamond 08",face="italic"))

p
ragg::agg_png("21_elevation.png",width=666.66,height=1000)
p
dev.off()

cmd <- paste0('convert ','"',"21_elevation.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)
