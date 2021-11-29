library(sf)
library(tidyverse)
sf_use_s2(FALSE)
proj <-  "+proj=sinu"

meta <- read_csv("data/meta_offices.csv")
meta$query <- paste(meta$city,meta$state,meta$country,sep=",")
locs <- tmaptools::geocode_OSM(meta$query,as.sf = TRUE)
hk <- tmaptools::geocode_OSM("Hong Kong",as.sf = TRUE)
locs1 <- bind_rows(locs[1:59,],hk,locs[60:91,])

world <- rnaturalearth::ne_countries(scale = "medium", type="countries", returnclass = "sf")
bb <- st_bbox(world)
bb_sf <- st_as_sf(st_make_grid(bb,n = 50))
p <- ggplot()+
  ggfx::with_outer_glow(geom_sf(data=bb_sf,col="#4267B2",fill="#4267B2"),colour="white")+
  geom_sf(data=world,size=0.1,col="black")+
  geom_sf(data=locs1,size=3,col="#cd2626")+
  coord_sf(crs = proj)+
  theme_void()+
  theme(panel.background = element_rect(fill="#898F9C",color=NA),
        plot.background = element_rect(fill="#898F9C",color=NA),
        plot.title = element_text(hjust=0.5,size=84,family="Xanh Mono",margin=margin(t=24)),
        plot.caption = element_text(hjust=0.5,size=44,family="Xanh Mono",margin=margin(b=24)))+
  labs(title= "Location of Meta Platforms, Inc. Offices",caption="@schochastics")

ragg::agg_png("30_meta.png", width = 2 * 1500, height = 2 * 1000, units = "px")
p
dev.off()

cmd <- paste0("convert ", '"', "30_meta.png", '"', ' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)
