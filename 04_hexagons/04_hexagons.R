# https://www.worldpop.org/geodata/summary?id=41389
# https://cartogrid.vercel.app/
library(tidyverse)
library(sf)
pop_tbl <- read_csv("data/deu_pd_2020_1km_ASCII_XYZ.csv")
pop_sf <- pop_tbl %>% 
  st_as_sf(coords = c("X", "Y"), agr = "constant") 
st_crs(pop_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

de <- rgdal::readOGR("data/Germany.geojson", "Germany")
de_df <- fortify(de)
de_sf <- de_df %>% 
  st_as_sf(coords = c("long", "lat"), agr = "constant") %>%
  group_by(id) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("POLYGON")
st_crs(de_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


tmp <- st_intersects(de_sf,pop_sf)
hex_dens <- sapply(tmp, function(x){mean(pop_tbl$Z[x],na.rm=TRUE)})
de_sf$pop_dens <- hex_dens

p <- ggplot()+
  geom_sf(data = de_sf,aes(fill=pop_dens),color="white",size=0.1)+
  scale_fill_distiller(palette=3,direction = 1,trans="sqrt",name="",
                       breaks=range(de_sf$pop_dens),
                       labels=c("low","high"))+
  theme_void()+
  theme(legend.position="top",
        plot.title = element_text(color="#8856a7",hjust=0.5, size=64, margin = margin(t=10,b=28),
                                  family="Lobster Two",face="bold"),
        plot.subtitle = element_text(color="#8856a7",hjust=0.5,size=56,family="Open Sans",
                                     margin = margin(b=10)),
        plot.caption = element_text(color="#8856a7",size=38,family="Open Sans",margin = margin(b=10)),
        plot.background = element_rect(fill="grey85",size=5))+
  guides(fill = guide_colourbar(barwidth = 35, barheight = 2.0,ticks = FALSE,
                                frame.colour = "white",title.position = "top",
                                label.theme = element_text(size=34,colour="#8856a7",family = "Open Sans")))+
  labs(title = "Population Density of Germany",
       subtitle = "aggregated on 1km x 1km hexagon tiles",
       caption = "@schochastics  ")+
  annotate("text",x=10.5,y=47.0,label="population data from worldpop.org,\nhex grid created with cartogrid.vercel.app",
           family="Open Sans",color="grey50",size=10)

ragg::agg_png("04_hexagons.png",width = 800*2,height = 1500*2,units = "px")
p
dev.off()

cmd <- paste0('convert ','"',"04_hexagons.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)

de_sf$color <- "white"
de_sf$fillstyle <- "dots"
de_sf$fill <- apply(
  colorRamp(c("#E65F50","#FF8D5C","#FDC85E","#FDEA8B","#7EA157"))(ambient::normalise(de_sf$pop_dens,to=c(1,0))),1,
  function(x) rgb(x[1],x[2],x[3],maxColorValue = 255))
de_sf$stroke <- 0.01
de_sf$hachuregap <- ambient::normalise(de_sf$pop_dens,to=c(6,1))
pst <- roughsf::roughsf(de_sf,height=1500,width = 800,title="Population Density of Germany",
                 title_font = "54px Pristina",
                 caption = "drawn by @schochastics",caption_font = "32px Pristina")

roughsf::save_roughsf(pst,"~/Documents/R/roughsf/man/figures/pop_dens_ger.png")
