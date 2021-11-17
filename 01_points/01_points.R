library(tidyverse)
library(sf)
stadiums_tbl <- read_csv("stadiums.csv")
stadiums_sf <- stadiums_tbl %>%
  dplyr::filter(!is.na(lon) & (lon!=0 & lat!=0)) %>% 
  st_as_sf(coords = c("lon", "lat"))

st_crs(stadiums_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

stadiums_sf <- stadiums_sf %>% 
  st_transform(crs = "+proj=moll")


n <- format(nrow(stadiums_tbl),big.mark = ",")
map_title <- paste0(n," Football Grounds around the World")
map_subtitle <- "(Data harmonized from various sources)"
map_caption <- "@schochastics"

p <- ggplot(stadiums_sf)+
  ggfx::with_outer_glow(geom_sf(col = "white",size = 0.6,alpha=0.5),colour = "#ffd700")+
  # ggfx::with_outer_glow(geom_sf(col = "white",aes(size=capacity),alpha=0.5),colour = "#ffd700")+
  # scale_size(range=c(0.05,0.3))+
  annotate(geom = "text", 
           x = -12500000, y = -4350000, 
           label = map_title, 
           family = "Premier 2019",
           color = "#ffd700",
           size = 18,
           hjust = 0.5) +
  annotate(geom = "text", 
           x = -12500000, y = -4900000, 
           label = map_subtitle, 
           family = "Premier 2019",
           color = "#fff09d",
           size = 14,
           hjust = 0.5) +
  theme_void()+
  theme(plot.background = element_rect(fill="black"),
        plot.caption =  element_text(colour="#ffd700",family = "Premier 2019",size=20))+
  labs(caption = map_caption)
# p
ragg::agg_png("01_points.png",width = 1500*2,height = 800*2,units = "px")
p
dev.off()

cmd <- paste0('convert ','"',"01_points.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)

