library(tidyverse)
library(sf)
library(patchwork)
games <- vroom::vroom("all.games.csv")
dict <- read_csv("country_continents.csv") %>% select(-SOV_A3,-ISO_A3,-Code)
games2019 <- games %>% dplyr::filter(level=="national",year>2019)


stats_tbl <- games2019 %>% 
  group_by(country) %>% 
  dplyr::summarise(hfa=sum(GH>GA)/n(),gpg=sum(GH+GA)/n(),nogoal=sum(GH==0 & GA==0)/n()) %>% 
  left_join(dict,by="country")

world <- st_read("world_map/ne_50m_admin_0_map_subunits.shp")


world1 <- world %>% 
  dplyr::filter(NAME!="Antarctica") %>% 
  left_join(stats_tbl,by=c("NAME"="name")) %>% 
  left_join(stats_tbl,by=c("SOVEREIGNT"="name")) %>% 
  mutate(hfa=coalesce(hfa.x,hfa.y),gpg=coalesce(gpg.x,gpg.y),nogoal=coalesce(nogoal.x,nogoal.y))

# crs <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +datum=WGS84 +units=m +no_defs"
# crs = "+proj=moll +lon_0=0 +x_0=0 +y_0=0"
label_size <- 28
title_size <- 54
mean_hfa <- sum(games2019$GH>games2019$GA)/nrow(games2019)
p1 <- ggplot(world1)+
  geom_sf(aes(fill=hfa),col="black",size=0.1)+
  scale_fill_gradient2(low="#346751",mid="#f7f7f7",high="#C84B31",midpoint = mean_hfa,name="Percentage of games won by home team")+
  theme_void()+
  theme(plot.title = element_text(hjust=0.5,margin=margin(t=34,b=12),
                                  size = title_size, colour = "white",family="Assyrian",face="bold"),
        plot.subtitle = element_text(hjust=0.5,margin=margin(b=34),size = title_size-8, colour = "white",family="Open Sans"),
        plot.caption = element_text(size = 22, colour = "white",family="Assyrian",face="bold"),
        legend.position = "top",
        panel.grid.major = element_line(color="white",size=0.1),
        plot.background = element_rect(fill="steelblue",color=NA))+
  guides(fill = guide_colourbar(barwidth = 30, barheight = 1,ticks = FALSE,
                                frame.colour = "black",title.position = "top", 
                                title.theme = element_text(colour="white",size=label_size+2,family = "Assyrian"),
                                label.theme = element_text(colour="white",size=label_size,family = "Assyrian")))+
  labs(title="Home Field Advantage in World Football (2019-2021)",
       subtitle="All games played in top tier leagues in 187 countries",
       caption="@schochastics  ")+
  coord_sf(crs="+proj=robin")
p1
ragg::agg_png("26_choropleth.png",width = 1500,height=1000)
p1
dev.off()

cmd <- paste0('convert ','"',"26_choropleth.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)
