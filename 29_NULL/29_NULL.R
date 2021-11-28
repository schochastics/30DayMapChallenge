library(tidyverse)
library(sf)
library(igraph)
library(ggraph)
library(ggimage)

map <- rnaturalearth::ne_countries(country="germany",scale = 50,returnclass = "sf")
stats_tbl <- read_csv("ger00_top10.csv")
teams_tbl <- read_csv("team_dict.csv")

g <- graph_from_data_frame(stats_tbl,FALSE,teams_tbl)
E(g)$zz_norm <- ambient::normalise(E(g)$zz,to=c(5,10))
p <- ggraph(g,"manual",x=V(g)$long,y=V(g)$lat)+
  ggfx::with_outer_glow(geom_sf(data=map,fill="#006400"),colour = "white",expand = 5)+
  geom_edge_arc(aes(width=I(zz_norm),label_size=I(zz_norm),label=zz), edge_colour="white",strength=0.1,
                 label_colour = "black",
                 # label_size = 12,
                 family="Open Sans",
                 # fontface="bold",
                 angle_calc = 'along',
                 show.legend = FALSE)+
  geom_image(data=teams_tbl,aes(x=long,y=lat,image=logo),size=0.05)+
  theme_void()+
  theme(plot.title = ggtext::element_markdown(size=34,colour="white",family="URW Gothic"),
        plot.caption = element_text(size=20,colour="white",family="URW Gothic"),
        plot.subtitle = element_text(size=32,colour="white",family="URW Gothic",face = "bold"),
        plot.background = element_rect(fill="black"))+
  labs(caption="@schochastics",
       subtitle = "all games played between 1963 and 2021",
       title="Fixtures with the most **0:0** as result in the German Bundesliga")+
  coord_sf(clip = "off")

ragg::agg_png("29_NULL.png",height=1500,width=1100)
p
dev.off()

cmd <- paste0('convert ','"',"29_NULL.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)

