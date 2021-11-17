library(tidyverse)
library(igraph)
library(ggraph)
library(edgebundle)
library(sf)

color_scheme <- c("North America" = "#E12D7B",
                  "South America" = "#F67B52",
                  "Africa" = "#EDCD3B", 
                  "Europe" = "#3BBC54", 
                  "Asia" = "#2665BD",
                  "Oceania" = "#481899")
world <- rnaturalearth::ne_countries(scale = "small", returnclass = "sf")
migr_tbl <- read_csv("data/migration2005_2010.csv")
dict <- read_csv("data/dict.csv")
centers <- read_csv("data/country_centroids_az8.csv") %>% 
  select(iso_a3,the_geom)

dict <- dict %>% left_join(centers,by="iso_a3") %>% 
  mutate(x=str_extract(the_geom,"\\([\\-0-9.]+") %>% str_remove("\\(") %>% as.numeric(),
         y=str_extract(the_geom,"[\\-0-9.]+\\)") %>% str_remove("\\)") %>% as.numeric()) %>% 
  select(-the_geom) %>% 
  dplyr::filter(!is.na(x))

edges <- migr_tbl %>% 
  semi_join(dict,by=c("to"="country")) %>% 
  semi_join(dict,by=c("from"="country")) %>% 
  dplyr::filter(count>5) %>% 
  rename(weight=3)

g <- graph_from_data_frame(edges,TRUE,dict)
E(g)$origin <- V(g)$continent[get.edgelist(g,FALSE)[,1]]
# ggraph(g,"manual",x=V(g)$x,y=V(g)$y)+
#   geom_edge_link0(aes(color=origin),edge_width = 0.1,edge_alpha=0.2)

xy <- cbind(V(g)$x,V(g)$y)
pbundle <- edge_bundle_path(g,xy,max_distortion = 20,weight_fac = 2,segments = 50)

pbundle_sf <- pbundle %>% 
  st_as_sf(coords = c("x", "y"), agr = "constant") %>%
  group_by(group) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING") %>% 
  mutate(weight=E(g)$weight,origin=E(g)$origin)

st_crs(pbundle_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

p <- ggplot() + 
  geom_sf(data=world,fill="grey25",col="white",size=0.1)+
  # geom_sf(data=pbundle_sf,col="#9d0191",size=0.05,aes(alpha=weight),show.legend = FALSE)+
  # geom_sf(data=pbundle_sf,col="white",size=0.005,aes(alpha=weight),show.legend = FALSE)+
  geom_sf(data=pbundle_sf,aes(col=origin,alpha=weight),size=0.1)+
  scale_color_manual(values=color_scheme,na.value = "grey66",name="")+
  scale_alpha(range=c(0.5,1),guide="none")+
  theme_minimal()+
  theme(panel.background = element_rect(color = NA, fill = "grey99"),
        panel.grid.major = element_line(color = "#b3bdbf", size = 0.5),
        plot.background = element_rect(color = NA, fill = "grey99"),
        legend.position = "top",
        legend.text = element_text(family = "Oxygen", margin = margin(r=30),
                                   color = "black",
                                   size = 24),
        legend.key.width = unit(12, 'lines'), 
        # legend.key.size= unit(5, 'lines'),
        plot.title = element_text(family = "Lobster Two", 
                                  color = "black",
                                  size = 84, 
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 7)),
        plot.subtitle = element_text(family = "Oxygen", 
                                     color = "black",
                                     size = 54, 
                                     face = "plain",
                                     hjust = 0.5),
        plot.caption = element_text(family = "Oxygen", 
                                    color = "black", 
                                    size = 28))+
  coord_sf(crs = "+proj=robin",clip = "off")+
  labs(title = "Global Migration (2005-2010)",
       subtitle = "colored by continent of origin",
       caption = "@schochastics")+
  annotate("text",y=-Inf,x=0,label='(Data from Abel and Sander "Quantifying Global International Migration Flows")',
           color="grey66",hjust=0.5,size=8,vjust=0)+
  guides(colour = guide_legend(label.position = "top",
                               override.aes = list(size=6),
                               nrow = 1))
p
ragg::agg_png("02_lines.png",width = 2*1500,height = 2*800,units = "px")
p
dev.off()
cmd <- paste0('convert ','"',"02_lines.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)
