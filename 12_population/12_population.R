library(edgebundle)
library(igraph)
library(tidyverse)
library(sf)

us <- rnaturalearth::ne_states(country="United States of America",returnclass = "sf")
us <- us[!us$name%in%c("Alaska","Hawaii"),]

ggplot(us)+geom_sf()+geom_point(aes(longitude,latitude))

coords <- tibble(name=us$name,x=us$longitude,y=us$latitude)
migration1519 <- us_migration %>% 
  dplyr::filter(year>=2015,!from%in%c("Alaska","Hawaii"),!to%in%c("Alaska","Hawaii")) %>% 
  group_by(from,to) %>% 
  dplyr::summarise(weight=sum(weight),.groups = "drop")

g <- graph_from_data_frame(migration1519,TRUE,coords)
xy <- cbind(V(g)$x,V(g)$y)

pbundle <- edgebundle::edge_bundle_path(g,xy,max_distortion = 12)
pbundle_sf <- pbundle %>% 
  st_as_sf(coords = c("x", "y"), agr = "constant") %>%
  group_by(group) %>%
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")
pbundle_sf$weight <- E(g)$weight
st_crs(pbundle_sf) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

el <- get.edgelist(g,FALSE)
pbundle_sf$dir <- atan2(xy[el[,1],2]-xy[el[,2],2],xy[el[,1],1]-xy[el[,2],1])
bbox_us <- st_bbox(us)
bbox_us["ymin"]-bbox_us["ymax"]
pbundle_sf$dir_ew <- ambient::normalise(xy[el[,1],1]-xy[el[,2],1],from=c(-57.8431,57.8431),to=c(-1,1))
pbundle_sf$dir_ns <- ambient::normalise(xy[el[,1],2]-xy[el[,2],2],from=c(-24.8269,24.8269),to=c(-1,1))

pbundle_sf$dir_ew_d <- case_when(pbundle_sf$dir_ew< -0.33~"we",
                                 pbundle_sf$dir_ew>0.33~"ew",TRUE~"ns")
# neg:w->e s->n pos:e->w n->s
ggplot()+
  geom_sf(data=us,col="white",fill="grey25")+
  geom_sf(data = pbundle_sf,aes(size=weight,col=dir_ew_d),alpha=0.5,show.legend = TRUE)+
  # scale_color_gradient2()+
  # scale_color_viridis_c(option = "A")+
  scale_size(range = c(0.1,2))+
  geom_point(data=us,aes(longitude,latitude))

us1 <- us
us1 <- st_cast(us1,"POLYGON")
flow_tbl <- tibble(name=V(g)$name,inflow=graph.strength(g,mode="in"),outflow=graph.strength(g,mode="out"),netflow=(inflow+1)/(outflow+1))
us1 <- left_join(us1,flow_tbl,by = "name")
us1$fillweight <- ambient::normalise(us1$inflow,to=c(0.5,2))
us1$hachureangle <- ambient::normalise(us1$netflow,to=c(45,-45))

pbundle_sf1 <- pbundle_sf
pbundle_sf1$color <- case_when(pbundle_sf$dir_ew< -0.1~"#832424",
                                 pbundle_sf$dir_ew>0.1~"#3A3A98",TRUE~"#ffffff")
pbundle_sf1$stroke <- ambient::normalise(pbundle_sf1$weight,to=c(0.2,1.5))#1.2

midx <- (bbox_us["xmin"]+bbox_us["xmax"])/2
ypos <- bbox_us["ymax"]+2
mx <- bbox_us["xmin"]

legend <- tibble(color=c("#832424","#3A3A98"),stroke=8)
leg_geom <- st_sfc(st_linestring(matrix(c(midx-11,ypos,midx-6,ypos),ncol=2,byrow = TRUE)),
                 st_linestring(matrix(c(midx+11,ypos,midx+6,ypos),ncol=2,byrow = TRUE)))

legend <- st_as_sf(legend,geometry=leg_geom)

legend_text <- tibble(label=c("from west to east","from east to west","inflow/outflow = min (more outflow)","inflow/outflow = max (more inflow)"),
                      label_pos=c("e","e","e","e"),
                      size=0.0,color="#ffffff")
leg_geom <- st_sfc(st_point(c(midx-5.2,ypos)),st_point(c(midx+11.8,ypos)),
                   st_point(c(mx+5.5,25.75)),st_point(c(mx+5.5,25.75+3)))
legend_text <- st_as_sf(legend_text,geometry=leg_geom)

legend_box <- tibble(hachureangle=c(45,-45))
rect1 <- matrix(c(mx,24.5,mx+5,24.5,mx+5,27,mx,27,mx,24.5),ncol=2,byrow = TRUE)
rect2 <- rect1
rect2[,2] <- rect2[,2]+3
leg_geom <- st_sfc(st_polygon(list(rect1)),
                   st_polygon(list(rect2)))
legend_box <- st_as_sf(legend_box,geometry=leg_geom)


pst <- roughsf::roughsf(list(legend,legend_text,legend_box,us1,pbundle_sf1),
                 title="Migration Flows in the US (2015-2019)",title_font = "94px Pristina",
                 caption="data from census.gov     drawn by @schochsatics", caption_font = "50px Pristina",
                 font="54px Pristina",
                 roughness = 3,simplification = 0.001,
                 width = 1500*2,height=1000*2)

roughsf::save_roughsf(pst,"12_population.png")
