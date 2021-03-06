library(tidyverse)
library(igraph)
library(edgebundle)
library(ggtext)
library(ggraph)
graph_union <- function(gtree){
  if(!all(sapply(gtree,function(x) igraph::is.igraph(x)))){
    stop("glist must be a list of igraph objects")
  }
  vattrs <- unique(c(sapply(gtree,igraph::vertex_attr_names)))
  dflist <- lapply(gtree,igraph::as_data_frame,what="both")
  verts <- lapply(dflist,function(x) x$vertices)
  edges <- lapply(dflist,function(x) x$edges)
  
  dummy_ids <- names(verts)
  
  verts <- lapply(1:length(dummy_ids),function(x){
    df <- verts[[x]]
    df$name <- str_replace_all(df$name,"dummy",paste0(dummy_ids[x],"_dummy"))
    df
  })
  
  edges <- lapply(1:length(dummy_ids),function(x){
    df <- edges[[x]]
    df$from <- str_replace_all(df$from,"dummy",paste0(dummy_ids[x],"_dummy"))
    df$to <- str_replace_all(df$to,"dummy",paste0(dummy_ids[x],"_dummy"))
    df
  })
  verts <- do.call("bind_rows",verts)
  verts <- verts[!duplicated(verts),]
  rownames(verts) <- NULL
  edges <- do.call("bind_rows",edges)
  rownames(edges) <- NULL
  # verts <- verts[,c(4,1,2,3)]
  verts <- verts[,c(which(vattrs=="name"),which(vattrs!="name"))]
  graph_from_data_frame(edges,FALSE,verts)  
}
f <- "gtree_015.RDS"

center_NA <- c(-100.167,45.167)
center_SA <- c(-56.1004,-15.6006)
center_AF <- c(14.100526,1.616742)
center_AS <- c(76.08583,38.48333)
center_AU <- c(132.133,-25.117)

# france <- ne_countries(country = "France", returnclass = "sf")
land_clip <- rnaturalearth::ne_countries(scale = 110, returnclass = "sf")
# land_clip <- sf::st_crop(land,c(xmin=-127.3,ymin=-57.8,xmax=175.7,ymax=75.8))


gtree_lst <- readRDS(f)
gtree <- graph_union(gtree_lst)
el <- get.edgelist(gtree)
idl <- which(!str_detect(el[,1],"dummy|France") | !str_detect(el[,2],"dummy|France"))
E(gtree)$label <- ""
E(gtree)$label[idl] <- scales::label_number_si(accuracy = 0.1)(E(gtree)$flow[idl])
E(gtree)$flow <- ambient::normalise(E(gtree)$flow,to=c(1,6))  
# graph ----
map_title <- "<span style='font-size:32pt'>Map of the quantities of French wine exported<br>from 2000-2018 (Top 50 countries)</span>"
class(gtree) <- c(class(gtree),"steiner_tree")
smooth_df <- edgebundle::tnss_smooth(gtree,bw=15,n=50)

p1 <- ggraph(gtree,"manual",x=V(gtree)$x,y=V(gtree)$y)+
  geom_sf(data=land_clip,fill="#e8e4c9",colour="black",size=0.3)+
  geom_path(data=smooth_df,aes(x,y,size=flow,group=destination),lineend="round",col="#DCB5A7",show.legend = FALSE)+
  geom_node_text(aes(filter=tnss=="leaf" & continent!="Europe",label=name),
                 family = "EB Garamond 08",size=3,repel = TRUE)+
  annotate("richtext",x=center_NA[1],y=center_NA[2],label="*NORTH AMERICA*",size=7,
           fill=NA,colour="black",label.colour=NA,
           lineheight=0.6,vjust=1,family="EB Garamond 08")+
  annotate("richtext",x=center_SA[1],y=center_SA[2],label="*SOUTH AMERICA*",size=7,
           fill=NA,colour="black",label.colour=NA,
           lineheight=0.6,vjust=0,family="EB Garamond 08")+
  annotate("richtext",x=center_AF[1],y=center_AF[2],label="*AFRICA*",size=7,hjust=0,
           fill=NA,colour="black",label.colour=NA,
           lineheight=0.6,vjust=0,family="EB Garamond 08")+
  annotate("richtext",x=center_AS[1],y=center_AS[2],label="*ASIA*",size=7,hjust=0,
           fill=NA,colour="black",label.colour=NA,
           lineheight=0.6,vjust=0,family="EB Garamond 08")+
  annotate("richtext",x=center_AU[1],y=center_AU[2],label="*AUSTRALIA*",size=7,hjust=0.5,
           fill=NA,colour="black",label.colour=NA,
           lineheight=0.6,vjust=0.5,family="EB Garamond 08")+
  coord_sf(expand=FALSE)+
  
  #title
  annotate("richtext",x=-Inf,y=Inf,hjust=0,vjust=1,label=map_title,
           size=8,fill="#f2eecb",family="EB Garamond 08",
           label.padding = unit(c(3.25, 1.0, 3.0, 1.0), "lines"))+
  #caption
  annotate("richtext",x=Inf,y=-Inf,hjust=1,vjust=0,label="<span style='font-size:14pt'>data from oec.world ··· drawn by *@schochastics* </span>",
           size=14,fill=NA,family="EB Garamond 08",label.color=NA,
           label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines"))+
  theme_void()+
  theme(plot.background = element_rect(fill="#f2eecb",colour="black",size=0.5),
        panel.background = element_rect(fill="#f2eecb",colour="black",size=0.1))
p1

# inset ----

wine <- vroom::vroom("wine.csv")

gbr_import_total  <- wine %>% 
  mutate(export_val=ifelse(export_val=="NULL","0",export_val)) %>% 
  mutate(export_val=as.numeric(export_val)) %>% 
  dplyr::filter(dest=="gbr") %>% 
  group_by(year) %>% 
  dplyr::summarise(total=sum(export_val,na.rm = TRUE),.groups="drop") %>% 
  dplyr::filter(year>=2000,year<=2016)

fra_export <- wine %>% 
  mutate(export_val=ifelse(export_val=="NULL","0",export_val)) %>% 
  mutate(export_val=as.numeric(export_val)) %>% 
  dplyr::filter(origin=="fra" & dest!="wld") %>% 
  group_by(year) %>% 
  dplyr::summarise(total=sum(export_val,na.rm = TRUE),.groups="drop") %>% 
  dplyr::filter(year>=1980,year<=2016)

fra_export_to_gbr <- wine %>% 
  mutate(export_val=ifelse(export_val=="NULL","0",export_val)) %>% 
  mutate(export_val=as.numeric(export_val)) %>% 
  dplyr::filter(origin=="fra" & dest=="gbr") %>% 
  group_by(year) %>% 
  dplyr::summarise(total=sum(export_val,na.rm = TRUE),.groups="drop") %>% 
  dplyr::filter(year>=2007,year<=2016)

xlab_year <- as.character(1980:2016)
xlab_year <- ifelse(str_sub(xlab_year,-1)=="0",xlab_year,str_sub(xlab_year,3,4))

a <- 1-(6e9-fra_export$total[fra_export$year==2003])/(fra_export$total[fra_export$year==2002]-fra_export$total[fra_export$year==2003])
b <- 1-(8e9-fra_export$total[fra_export$year==2007])/(fra_export$total[fra_export$year==2006]-fra_export$total[fra_export$year==2007])
c <- 1-(10e9-fra_export$total[fra_export$year==2008])/(fra_export$total[fra_export$year==2007]-fra_export$total[fra_export$year==2008])

p2 <- ggplot(fra_export,aes(x=year,y=total))+
  geom_line(col="#DCB5A7",size=1.1)+
  geom_line(data=fra_export_to_gbr,aes(x=year,y=total),col="#DCB5A7")+
  geom_line(data=gbr_import_total,aes(x=year,y=total),col="black")+
  annotate("text",x=1990,y=4.9e9,label="total export of wine from France",hjust=0.5,angle=22,family = "EB Garamond 08",size=4)+
  annotate("text",x=2008.5,y=5.15e9,label="total import of wine in the UK",hjust=0.5,angle=0,family = "EB Garamond 08",size=3)+
  annotate("text",x=2001.5,y=2.3e9,label="total export of wine from France to the UK",hjust=0,angle=0,family = "EB Garamond 08",size=2)+
  annotate("segment",x=2002+a,y=6e9,xend=2016,yend=6e9,size=0.2)+
  annotate("segment",x=2006+b,y=8e9,xend=2016,yend=8e9,size=0.2)+
  annotate("segment",x=2011,y=10e9,xend=2016,yend=10e9,size=0.2)+
  annotate("segment",x=2000,xend=2016,y=1e9,yend=1e9,size=0.2)+
  annotate("segment",x=2000,xend=2016,y=2e9,yend=2e9,size=0.2)+
  annotate("segment",x=2000,xend=2016,y=3e9,yend=3e9,size=0.2)+
  annotate("segment",x=2000,xend=2016,y=4e9,yend=4e9,size=0.2)+
  geom_segment(aes(xend=year),yend=0,size=0.2)+
  scale_y_continuous(labels = scales::label_number_si(accuracy = 0.1),
                     breaks = c(2e9,4e9),
                     limits = c(0,11e9),expand = c(0,0),
                     sec.axis = sec_axis(~.,breaks = c(0,1e9,2e9,3e9,4e9,6e9,8e9,10e9,12e9,14e9),
                                         labels = scales::label_number_si(accuracy = 0.1)))+
  scale_x_continuous(breaks=1980:2016,labels = xlab_year,expand = c(0,0))+
  theme(
    axis.text = element_text(family = "EB Garamond 08"),
    panel.grid = element_blank(),
    panel.background = element_rect(colour=NA,fill="#f2eecb"),
    plot.background = element_rect(fill="#f2eecb",colour="black",size=0.5),
    axis.line.x = element_line(colour="black",size=0.5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(angle=90,hjust=1),
    plot.margin = unit(c(0,0,0,0),"cm")
  )+
  labs(x="",y="")+
  coord_cartesian(clip = "off")

inset_plot <- ggplotGrob(
  p2
)

p <- p1+annotation_custom(
  inset_plot,
  xmin = 90,
  xmax = 180,
  ymin = 42,
  ymax = 80
)

ragg::agg_png("24_historical.png",width=1500,height=1000)
p
dev.off()

cmd <- paste0('convert ','"',"24_historical.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)
