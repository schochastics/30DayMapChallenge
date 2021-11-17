library(sf)
library(tidyverse)
library(patchwork)
library(ggfx)
library(ggtext)
monopoly_names <- c("Badstraße","Turmstraße",
                    "Chausseestraße","Elisenstraße","Poststraße",
                    "Seestraße","Hafenstraße","Neue Straße",
                    "Münchner Straße","Wiener Straße","Berliner Straße",
                    "Theaterstraße","Museumstraße","Opernplatz",
                    "Lessingstraße","Schillerstraße","Goethestraße",
                    "Rathausplatz","Hauptstraße","Bahnhofstraße",
                    "Parkstraße","Schlossallee")
monopoly_cols <- c("#82479A","#82479A",
                   "#80CCFD","#80CCFD","#80CCFD",
                   "#CE68CD","#CE68CD","#CE68CD",
                   "#F17E24","#F17E24","#F17E24",
                   "#ED4E20","#ED4E20","#ED4E20",
                   "#FBEF31","#FBEF31","#FBEF31",
                   "#3E8133","#3E8133","#3E8133",
                   "#3153CC","#3153CC")

monopoly_group <- c(1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6,7,7,7,8,8)
monopoly_x <- c(9,7,4,2,1,0,0,0,0,0,0, 1, 3, 4, 6, 7, 9,10,10,10,10,10)
monopoly_y <- c(0,0,0,0,0,1,3,4,6,8,9,10,10,10,10,10,10, 9, 8, 6, 3, 1)


de1 <- readRDS("monopoly_streets.RDS")
de1$NAME <- factor(de1$NAME,levels = monopoly_names)
de_shp <- rnaturalearth::ne_countries(scale=50,country="Germany",returnclass = "sf")

monopoly_tbl <- tibble(name=factor(monopoly_names,levels = monopoly_names),
                       color=monopoly_cols,group=monopoly_group,
                       x=monopoly_x,y=monopoly_y,n=as.numeric(table(de1$NAME))) %>% 
  mutate(alt_name = str_replace_all(name,"ß","ss") %>% str_replace_all("ü","ue")) %>% 
  mutate(circ_number = readLines("circ_nums.txt"))

pstat <- ggplot(monopoly_tbl,aes(x,y))+
  geom_tile(aes(fill=name),col="black")+
  geom_text(aes(label=n),family="Kabel",vjust=1.4,size=7)+
  geom_text(aes(label=circ_number),vjust=0,size=10)+
  annotate("rect",xmin=-0.5,xmax=10.5,ymin=-0.5,ymax=10.5,fill=NA,
           color="black",size=1.5)+
  scale_fill_manual(values=monopoly_cols,name="",
                    labels=paste0(str_pad(monopoly_tbl$circ_number,3,"left"," "),
                                  " ",monopoly_tbl$alt_name))+
  scale_x_continuous(expand = c(0.001,0.001))+
  scale_y_continuous(expand = c(0.001,0.001))+
  labs(caption="@schochastics")+
  coord_fixed()+
  theme_void()+
  theme(panel.background = element_rect(fill="#CBF5CE",color=NA),
        plot.background = element_rect(fill="#CBF5CE",color=NA),
        plot.caption = element_text(family="Kabel",size=10),
        legend.text = element_text(size=14,family="Kabel"),
        legend.background = element_rect(fill=NA,colour="black"))+
  annotate("text",x=5,y=5,hjust=0.5,vjust=0.5,label="Occurrences of\nMonopoly Street Names\nin Germany",family="Kabel",angle=-45,size=12)+
  annotate("text",x=0.5,y=0.5,hjust=0,vjust=0,label="  Data:OpenStreetMap Contributors",family="Kabel",size=4,color="grey66")+
  guides(fill=guide_legend(ncol=1))

ragg::agg_png("05_supplement.png",width=800,height=800,units = "px")
pstat
dev.off()

cmd <- paste0('convert ','"',"05_supplement.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)


de1$monopoly_group <- monopoly_tbl$group[match(de1$NAME,monopoly_tbl$name)]

de_grp <- de1 %>% 
  group_split(by=NAME)

monopoly_size <- case_when(monopoly_tbl$n<100~5,
                           monopoly_tbl$n<1000~3,
                           TRUE~2)

pList <- map(1:length(de_grp),function(x){
  map_title <- str_replace(paste0(unique(de_grp[[x]]$NAME),collapse = ", "),"ß","ss") %>% 
    str_replace("ü","ue")
  ggplot()+
    geom_sf(data=de_shp,size=0.7,fill="grey25",color="white")+
    ggfx::with_outer_glow(geom_sf(data=de_grp[[x]],col=monopoly_cols[x],size=monopoly_size[x],
                                  show.legend = FALSE),colour = "white")+
    theme_void()+
    theme(plot.title = element_textbox_simple(
      halign = 0.5,family="Kabel",size=40,
      color="black",fill = monopoly_cols[x],padding = margin(15,15,15,15)))+
    labs(title=map_title)
})
pList[[1]]
p <- Reduce("+",pList)
layout <- '
ABCDEF
GHIJKL
MNOPQR
#STUV#
'

ragg::agg_png("05_osm.png",width = 1500*2,height = 1500*2,units = "px")
p+plot_layout(design = layout)+
  plot_annotation(
    title = "Monopoly Street Names in Germany",
    caption = "@schochastics",
    theme = theme(plot.title = element_textbox_simple(
      family = "Kabel",size=84,color="white",halign = 0.5,
      fill="#E44B1E",
      margin = margin(0,0,20,0),padding = margin(30,30,30,30)
      ),
      plot.caption = element_text(size=34,family="Kabel",color="white"),
      plot.background = element_rect(fill="#CBF5CE")))
dev.off()

cmd <- paste0('convert ','"',"05_osm.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)
