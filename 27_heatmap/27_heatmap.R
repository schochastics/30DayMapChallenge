library(tidyverse)
fish_tbl <- read_csv("fishing_hours2020.csv")

p <- ggplot(fish_tbl)+geom_raster(aes(cell_ll_lon,cell_ll_lat,fill=fishing_hours))+
  # scale_fill_viridis_c(trans="pseudo_log",name="")+
  scale_fill_gradientn(colours = viridisLite::turbo(20),trans="pseudo_log",
                       breaks=c(0.01,50000),labels=c("0","69,730"))+
  theme_void()+
  theme(panel.background = element_rect(fill="black"),
        plot.background = element_rect(fill="black"),
        plot.caption = element_text(hjust=0.5,size=44,color="white",family="EB Garamond"),
        plot.title = element_text(hjust=0.5,size=86,color="white",family="EB Garamond",margin = margin(t=33,b=33)),
        plot.subtitle = element_text(hjust=0.5,size=60,color="white",family="EB Garamond",margin = margin(b=33)),
        legend.position = "top"
        )+
  labs(title = "Global Fishing Efforts",
       subtitle = "Fishing hours per grid cell in 2020",
       caption = "data: globalfishingwatch.org · visualization by @schochastics")+
  guides(fill = guide_colourbar(barwidth = 60, barheight = 3,ticks = FALSE,
                                label.theme = element_text(colour="white",size=24,family = "Open Sans"),
                                frame.colour = "white",title.position = "top"))
  
ragg::agg_png("27_heatmap.png",width = 1500*2,height=1000*2)
p
dev.off()

cmd <- paste0('convert ','"',"27_heatmap.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)

