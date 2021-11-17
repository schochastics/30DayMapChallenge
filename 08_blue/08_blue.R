# https://gramener.com/flags/
library(tidyverse)
library(ggtext)
cols <- c(Red = "#b40000", Saffron = "#ff9600", Yellow = "#ffc800", Green = "#008000", 
          Blue = "#000080", LiBlue = "#0080ff", Black = "#000000", White = "#ffffff"
)
world <- rnaturalearth::ne_countries(scale = "medium", type="countries", returnclass = "sf")
flag_data <- read_csv("flag_data.csv")

world <- left_join(world,flag_data,by=c("iso_a3"="ID"))
world$Blue <- world$Blue+world$LiBlue
world$Blue <- coalesce(world$Blue,0)

p <- ggplot(world)+
  geom_sf(fill="white",col="black",size=0.1)+
  geom_sf(aes(fill=(Blue!=0),alpha=Blue),col="black",size=0.1)+
  coord_sf(crs = "+proj=robin",clip = "off")+
  scale_fill_manual(values=c("grey66","#000080"),guide="none")+
  scale_alpha_continuous(name="",breaks=c(0,20,40,60,80,100),limits=c(0,100))+
  theme_void()+
  labs(title = "Countries with the color <span style = 'color:#000080;'>**blue**</span> in their flag",
       subtitle = "alpha value indicates the percentage of blue in the flag",
       caption = "@schochastics")+
  theme(panel.grid.major = element_line(color="grey66"),
        plot.title = element_textbox_simple(halign = 0.5,size = 84,family = "Lobster Two"),
        plot.subtitle = element_text(family = "Oxygen", 
                                     color = "black",
                                     size = 42, 
                                     face = "plain",
                                     hjust = 0.5),
        plot.caption = element_text(family = "Oxygen", 
                                    color = "black", 
                                    size = 28),
        legend.position = "top",
        legend.text = element_text(family = "Oxygen", margin = margin(r=30),
                                   color = "black",
                                   size = 24),
        legend.key.width = unit(12, 'lines'))+
  annotate("text",y=-Inf,x=0,label='(Data from https://gramener.com/flags/)',
           color="grey66",hjust=0.5,size=10,vjust=0)+
  guides(alpha = guide_legend(label.position = "top",
                              override.aes = list(size=1,fill="#000080"),
                              nrow = 1))
# p
ragg::agg_png("08_blue.png",width = 2*1500,height = 2*800,units = "px")
p
dev.off()
cmd <- paste0('convert ','"',"08_blue.png",'"',' -set filename:base "%[base]" -trim +repage "%[filename:base].png"')
system(cmd)

