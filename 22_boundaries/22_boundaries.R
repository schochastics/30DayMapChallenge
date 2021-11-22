# https://www.landatlas.de/daten.html
# https://www.washingtonpost.com/news/worldviews/wp/2014/10/31/the-berlin-wall-fell-25-years-ago-but-germany-is-still-divided/
# https://www.zeit.de/feature/german-unification-a-nation-divided
library(tidyverse)
library(sf)
library(patchwork)

ger <- st_read("germany/georef-germany-kreis-millesime.shp")
ger$krs_code[ger$krs_code=="03159"] <- "03152"

clean_stats <- function(file){
  dat <- readxl::read_xlsx(file) %>% janitor::clean_names()
  dat$krs_code <- str_remove_all(as.character(dat$krs1214),"000$") %>% 
    str_replace_all("2e\\+06","02000") %>% 
    str_replace_all("1\\.1e\\+07","11000") %>% 
    str_pad(5,"left","0")
  dat <- dat %>% select(-ends_with("typ"))
  names(dat)[3] <- "value"
  select(dat,krs_code,value)
}
main_colors <- c("#3182bd","#31a354","#e6550d","#756bb1","black")
bev1 <- clean_stats("Bevoelkerung_Auslaender.xlsx")
# bev2 <- clean_stats("Bevoelkerung_insgesamt.xlsx")
bev3 <- clean_stats("Lebenserwartung_maennlich.xlsx")
bev4 <- clean_stats("Einkuenfte.xlsx")
bev5 <- clean_stats("ALQ_gemittelt.xlsx")

title_size <- 28
label_size <- 16

p1 <- ger %>% left_join(bev1,by="krs_code") %>% 
  ggplot()+
  ggfx::with_outer_glow(geom_sf(aes(fill=value),col="white",size=0.05))+
  scale_fill_gradient(low="white",high=main_colors[1],name="",trans="sqrt",
                      breaks=c(1,32),labels=c("low","high"))+
  theme_void()+
  theme(
    plot.title = element_text(size = title_size, colour = main_colors[1],family="Open Sans",face="bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill="white",color=NA))+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5,ticks = FALSE,
                                frame.colour = "black",title.position = "top",
                                label.theme = element_text(size=label_size,family = "Open Sans")))+
  labs(title="Percentage of Foreigners")

p2 <- ger %>% left_join(bev3,by="krs_code") %>% 
  ggplot()+
  ggfx::with_outer_glow(geom_sf(aes(fill=value),col="white",size=0.05))+
  scale_fill_gradient(low="white",high="black",name="")+
  theme_void()+
  theme(
    plot.title = element_text(size = title_size, colour = "black",family="Open Sans",face="bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill="white",color=NA))+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5,ticks = FALSE,
                                frame.colour = "black",title.position = "top",
                                label.theme = element_text(size=label_size,family = "Open Sans")))+
  labs(title="Life Expectancy (men)")

p3 <- ger %>% left_join(bev4,by="krs_code") %>% 
  ggplot()+
  ggfx::with_outer_glow(geom_sf(aes(fill=value),col="white",size=0.05))+
  scale_fill_gradient(low="white",high=main_colors[3],name="")+
  theme_void()+
  theme(
    plot.title = element_text(size = title_size, colour = main_colors[3],family="Open Sans",face="bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill="white",color=NA))+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5,ticks = FALSE,
                                frame.colour = "black",title.position = "top",
                                label.theme = element_text(size=label_size,family = "Open Sans")))+
  labs(title="Median Income")

p4 <- ger %>% left_join(bev5,by="krs_code") %>% 
  ggplot()+
  ggfx::with_outer_glow(geom_sf(aes(fill=value),col="white",size=0.05))+
  scale_fill_gradient(low="white",high=main_colors[4],name="")+
  theme_void()+
  theme(
    plot.title = element_text(size = title_size, colour = main_colors[4],family="Open Sans",face="bold"),
    legend.position = "bottom",
    plot.background = element_rect(fill="white",color=NA))+
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5,ticks = FALSE,
                                frame.colour = "black",title.position = "top",
                                label.theme = element_text(size=label_size,family = "Open Sans")))+
  labs(title="Unemployment Rate")


ragg::agg_png("22_boundaries.png",width=1500,height=1500)
p1+p2+p3+p4+plot_layout(ncol = 2,nrow = 2,)+
  plot_annotation(
    title="(In)visible boundaries between East and West Germany",
    caption="@schochastics",
    theme = theme(plot.caption = element_text(size=18,family="Open Sans"),
                  plot.title = element_text(size=40,family="Open Sans",face = "bold",hjust = 0.5,
                                            margin = margin(10,0,32,0)))
  )
dev.off()
