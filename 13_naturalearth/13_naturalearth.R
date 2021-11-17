library(tidyverse)
library(rnaturalearth)
library(sf)
library(ggtext)

dat <- read_csv("data/cargo2019.csv") %>% dplyr::filter(total > 10000)
dat$city <- dat$city %>% str_remove_all(" \\(.*\\)")
europe <- ne_countries(scale = 50, continent = "Europe", returnclass = "sf")

europe$fill <- as.factor(europe$mapcolor7)
dat$size <- ambient::normalise(log(dat$total), to = c(4, 14))
dat$radius <- dat$size / 10

newloc <- packcircles::circleRepelLayout(select(dat, long, lat, radius))
dat1 <- bind_cols(dat, newloc$layout[, -3])


map_title <- "<span style='font-size:32pt;'>Figurative Map of the Cargo Tonnage of the Major Ports of Europe</span>"
p <- ggplot() +
  geom_sf(data = europe, aes(fill = fill), show.legend = FALSE, size = 0.1) +
  geom_sf_text(
    data = europe[europe$pop_est > 8e6 & !europe$name_long %in% c("Netherlands", "Belgium", "Portugal"), ],
    aes(label = name_long), size = 10, family = "EB Garamond 08"
  ) +
  geom_point(data = dat1, aes(x, y, size = I(size)), show.legend = FALSE, shape = 21, fill = "darkgreen") +
  geom_text(data = dat1, aes(x, y, size = I(size / 3), label = total), family = "EB Garamond 08") +
  geom_text(data = dat1, aes(x, y, label = city), size = 2, vjust = -dat$size / 3, family = "EB Garamond 08") +
  scale_fill_manual(values = c("#F9D8D6", "#CDF5F6", "#F9EBDF", "#EFD2AC", "#f9d6f7", "#d6f9d8", "#f7f9d6")) +
  coord_sf(expand = FALSE, xlim = c(-10, 50), ylim = c(35, 75)) +
  # title
  annotate("richtext",
    x = -Inf, y = Inf, hjust = 0, vjust = 1, label = map_title,
    size = 8, fill = "#e8e4c9", family = "EB Garamond 08",
    label.padding = unit(c(3.25, 1.0, 3.0, 1.0), "lines")
  ) +
  # caption
  annotate("richtext",
    x = Inf, y = -Inf, hjust = 1, vjust = 0, label = "<span style='font-size:22pt'>@schochastics</span>",
    size = 12, fill = NA, family = "EB Garamond 08", label.color = NA,
    label.padding = unit(c(0.2, 0.2, 0.2, 0.2), "lines")
  ) +
  theme_void() +
  theme(
    plot.title = element_text(family = "EB Garamond 08", size = 46, hjust = 0, margin = margin(0, 0, 0, 0)),
    plot.background = element_rect(fill = "#e8e4c9", colour = "black", size = 0.5),
    panel.background = element_rect(fill = "#e8e4c9", colour = "black", size = 0.1)
  )

ragg::agg_png("13_naturalearth.png", width = 1500, height = 1500)
p
dev.off()

europe <- ne_countries(scale = 110, continent = "Europe", returnclass = "sf")
europe$fill <- c("#F9D8D6", "#CDF5F6", "#F9EBDF", "#EFD2AC", "#f9d6f7", "#d6f9d8", "#f7f9d6")[europe$mapcolor7]
europe$fillweight <- 2
europe$label <- ""
keep <- c("Spain", "Germany", "Poland", "Austria", "Hungary", "Romania")
europe <- st_cast(europe, "POLYGON")
europe_crop <- st_crop(europe, st_polygon(list(matrix(c(-10, 32, 50, 32, 50, 70, -10, 70, -10, 32), ncol = 2, byrow = TRUE))))

dat1 <- dat1 %>% st_as_sf(coords = c("long", "lat"))
st_crs(dat1) <- st_crs(europe)
dat1$size <- ambient::normalise(dat1$size, to = c(10, 30))
dat1_crop <- st_crop(dat1, st_polygon(list(matrix(c(-10, 32, 50, 32, 50, 75, -10, 75, -10, 32), ncol = 2, byrow = TRUE)))) %>%
  dplyr::filter(y >= 35)
dat1_crop$color <- "red"

water <- data.frame(name = "water")
water$geometry <- st_sfc(st_polygon(list(matrix(c(-10, 32, 50, 32, 50, 73, -10, 73, -10, 32), ncol = 2, byrow = TRUE))))
water <- st_sf(water)
st_crs(water) <- st_crs(europe)
water$fill <- "#006994"
water$fillstyle <- "cross-hatch"
water$fillweight <- 0.1
water$stroke <- 5

europe_bg <- europe_crop
europe_bg$fill <- "#ffffff"
europe_bg$fillstyle <- "solid"
pst <- roughsf::roughsf(list(water, europe_bg, europe_crop, dat1_crop),
  title = "Ports of Europe", title_font = "94px Pristina",
  caption = "(data from naturalearth) drawn by @schochastics", caption_font = "40px Pristina",
  width = 2500, height = 2500
)
pst
roughsf::save_roughsf(pst, "13_naturalearth.png")
