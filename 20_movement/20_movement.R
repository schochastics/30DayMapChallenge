library(tidyverse)
library(lubridate)
library(sf)
library(rdeck)

ukgrid <- "+init=epsg:27700"
latlong <- "+init=epsg:4326"

raw_df <- data.table::fread("../../train_uk/trains.csv") %>% as_tibble() %>% 
  mutate(V8=ymd_hms(V8))

raw_sf <- raw_df %>% st_as_sf(coords = c("V4", "V5"))
st_crs(raw_sf) <- ukgrid
train_sf <- st_transform(raw_sf,latlong)

tmax <- as.numeric(max(raw_sf$V8))
tmin <- as.numeric(min(raw_sf$V8))

train_sf$time <- ambient::normalise(as.numeric(train_sf$V8),from = c(tmin,tmax),to = c(0,1))

train_sf_lstr <- train_sf %>%
  select(V1,V3,time,geometry) %>% 
  group_by(V1) %>% 
  dplyr::summarise(npts=n(),carrier=sample(V3,1),
                   time=list(time),do_union=FALSE) %>% 
  st_cast("LINESTRING")

train_sf_lstr <- train_sf_lstr %>% dplyr::filter(npts>=5)

saveRDS(train_sf_lstr,"temp_train_sf_lstr.RDS")

rdeck(
  map_style = mapbox_dark(),
  initial_bounds = st_bbox(train_sf_lstr),
) %>% 
  add_trips_layer(
    name = "trains",
    data = train_sf_lstr,width_units = "pixels",
    loop_length = 1,animation_speed = 0.05,trail_length = 0.01,
    width_min_pixels = 2,
    get_color = scale_color_category(
      col = carrier,
      palette = viridisLite::turbo(length(unique(train_sf_lstr$carrier))),
      legend = TRUE
    ),
    get_timestamps = time,
    get_path = geometry
) -> p
p

rdeck(
  map_style = mapbox_dark(),
  initial_bounds = st_bbox(train_sf),
) %>% 
  add_heatmap_layer(
    name = "trains",
    data = train_sf,
    get_position = geometry,
    radius_pixels = 20,threshold = 0.1
  )
