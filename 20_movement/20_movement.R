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

train_sf_lstr <- train_sf %>% 
  select(V1,V8,geometry) %>% 
  group_by(V1) %>% dplyr::summarise(npts=n(),time=list(as.numeric(V8)-1637329241),do_union=FALSE) %>% st_cast("LINESTRING")
train_sf_lstr <- train_sf_lstr %>% dplyr::filter(npts>=5)

train_sf_lstr$time <- map(train_sf_lstr$time,function(x) x/80000)

rdeck(
  map_style = mapbox_dark(),
  initial_bounds = st_bbox(train_sf_lstr),
) %>% 
  add_trips_layer(
    name = "trains",
    data = train_sf_lstr,width_units = "pixels",loop_length = 1,animation_speed = 0.4,trail_length = 0.01,
    width_min_pixels = 2,get_color = "#ffffff",
    get_timestamps = time,get_path = geometry
  )

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
