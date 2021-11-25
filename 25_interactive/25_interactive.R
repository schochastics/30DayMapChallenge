# http://blog.schochastics.net/post/traveling-beerdrinker-problem/
library(tidyverse)
library(TSP) 
library(leaflet)
library(osrm)
library(sf)

beer <- read_csv("https://gist.githubusercontent.com/schochastics/39a81a1f0159a38070efdd845d4b212b/raw/2807bc7dadc8b64e5f4a97ea78b93f8ccf52e10d/beergarden.csv")
beer_sf <- st_as_sf(beer, coords = c("Long", "Lat"),crs=4326)

# get some info on beer
unique(beer$Biermarke)

n <- nrow(beer)
beer_dist <- matrix(0,n,n)
for(i in 1:n){
  for(j in 1:n){
    beer_dist[i,j] <- geosphere::distGeo(c(beer$Long[i],beer$Lat[i]),
                                         c(beer$Long[j],beer$Lat[j]))
  }
}
beer_tsp <- TSP(beer_dist,labels=beer$Name)
beer_sol <- solve_TSP(beer_tsp,method="concorde")
beer_route <- as.integer(beer_sol)
beer_route <- c(beer_route,beer_route[1])

route <- vector("list",length(beer_route)-1)
for(step in 1:(length(beer_route)-1)){
  cat(step,"\r")
  s <- beer_route[step]
  t <- beer_route[step+1]
  route[[step]] <-  osrmRoute(src = c(beer$Long[s], beer$Lat[s]), 
                       dst = c(beer$Long[t], beer$Lat[t]),
                       returnclass = "sf")
  Sys.sleep(1.5)
}

route_sf <- do.call("rbind",route)
saveRDS(route_sf,"route.RDS")
ggplot(route_sf)+geom_sf(aes(col=distance))

route_df <- as_tibble(st_coordinates(route_sf))
###########
beer_annot <- beer %>% mutate(popup_text=paste(sep = "<br/>", paste0("<b>", Name, "</b>"),paste0("Brewery: ", Biermarke)))
saveRDS(beer_annot,"beer_annot.RDS")

saveRDS(beer_route,"beer_route.RDS")
saveRDS(beer,"beer_data.RDS")

leaflet() %>% 
  # addProviderTiles(providers$OpenStreetMap) %>% 
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% 
  setView(11.5773133,48.1382570, zoom = 10) %>% 
  addPolylines(data=beer[beer_route,],lat=~Lat,lng=~Long,color = "black",opacity = 1,weight=3,group="crow") %>% 
  addPolylines(data=route_sf,color = "#cd2626",opacity = 1,weight = 3,group="osrm route",
               highlightOptions = highlightOptions(color = "#cd2626", weight = 6,
                                                   bringToFront = TRUE)) %>% 
  addCircleMarkers(
    data=beer_annot,
    lng=~Long,
    lat=~Lat,
    popup = ~popup_text,
    radius=5,
    color = "black",
    stroke=TRUE,
    fillOpacity = 1) %>% 
  addLayersControl(
    baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
    overlayGroups = c("crow", "osrm route"),
    options = layersControlOptions(collapsed = FALSE)
  )%>% hideGroup("crow")
