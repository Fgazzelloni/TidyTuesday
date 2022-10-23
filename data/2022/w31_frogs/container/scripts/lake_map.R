
## This script is to make the lake map plot.

library(tidyverse)

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')

# Get the latitude and the longitude from data.
frogs_coord <- tibble(frogs$UTME_83, frogs$UTMN_83)


# Transform the coords into geometry for mapping.
frogs_coord <- 
  sf::st_as_sf(x = frogs_coord, 
               coords = c(1,2), 
               crs = "+proj=utm +zone=10") %>%
  sf::st_transform(frogs_coord, 
                   crs = "+proj=longlat +datum=WGS84")  %>%
  tibble()



frogs_location <- tibble(Detection = frogs$Detection,
                         Subsite = frogs$Subsite,
                         Frequency = frogs$Frequency,
                         lat = unlist(map(frogs_coord$geometry, 2)),
                         long = unlist(map(frogs_coord$geometry, 1)))


# frogs_captured
frogs_captured <- frogs_location %>%
  filter(Detection=="Captured")
# frogs_vis
frogs_vis <- frogs_location %>%
  filter(Detection=="Visual")
# frogs_novis
frogs_novis <- frogs_location %>%
  filter(Detection=="No visual") %>%
  group_by(Subsite) %>%
  mutate(range=round(max(Frequency)-min(Frequency),2)*50) %>%
  ungroup()
# frog_subsite
frog_subsite<-frogs_location%>%
  group_by(Subsite)%>%
  summarise(lat=mean(range(lat)),
            long=mean(range(long))) %>%
  ungroup()


# Adjust the mean range of the subsite latitude and longitude to set the labels.
frog_subsite<-frog_subsite %>%
  mutate(lat=case_when(Subsite=="N Res"~43.808,
                       Subsite=="NE Res"~43.812,
                       TRUE~lat),
         long=case_when(Subsite=="Cow Camp River"~-121.782,
                        Subsite=="Cow Camp Pond"~-121.782,
                        TRUE~long))



## Lake Map

# Use {leaflet} for mapping at **Crane Prairie Reservoir in Oregon, USA**
# More info on documentation here:
# - Bounding Box [-121.824775, 43.764375, -121.764923, 43.814821]
# - https://www.sciencebase.gov/catalog/item/imap/60ba5a00d34e86b9388d86bc
# - leaflet docs: https://rstudio.github.io/leaflet/map_widget.html
# - logo credit: https://www.vecteezy.com/free-vector/frog-logo



frog_logo_captured <- file.path(here::here("data/2022/w31_frogs/container/images/frog_logo_captured.png"))
frog_logo_visual <- file.path(here::here("data/2022/w31_frogs/container/images/frog_logo_visual.png"))

library(leaflet)

lake_map <- leaflet(options = leafletOptions(minZoom = 13.8, 
                                        maxZoom = 13.8,
                                        zoomControl=F)) %>% 
  addTiles() %>%
  fitBounds(-121.824775, 43.764375, -121.764923, 43.814821) %>%
  addCircleMarkers(lng = frogs_novis$long, 
                   lat = frogs_novis$lat,
                   radius = frogs_novis$range,
                   color = "gray",
                   weight = c(0,0.2,0.5),
                   fillOpacity = 0.02) %>%
  addMarkers(lng = frogs_vis$long, 
             lat = frogs_vis$lat,
             icon =  list(iconUrl = frog_logo_visual, 
                          iconSize = c(70, 60))) %>%
  addMarkers(lng = frogs_captured$long, 
             lat = frogs_captured$lat,
             icon =  list(iconUrl = frog_logo_captured, 
                          iconSize = c(70, 60))) %>%
  addLabelOnlyMarkers(lng = frog_subsite$long, 
                      lat = frog_subsite$lat,
                      label = frog_subsite$Subsite,
                      labelOptions = labelOptions(noHide = T, 
                                                  textOnly = TRUE,
                                                  direction = "top",
                                                  style = list(
                                                    "color" = "navy",
                                                    "font-family" = "Roboto Condensed",
                                                    "font-style" = "italic",
                                                    #"box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                                    "font-size" = "10px",
                                                    "border-color" = "rgba(0,0,0,0.5)")))%>%
  addMiniMap(zoomLevelOffset = -9,width = 200)

lake_map
# Then take a shot and save the png to join it with other plots.
