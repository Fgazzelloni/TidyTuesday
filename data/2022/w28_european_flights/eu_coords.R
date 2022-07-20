# European coords

# This code is to make the map for the European countries. 
# To draw the polygons (the borders of the states) I reteived the world data from `ggplot2::map_data()`.


# Set the working directory to the directory where the .R file is saved.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load the library
library(tidyverse)

# load the world data
world <- map_data("world") %>%
  filter(!region=="Antarctica")


# make the map for the selected part of the world
ggplot(world, aes(long, lat)) +
  geom_polygon(aes(group = group),
               size=0.2,
               fill="grey60",
               color="grey80") +
  geom_polygon(data= world %>% 
                 filter(region %in% c("Ukraine","Belgium","France","Italy","Spain","UK")),
               mapping=aes(group = group,fill= region),
               inherit.aes = T,
               size=0.2,
               alpha=0.4,
               color="grey40") +
  # remember to add coord_sf() otherwise the polygons do not match each other
  # in substitution of coord_map.
  coord_sf(xlim = c(-15,50),ylim = c(30,70))+
  ggthemes::theme_map() +
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank())


# save the map as .png
# ggsave("map.png",
#        dpi = 320,
#        width = 4,height = 3)
