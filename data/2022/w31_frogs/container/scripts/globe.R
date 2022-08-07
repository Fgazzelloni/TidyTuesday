
## This script is to make the Globe plot.

library(tidyverse)
library(rnaturalearth)

# world data full 
world_full <- ne_countries(scale = "medium", returnclass = "sf")
world_data <- filter(world_full, continent != "Antarctica")

# world lat&long
world<-map_data(map = "world") 

my_world_data<- world %>%
  full_join(world_data, by = c("region"="name")) %>%
  select(long,lat,group,order,region,region_wb)

# states lat&long
states <- map_data("state")
states_oregon<-states %>% filter(region=="oregon")

# grob for globe water
g <- grid::circleGrob(gp = grid::gpar(fill = "#9ad6f0",color="#47bccf"))

world<-ggplot() +
  geom_polygon(data=my_world_data,
               aes(x=long,y=lat,group=group),
               fill="gray",color="#9ad6f0",size=0.05) + 
  geom_polygon(data = states,aes(x = long, y = lat, group = group),
               fill="pink",color="#9ad6f0",size=0.05,alpha=0.1)+
  geom_polygon(data = states_oregon,aes(x = long, y = lat, group = group),
               fill="red",color="#9ad6f0",size=0.3)+
  coord_map("ortho", orientation = c(40.84419442708065, -100.20830051641454,0)) + #c(3.849945, -103.525750, 0))+ 
  theme_void() 

library(cowplot)
globe <- ggdraw() +
  draw_grob(g, scale = 1,x = 0,y = 0) +
  draw_plot(world)

globe

# ggsave(here::here("data/2022/w31_frogs/container/images/globe.png"),dpi=320,bg="white")

