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
# states <- map_data("state")
# states_oregon<-states %>% filter(region=="oregon")

# grob for globe water
g <- grid::circleGrob(gp = grid::gpar(fill ="#d8eaf2")) # "#b3d8e8"))

library(ggforce)
circles <-
  data.frame(
    x0 = text_full$X,
    y0 = text_full$Y,
    r = text_full$avg)

world<-ggplot() +
  geom_polygon(data=my_world_data,
               aes(x=long,y=lat,group=group),
               fill="gray",color="grey60",size=0.05) + 
  geom_circle(data = circles, aes(x0 = x0, y0 = y0, 
                                  r = r,
                                  fill=r,
                                  alpha=r),
              color="gray",size=0.2)+
  scale_fill_gradient(low = "white",high = "blue")+
  scale_alpha_continuous(range = c(0.1,0.3))+
  coord_map("ortho", orientation = c(-20, 160,0)) + 
  theme_void() +
  theme(legend.position = "none")

world

library(cowplot)
globe <- ggdraw() +
  draw_grob(g, scale = 1,x = 0,y = 0) +
  draw_plot(world)

globe

ggsave("~/Documents/R/R_general_resources/TidyTuesday/data/2022/w35_pell/globe.png",
       dpi=320,bg="transparent")

