# week 19 Water------------

library(tidytuesdayR)
library(tidyverse)
library(sf)
library(raster)
library(spData) # spatial data
library(spDataLarge)
library(tmap)    # for static and interactive maps
library(leaflet) # for interactive maps


# loading data --------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 19)

# manipulation ---------------
water <- tuesdata$water
water$water_source[is.na(water$water_source)] <- "unknown"
water_df<-water%>%
  arrange(row_id)%>%
  clean_names()%>%
  mutate(report_date=format(as.Date(report_date,"%m/%d/%Y")),
         report_date=as.Date(report_date),
         water_source=as.factor(water_source),
         status_id=as.factor(status_id))%>%
  select(1:6,9)

# loading africa -------------------
world_africa = world[world$continent == "Africa", ]
africa = st_union(world_africa)

# plotting --------------------
library(extrafont)
library(patchwork)

water_plot<-ggplot() +  
  geom_sf(data = africa) +
  geom_point(data = water_df,
             aes(x=lon_deg,y=lat_deg,color=country_name),
             alpha=0.5) +
  labs(title="Africa source of water by country",
       subtitle="",
       caption="Viz. Federica Gazzelloni DataSource: Water Access Points, WPDX TidyTuesday week 19")+
  theme_ps()+
  theme(legend.position = "none",
        plot.margin = margin(5,5,5,5),
        plot.title = element_text(size=24,family="xkcd",vjust=-2),
        plot.caption = element_text(size=8,family="xkcd",hjust = 0.5,vjust=-2))


# background ---------
library(ggimage)


image<-"frame.png"

final_plot<-ggbackground(water_plot, image, alpha=.7,color="#94BCFF")



####### SAVING ######################################
ragg::agg_png(here::here("tidytuesday_Water.png"),
              res = 320, width = 14, height = 8, units = "in")
final_plot

dev.off()



#### ATTACHING LOGO ############################ 
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


final_plot <- image_read("W19/tidytuesday_Water.png")

attached_logo <- image_composite(final_plot, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "tidytuesday_Water.png", format = "png") # save final plot






