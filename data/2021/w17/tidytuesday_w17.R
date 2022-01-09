# TidyTuesday week17 NETFLIX & Upwards day20



library(tidytuesdayR)
library(tidyverse)
library(extrafont)
library(showtext)

showtext_opts(dpi = 320)
showtext_auto(enable = TRUE)
my_family = "Roboto Condensed"

#tuesdata <- tidytuesdayR::tt_load(2021, week = 17)

netflix <- tuesdata$netflix

head(netflix)
dim(netflix)

set.seed(73)
NETFLIX_plot <- ggplot(data=netflix, aes(x=release_year, y=sample(7787),fill=release_year)) +
  geom_col() +
  scale_fill_gradient(low = "#333333",high = "red") + 
  labs(title="NETFLIX",
       subtitle="Show released years from 1925 to 20121",
       caption="Viz Federica Gazzelloni | DataSource: Kaggle - 'NETFIX titles'| Tidytuesday week 17 & Upwards Day 20",
       x="Time(Year)",
       y="",
       fill="Release Year")+
  theme_void() +
  theme(plot.title=element_text(family=my_family,size=50,face="bold",color="red"),
        plot.subtitle = element_text(family=my_family,size=15,face="bold"),
        plot.caption = element_text(family=my_family,size=10,face="bold"),
        
        panel.background = element_rect(fill = "#333333") ,
        plot.margin = margin(10,10,10,10),
        panel.grid = element_line(color = "white",size=2),
        panel.grid.major = element_line(color = "white",size=1.5),
        panel.grid.minor =element_line(color = "white",size=2), 
        axis.line = element_line(colour = "white"),
        axis.line.x = element_line(color="white"),
        axis.line.y = element_blank(),
        axis.text.x = element_text(family=my_family,size=10,face="bold"),
        legend.text = element_text(family=my_family,size=10,face="bold"),
        legend.title = element_text(family=my_family,size=10,face="bold"),
        legend.position = "bottom")



####### SAVING ######################################
ragg::agg_png(here::here("w17", "tidytuesday_NETFLIX.png"),
              res = 320, width = 14, height = 8, units = "in")
NETFLIX_plot

dev.off()



#### ATTACHING LOGO ############################ 
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


tidy_NETFLIX_plot <- image_read("W17/tidytuesday_NETFLIX.png")

attached_logo <- image_composite(tidy_NETFLIX_plot, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "tidytuesday_NETFLIX.png", format = "png") # save final plot










