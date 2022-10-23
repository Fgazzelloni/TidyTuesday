# Week 15 FIRE

# libraries, fonts and colours --------------------
library(tidytuesdayR)
library(tidyverse)
library(extrafont)
fonts()

my_col <- "#720000"
my_col2 <- "#9900bfbf"

# load data and wrangling ----------------------------

tuesdata <- tidytuesdayR::tt_load(2021, week = 15)

forest <- tuesdata$forest
forest_area <- tuesdata$forest_area
brazil_loss <- tuesdata$brazil_loss
soybean_use <- tuesdata$soybean_use
vegetable_oil <- tuesdata$vegetable_oil


brazil_loss <- tuesdata$brazil_loss

slopes <- brazil_loss%>%
  pivot_longer(cols=c(5,6,11,12,13),names_to="Predictors",values_to="values")

slopes <- slopes %>% select(year,fire,Predictors,values)

# slope plot --------------------------------------------------

slope_plot <- ggplot(slopes, aes(x = fire, y = values, color = Predictors) ) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .15, aes(fill = Predictors)) +
  theme_minimal() +
  
  scale_y_continuous(name="Predictors", labels = scales::label_number_si(), limits=c(0,92000)) +
  scale_x_continuous(name="Fire (hectares)", labels = scales::label_number_si(), limits=c(26000,537000)) +
  
  
  annotate("curve", x = 400000, xend = 450000, y = 50000, yend = 75000, 
           color = "red", curvature = -0.5) +
  annotate("text", x=500000, y= 75000, label="driving down: tree plantations \ndriving up: natural disturbances", colour=my_col) +
  
  labs(x="Fire",
       y="Predictors",
       title = "Brazil Fire due to predictors",
       subtitle = "flooding, mining, disturbances, plantations, infrastructures...",
       caption = "Viz @fgazzelloni | DataSource: @ourworldindata | Brazil Fire predictors") +
  
  theme(legend.position = "bottom",
        legend.text = element_text(family="Trebuchet MS"),
        legend.background = element_blank(),
        legend.title = element_text(family="Trebuchet MS"),
        legend.key = element_rect(fill = "white", colour = NA),
        
        plot.title = element_text(family="Trebuchet MS", size =32,face="bold", hjust=0 ),
        plot.subtitle = element_text(family="Trebuchet MS", size =20),
        
        axis.title = element_text(family="Trebuchet MS", size =12),
        
        strip.background = element_rect(colour = "black", fill = "white"),
        strip.text.x = element_text(colour = "white", face = "bold"),
        panel.spacing = unit(5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "azure", color = NA),
        panel.background = element_rect(fill = "azure")
       ) +
  annotate("text", x = 280000, y = 80000, 
           family="Trebuchet MS",
           label = "researchers at *Global Forest Watch* estimate that global deforestation \nin 2019 was around 5.4 million hectares. \n95% of this was in the tropics 33.12% in Brazil")
        

# save final plot ---------------------------------------------------------


ragg::agg_png(here::here("w15", "tidytuesday_slope.png"),
              res = 320, width = 14, height = 8, units = "in")
slope_plot

dev.off()


# read the image, attach the Tidytuesday logo and save it --------------------------
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


tidy_slope <- image_read("tidytuesday_slope.png")

attached_logo <- image_composite(tidy_slope, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "tidytuesday_slope.png", format = "png") # save final plot
