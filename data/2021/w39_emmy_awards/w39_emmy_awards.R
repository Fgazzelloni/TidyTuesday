# TidyTuesday
# week 39 nominees EMMYS'

# load libraries --------------

library(tidyverse)

library(extrafont)
library(showtext)
#font_families_google()
font_add_google("Roboto Condensed","Roboto Condensed")

library(patchwork)
library(cowplot)

# load the data ------

nominees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-21/nominees.csv')
head(nominees)

# data wrangling -------
# arrange the set to have the count of the types
# add a column with the icons for types

my_df <-
  nominees %>%
  filter(year>=2015) %>%
  filter(distributor==c("HBO")) %>%
  count(year,distributor,type,sort=T) %>%
  mutate(distributor_lab=paste0("HBO_",year))%>%
  mutate(img = if_else(
      type == "Winner",
      "<img src='w39/emmy_winner.png' width='12'/>",
      "<img src='w39/emmy_nom.png' width='12'/>"
    ))



# set a special vector for the facet strip names with the type counts

type_c <- c("HBO_2015"= "219 to 140",
            "HBO_2016"= "198 to 75",
            "HBO_2017"= "216 to 118",
            "HBO_2018"= "302 to 96",
            "HBO_2019"= "297 to 146",
            "HBO_2020"= "217 to 124",
            "HBO_2021"= "261 to 66") 


# make the plot -------

library(waffle)
library(ggtext)
library(ggthemes)


plot <- ggplot(my_df, aes(fill = type, values = n,label = img)) +
  facet_wrap(distributor_lab~year, nrow = 1, strip.position = "bottom",
             labeller = labeller(distributor_lab  = as_labeller(type_c))) +
  stat_waffle(geom = "richtext", fill = NA, label.color = NA, flip = TRUE, n_rows = 10) +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10, expand = c(0,0)) +
  ggthemes::scale_fill_fivethirtyeight(name=NULL) +
  coord_equal() +
  labs(title="\n",subtitle="\n",
    caption="Source: The data this week comes from emmys.com") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(text=element_text(family = "Roboto Condensed"),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_text(family = "Roboto Condensed",size=22,face="bold"),
        legend.position = "top",
        panel.grid.major.x = element_line(color="grey50",size=10),
        panel.grid.minor.x = element_line(color="grey50",size=10),
        plot.margin = margin(5,5,1,5,unit = "pt"),
        plot.background = element_rect(color="#F0F8FF", fill="#F0F8FF"),
        plot.caption.position = "panel",
        plot.caption = element_text(family = "Roboto Condensed",color= "grey40", face="bold",size=30, hjust=0,vjust=-1)) 


#------------finish touches

library(ggimage)
library(magick)
library(cowplot)


library(ggpubr)

graphics <- ggarrange(plot) +
  theme(plot.background = element_rect(fill = "#F0F8FF", color = "#F0F8FF"))


# annotate the plot

final_plot <- annotate_figure(graphics,
                              top = text_grob("EMMY AWARD WINNERS AND NOMINEES",
                                              color = c("grey28"), face = "bold", size = 34,
                                              family = "Roboto Condensed"),
                              bottom = text_grob("Infographics Federica Gazzelloni DataSource: TidyTuesday week39",
                                                 color = "grey28",family = "Roboto Condensed",
                                                 hjust = 0.5, x = 0.5, face = "bold.italic", size = 24),
                              left = text_grob("", color = c("#778899"), rot = 90,size = 12),
                              right = text_grob(bquote(""), color = c("#778899"),rot = 90,size = 10),
                              fig.lab = "", fig.lab.face = "bold.italic",fig.lab.size = 8,
                              fig.lab.pos = "bottom.right"
)

final_plot <-
  final_plot +

  annotate(geom = 'segment',y = 0.78, yend = 0.93, x = 0.1,xend = 0.1, color="#8A2BE2", size = 9) +

  annotate(geom = "text", label = "HBO RECORD AT THE EMMYS \nAWARDS AND NOMINEES",
           x = 0.44, y = 0.875,colour = "black",size = 16,family = "Roboto Condensed",fontface="bold") +

  annotate(geom = "text", label = "Number of Emmy nominations and wins for HBO",
           x = 0.44, y = 0.79,colour = "grey40",size = 10,family = "Roboto Condensed") +
  
  annotate(geom = "text", label = "Winner",
         x = 0.5, y = 0.1,colour = "grey50",size = 8,family = "Roboto Condensed") +
  annotate(geom = "text", label = "Nominee",
         x = 0.32, y = 0.1,colour = "grey50",size = 8,family = "Roboto Condensed") 


# add the images for the legend keys 

imgWin <- image_read(here::here("w39/emmy_winner.png"))
imgNom <- image_read(here::here("w39/emmy_nom.png"))
imgHBO <- image_read(here::here("w39/hbo.png"))

final <- ggdraw() +
  draw_plot(final_plot) +
  draw_image(imgWin, x = 0.55, y = -0.4,width = 0.06)+
  draw_image(imgNom, x = 0.37, y = -0.4,width = 0.06)+
  draw_image(imgHBO, x = 0.1, y = -0.1,width = 0.12)



## save final plot ----

ragg::agg_png(here::here("w39/w39_nominees.png"),
              res = 320, width = 12, height = 14, units = "in")
final

dev.off()



# read the image, attach the Tidytuesday logo and save it --------------------------


tidy_logo <- image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")

tidy_final <- image_read(here::here("w39/w39_nominees.png"))
attached_logo <- image_composite(tidy_final, tidy_logo,
                                 operator = "atop",
                                 gravity = "southeast")

image_write(attached_logo, path = "w39_nominees.png", format = "png")






