
# Mario Bross 
# https://mkwrs.com/
# https://www.thegamer.com/mario-kart-64-speedrunner-is-the-first-to-hit-a-190000-trick-breaks-two-world-records-at-once/


library(ggcorrplot)


library(tidytuesdayR)
library(rtweet)
library(DataExplorer)
library(tidyverse)
library(lubridate)
library(viridis)
library(ggthemes)
library(extrafont)
library(hrbrthemes)
library(ggExtra)
library(ggtext)
library(ggrepel)
library(extrafont)
library(patchwork)
# fonts()


tuesdata <- tidytuesdayR::tt_load(2021, week = 22)

drivers <- tuesdata$drivers
records <- tuesdata$records

driv_players<-plyr::count(drivers$player)
players<-driv_players$x

my_df <- records%>%
  arrange(player)%>%
  filter(player%in%players)%>%
  mutate(year=year(date))%>%
  inner_join(drivers,by=c("player","year"))%>%
  drop_na()

second_plot

library(RColorBrewer)
display.brewer.all()



plot<-ggplot(my_df,aes(x=date,y=records))+
  geom_point(aes(group=track,color=system_played,size=record_duration),alpha=0.5)+
  geom_smooth(color="purple")+
  scale_color_wsj()+
  labs(title="How did the world records develop over time?",
       size="Record duration(days)",
       color="System played")+
  xlab("World record date(Year)")+
  ylab("Number of world records")+
  theme_wsj()+
  theme(panel.background = element_blank(),
        plot.background = element_blank(),
        legend.title = element_text(size=12),
        legend.background = element_blank(),
        legend.key = element_rect(fill=NA),
        legend.text = element_text(size=12,face="bold"),
        legend.box.margin = margin(6, 6, 6, 6),
        plot.title.position = "plot",
        plot.title = element_text(hjust=0,vjust=-1,size=28,color="darkblue"),
        axis.title = element_text(size=18,color="darkred",face="bold"),
        panel.grid = element_line(color="white"),
        panel.grid.major.y = element_blank())


plot<-ggMarginal(plot,
           type="histogram",color="blue",xparams = list(bins=40),
           fill = "darkred", yparams = list(bins=30))



labels <-
  tibble(
    labels = c(
      "<img src='super_mario_run_character_artwork.png'
      +     width='120' /><br><b style='color:#d6182e'> </b><br><i style='color:#d6182e'> </i></b>"),
    x = 1:5,
    y = rep(1, 5)
    )

legend <-
  ggplot(labels, aes(x, y)) +
  geom_richtext(aes(label = labels),
                fill = NA,
                color = NA,
                vjust = 0.5) +
  annotate("text", x = 3.5, y = 1.018,
           label = "Mario Kart World Records",
           size = 20,color="red",
           fontface = "bold",
           family = "Courier New") +
  scale_x_continuous(limits = c(0.6, 6.1)) +
  scale_y_continuous(limits =  c(1, 1.02)) +
  theme_void() +
  theme(plot.background = element_rect(fill = "#ebe046"),
        plot.margin = unit(c(1,1,0,1), "cm"))

caption <-
  ggplot(data.frame(x = 1:2, y = 1:10)) +
  labs(x = NULL, y = NULL,
       caption = "Viz @fgazzelloni| Source: Mario Kart World Records | TidyTuesday Week 22")+
  theme(line = element_blank(),
        plot.caption = element_text(size=8, family="Courier New",color="#460046",face="bold"),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent",color = "transparent"),
        panel.border = element_rect(color = "transparent"),
        axis.text = element_blank())


 

###################### SAVING ############################



final <- legend + plot  + caption + 
  plot_layout(ncol = 1,heights = c(0.3, 1, 0))

ragg::agg_png(here::here("w22","w22_supermario.png"),
              res = 320, width = 14, height = 8, units = "in")
final

dev.off()



#### ATTACHING LOGO ############################
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


final_plot <- image_read("super_mario_w22.png")

attached_logo <- image_composite(final_plot, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "super_mario_w22.png",
            format = "png") # save final plot



2##############################################################





