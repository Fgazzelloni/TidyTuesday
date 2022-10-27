


library(tidyverse)
library(showtext)
library(sysfonts)
library(extrafont)
library(ggridges)
library(cowplot)


# set the fonts
showtext::showtext_auto()
showtext::showtext_opts(dpi=320)
font.add(family = "Benguiat",regular="Benguiat Normal.ttf")





episodes_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')

df <-
  episodes_raw %>%
  filter(!is.na(dialogue)) %>%
  mutate(season = paste("Season", season)) %>%
  select(season,episode,start_time,end_time,dialogue) %>%
  mutate(time = end_time-start_time,
         time = as.numeric(time),
         trims = trimws(dialogue),
         space = sapply(strsplit(trims, " "), length)) %>% #pull(time)%>%summary()
  filter(time > 1) %>%
  mutate(velocity = space/(time/60)) %>%
  mutate(time_s=time,
         time = time/60)
  
  

p <- df %>%
  group_by(season,episode)%>%
  mutate(velocity=mean(velocity))%>%
  ungroup()%>%
  ggplot(aes(x = velocity,y = fct_reorder(factor(episode),desc(episode)),
             fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  ggridges::stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE, alpha=0.5) +
  scale_fill_viridis_c(name = "Speed wpm", option = "A",labels = scales::percent) +
  scale_x_continuous(expand = c(0,0)) +
  labs(title="All seasons episodes",
       caption="DataSource: #TidyTuesday 2022 week 42 by Stranger things dialogue | DataViz: Federica Gazzelloni (@fgazzelloni)",
       x="Dialogue speech speed (wpm)",y="Episode")+
  ggridges::theme_ridges()+
  theme(text=element_text(color="grey90",family="Benguiat"),
        axis.text.x = element_text(color="grey90",size=9),
        axis.text.y = element_text(color="grey90",size=9),
        plot.margin = margin(10,10,10,10,unit = "pt"),
        plot.title = element_text(size=12,hjust = 0.03),
        plot.title.position = "panel",
        plot.subtitle = element_text(color="grey90"),
        plot.caption = element_text(size=10),
        plot.caption.position = "plot",
        plot.background = element_rect(color="grey10",fill="grey10"),
        panel.background = element_rect(color="grey10",fill="grey10"),
        strip.background = element_blank(),
        legend.text = element_text(size=9),
        legend.title = element_text(size=8))

ggsave("p.png",dpi=400,width = 8,height = 4)

p1 <- ggplot(df, 
             aes(x = time, y = velocity,group=space)) +
  geom_jitter(aes(color=factor(season)),
              size=0.2,
              alpha=0.3)+
  geom_line(aes(color=factor(season)),
            size=0.2,
            alpha=0.5) +
  scale_y_continuous(expand = c(0,0)) +
  scale_color_viridis_d(option = "A")+
  labs(title = "Talking fast slows down as time increases\n",
       y = "Speed (wpm)", x = "Time in minutes")+
  ggridges::theme_ridges()+
  theme(text=element_text(color="grey90",size=14,family="Benguiat"),
        axis.text.x = element_text(color="grey90"),
        axis.text.y = element_text(color="grey90"),
        axis.line = element_line(color="grey90",
                                 arrow = arrow(type='closed', 
                                               length = unit(10,'pt'))),
        panel.grid.major = element_line(color="grey60",size=0.1,linetype = "dashed"),
        plot.margin = margin(10,10,10,10,unit = "pt"),
        plot.subtitle = element_text(color="grey90"),
        plot.caption = element_text(size=11),
        plot.background = element_rect(color="grey10",fill="grey10"),
        panel.background = element_rect(color="grey10",fill="grey10"),
        strip.background = element_blank(),
        legend.position = "none")



ggsave("p1.png",dpi=300,width = 9,height = 4)


cowplot::ggdraw()+
  draw_image("p.png",scale=0.93,x=-0.02,y=-0.22)+
  draw_image("p1.png",scale=0.7, y=0.22,x=-0.13)+
  draw_image("logo.png",scale = 0.2,y=0,x=0.4)+
  draw_label("Stranger things...on dialogue's speed", 
             fontfamily = "Benguiat",
             x=0.4,y=0.95,
             size=25,
             color="grey90")+
  draw_label("wpm = word/per minutes\n\nWord Speed:\nnumber of words\ndivided by time difference\nfrom end to start\n\nAvg 2.5 words per second\n\n\nSlow: less than 110 wpm\nConversational: 120 - 150 wpm\nFast: more than 160 wpm",
             x=0.96,y=0.74,color="grey90",
             hjust = 1,
             size=9,
             lineheight = 1.5,
             fontfamily = "Benguiat")+
  theme(plot.background = element_rect(fill="grey10", color = "grey10"))



ggsave("w42_stranger_things_dialogue.png",
       dpi=200,
       width = 14,
       height = 12)


