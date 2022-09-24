setwd("~/Documents/R/R_general_resources/TidyTuesday/data/2022/w37_bigfoot")
library(tidyverse)
# bigfoot colors: https://icolorpalette.com/color/bigfoot

# load data
bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')
df <- bigfoot%>%
  select(county,
         state,
         latitude,
         longitude,
         date,
         number,
         precip_type,
         visibility,
         classification) %>%
  filter(!is.na(latitude)) %>%
  mutate(precip_type=ifelse(is.na(precip_type),"unknown",precip_type),
         date=as.Date(date,"%Y-%m-%d")) %>%
  mutate(year=lubridate::year(date),.after="date",
         visibility=ifelse(is.na(visibility),mean(visibility,na.rm = T),visibility)) %>%
  filter(year>=1963) %>%
  filter(longitude>-130) %>%
  mutate(state=tolower(state),
         classification=case_when(classification=="Class A"~"clear sightings",
                                  classification=="Class B"~"not clear view",
                                  classification=="Class C"~"second-hand reports")) %>%
  rename(ID=state)
df%>%names



labels <- df%>%
  group_by(ID) %>%
  mutate(pct_view=number/sum(number)*100,.after=number)%>%
  mutate(cent_long=mean(range(longitude)),cent_lat=mean(range(latitude)),.after=longitude)%>%
  ungroup() %>%
  count(ID,cent_long,cent_lat,pct_view)%>%
  group_by(ID) %>%
  summarize(avg_pct_view=round(mean(pct_view),2),cent_long,cent_lat,.groups="drop")%>%
  ungroup() %>%
  distinct()

labels


states <- map_data("state")
world <- map_data("world") %>%
  # set a restricted view to long = c(-122,-66) and lat = c(25,50)
  filter(long> -125,long< -66,
         lat> 25, lat< 60)

# load BigFoot fonts
# library(systemfonts)
# fonts <- system_fonts()
# fonts%>%
#  arrange(family)%>%
#  filter(str_detect(family,"Big"))%>%select(family)


library(randomcoloR)
n <- 48
palette <- distinctColorPalette(n)

set.seed(1)
p <- ggplot() +
  geom_polygon(data = world, mapping = aes(long, lat,
                                           group=group),
               fill="#f4d6b5",color="#446471",
               size=0.2) +
  geom_polygon(data = states, mapping = aes(long, lat,
                                            group=group),
               fill="#ebe2df",color="#446471",
               size=0.2) +
  geom_point(df, mapping = aes(x=longitude,y=latitude,
                               color=ID),
             alpha=0.3,
             size=0.5,show.legend = F) +
  ggrepel::geom_label_repel(labels, 
            mapping = aes(x=cent_long,y=cent_lat,
                          label=avg_pct_view),
            label.padding = unit(0.05,"pt"),
            color="#1a2f38",
            family="Monaco",
            size=3,
            max.overlaps = Inf,
            label.size = unit(0.05,"pt"), 
            fill = "grey90"
            )+
  coord_map() +
  scale_color_manual(values = palette) +
  ggthemes::theme_map() +
  labs(title="Bigfoot",
       subtitle = "Avg(%) views by county from 1963",
       color="",
       caption="\nDataSource: #TidyTuesday2022 week37 BigFoot\nDataViz: Federica Gazzelloni\n") +
  theme_void()+
  theme(text=element_text(family="Monaco",color="grey30"),
        plot.title = element_text(size=55,hjust=0.1,vjust=0.5,
                                  family="Big Bloke BB"),
        plot.subtitle = element_text(size=9,hjust=0.1),
        plot.caption = element_text(vjust = 0.5,hjust=0),
        plot.title.position = "plot",
        legend.key.size = unit(1,units = "pt"),
        legend.background = element_rect(fill="white"),
        legend.key = element_blank(),
        legend.box.background = element_blank(),
        legend.direction = "horizontal",
        legend.position = c(0.3,0.05),
        plot.background = element_rect(fill="#fff0c6",
                                       color="#fff0c6"))
  
library(cowplot)
ggdraw(p)+
  draw_image("bigfoot.png",scale=0.2,
             y=0.144,x=-0.15) +
  draw_image("bigfoot.png",scale=0.15,
             y=0.12,x=0)+
  draw_image("bigfoot.png",scale=0.1,
             y=0.08,x=0.1)



ggsave("w37_bigfoot.png",
       bg="#fff0c6",
       dpi=320,
       width = 5.81,
       height = 6)

dev.off()

