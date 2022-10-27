---
title: "w42 Stranger things dialogue"
output: html_document
date: "2022-10-18"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
episodes_raw <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')

episodes <-
  episodes_raw %>%
  filter(!is.na(dialogue)) %>%
  mutate(season = paste0("season", season))

episodes
```

```{r}
episodes%>%names
```


```{r}
df <- episodes %>%
  select(season,episode,start_time,end_time,dialogue)%>%
  mutate(#season=gsub("season","",season),
         time=end_time-start_time,
         time=as.numeric(time),
         trims=trimws(dialogue),
         trims=gsub(" ","",trims),
         space=nchar(trims)) %>%
  select(-start_time,-end_time,-dialogue,-trims)%>%
  mutate(velocity=space/time)%>%
  filter(time>0)



  #df %>%
  #filter(season=="1",episode==1)%>%
  #arrange(time)
  #count(stage_direction)
  #DataExplorer::profile_missing()
```

```{r}
df %>%
  ggplot(aes(time,velocity,group=episode))+
  geom_point(aes(color=factor(season)))
```

```{r}
df %>%
  group_by(season,episode)%>%
  summarize_all(.funs=mean)%>%
  ungroup()%>%
  ggplot(aes(time,velocity))+
  geom_point(aes(color=factor(season)),size=5)
```


```{r}
library(ggridges)
df %>%#summary()
  group_by(season,episode)%>%
  summarize_all(.funs=mean)%>%
  ungroup()%>%
  ggplot(aes(time,velocity,height=space))+
  ggridges::geom_ridgeline()+
  facet_wrap(~season,scales = "free")
```


```{r}
df %>%#summary()
  #group_by(season,episode)%>%
  #summarize_all(.funs=mean)%>%
  #ungroup()%>%
  #filter(episode==1)%>%
  ggplot(aes(velocity,factor(episode)))+
  geom_density_ridges()+
  facet_wrap(vars(season),scales = "free")
```



```{r}
df %>%#summary()
  #group_by(season,episode)%>%
  #summarize_all(.funs=mean)%>%
  #ungroup()%>%
  #filter(episode==1)%>%
  ggplot(aes(time,factor(episode)))+
  geom_density_ridges()+
  facet_wrap(vars(season),scales = "free")
```

```{r}
# library(RColorBrewer)
library(showtext)
library(sysfonts)
library(extrafont)

# set the fonts
showtext::showtext_auto()
showtext::showtext_opts(dpi=320)
font_add_google(name="Pangolin",family="pangolin")
```



```{r}
df%>%#summary()
  group_by(season)%>%
  summarize_all(.funs=mean)%>%
  ungroup()%>%
  ggplot(aes(time,velocity))+
  geom_smooth()+
  geom_point(aes(color=factor(season)),
             size=15,
             shape=c("1","2","3","4"),
             show.legend = F)+
  scale_color_manual(values=c("#008FD5", "#FF2700", "#77AB43", "black"))+
  #ggthemes::theme_fivethirtyeight()+
  theme(text = element_text(family="pangolin"))
```


```{r}
# options(timeout = 600)
# install.packages("ggalt")
# library(ggalt)
# quartz()
# df%>%
# ggplot(aes(time, velocity, group=season, color=factor(season))) +
#   geom_point(color="black") +
#   geom_smooth(se=FALSE, linetype="dashed", size=0.5) +
#   geom_xspline(spline_shape=-0.4, size=0.5)
```

```{r}

ggplot(df, 
       aes(x = time, y = velocity,group=space)) +
       geom_jitter(size=0.2,
                   aes(color=factor(season)),
                   alpha=0.5)+
       geom_line(aes(color=factor(season)),
                 size=0.2,
                 alpha=0.5) +
  geom_density(aes(y = velocity, 
                   group=factor(season), 
                   #fill=factor(season),
                   color=factor(season)),
               inherit.aes = F)+
  geom_density(aes(x = time,
                   group=factor(season),
                   #fill=factor(season),
                   color=factor(season)),
               inherit.aes = F)+
  scale_x_discrete(expand = c(0,0)) +
  scale_color_hue()+
  #facet_wrap(~season,scales = "free")+
  ggthemes::theme_fivethirtyeight()
  
```


```{r}
df %>%
  ggplot() +
  geom_density(aes(x=velocity,group=episode))+
  facet_wrap(~season,scales = "free")
```

```{r}
df %>%
  ggplot(aes(x=time,y=space,z=velocity,group=season)) +
  geom_contour_filled()
```


```{r}
df %>%
  ggplot(aes(x=time,y=space,z=velocity,group=season)) +
  #geom_contour_filled()+
  geom_contour(aes(colour = after_stat(level)),size=2)
```
```{r}
df %>%
  ggplot(aes(x=time,y=velocity,width=space,group=season)) +
  geom_vridgeline(aes(color=season,fill=season))
```


```{r}
df %>%
  ggplot(aes(x=time,y=velocity,width=space,group=season)) +
  geom_hex()
```
```{r}
df %>%
  ggplot(aes(velocity,space,group=season)) +
  geom_point()
```


```{r}
df%>%
  ggplot(aes(space,velocity,group=season))+
  #geom_point(aes(color=season),alpha=0.2)+
  geom_smooth(se=F)
```

```{r}
df %>%
  group_by(season,episode)%>%
  mutate(velocity=mean(velocity))%>%
  ungroup()%>%
  ggplot(aes(x=velocity,group=season)) +
  geom_density(aes(fill=season,alpha=season))+
  scale_fill_cyclical(values = c("orange","grey","green","lightblue"))+
  scale_alpha_cyclical(values = c(0.4, 0.8))
```
```{r message=FALSE, warning=FALSE, paged.print=FALSE}
df %>%
  group_by(season,episode)%>%
  mutate(velocity=mean(velocity))%>%
  ungroup()%>%
  ggplot(aes(velocity,factor(episode),fill=stat(x))) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis_c(name = "Velocity w/s", option = "C") +
  # geom_density_ridges(aes(fill=velocity))+
  facet_wrap(~season,scales = "free") +
  labs(title="Episode dialogue speed by season",
       #subtile="Speed or velocity = space/time",
       caption="DataSource: #TidyTuesday 2022 week 42 by Stranger things dialogue\nDataViz: Federica Gazzelloni (@fgazzelloni)")+
  theme_ridges()+
  theme(plot.margin = margin(1,1,1,1,unit = "pt"),
        plot.subtitle = element_text())
```


```{r}
df %>%
  group_by(season,episode)%>%
  mutate(velocity=mean(velocity))%>%
  ungroup()%>%
  ggplot(aes(velocity,factor(episode),fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Tail probability", direction = -1,option = "C")+
  theme_ridges()
```

```{r message=FALSE, warning=FALSE, paged.print=FALSE}
df %>%
  group_by(season,episode)%>%
  mutate(velocity=mean(velocity))%>%
  ungroup()%>%
  ggplot(aes(velocity,factor(episode),fill = 0.5 - abs(0.5 - stat(ecdf)))) +
  stat_density_ridges(geom = "density_ridges_gradient", calc_ecdf = TRUE) +
  scale_fill_viridis_c(name = "Speed w/s", option = "C") +
  # geom_density_ridges(aes(fill=velocity))+
  facet_wrap(~season,scales = "free") +
  labs(title="Episode dialogue speed by season",
       #subtile="Speed or velocity = space/time",
       caption="DataSource: #TidyTuesday 2022 week 42 by Stranger things dialogue\nDataViz: Federica Gazzelloni (@fgazzelloni)",
       x="Speed",y="Episode")+
  theme_ridges()+
  theme(text=element_text(color="grey90",size=14),
        plot.margin = margin(1,1,1,1,unit = "pt"),
        plot.subtitle = element_text(),
        plot.background = element_rect(color="grey5",fill="grey5"),
        panel.background = element_rect(color="grey5",fill="grey5"),
        strip.background = element_blank(),
        legend.text = element_text(size=9),
        legend.title = element_text(size=8))
```

```{r}

p2 <- df %>%
  group_by(season,episode)%>%
  summarize_all(.funs=mean)%>%
  ungroup()%>%
  ggplot(aes(time,velocity))+
  geom_path(size=0.2,
            color="grey80",
            arrow = arrow(length=unit(0.15,"cm"), 
                          ends="last", 
                          type = "closed"))+
  geom_point(aes(color=factor(season)),
             #show.legend = F,
             size=5)+
  scale_color_viridis_d(option = "A")+
  guides(color=guide_legend(nrow = 2,title = ""))+
  labs(title="All seasons",
       y="Speed wpm",x="Avg time in minutes")+
  theme_ridges()+
  theme(text=element_text(color="grey90",family="Benguiat"),
        axis.text.x = element_text(color="grey90",size=9),
        axis.text.y = element_text(color="grey90",size=9),
        axis.line = element_line(color="grey90",
                                 arrow = arrow(type='closed', 
                                               length = unit(10,'pt'))),
        panel.grid.major = element_line(color="grey60",size=0.1,linetype = "dashed"),
        plot.margin = margin(10,10,10,10,unit = "pt"),
        plot.title = element_text(color="grey90",size=12),
        plot.caption = element_text(size=11),
        plot.background = element_rect(color="grey10",fill="grey10"),
        panel.background = element_rect(color="grey10",fill="grey10"),
        strip.background = element_blank(),
        legend.text = element_text(size=9),
        legend.background = element_blank(),
        legend.key.height = unit(2,units = "pt"),
        legend.position = c(0.02,-0.14),
        legend.direction = "horizontal")

ggsave("p2.png",dpi=300,width = 6,height = 4)

```

