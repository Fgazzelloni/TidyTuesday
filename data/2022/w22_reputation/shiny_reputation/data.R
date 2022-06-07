library(tidyverse)

poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')


rep2<-reputation%>%
  group_by(company,industry)%>%
  summarize(score,rank)%>%
  ungroup()%>%
  mutate(year=2022)



full <- poll%>%
  filter(!is.na(year))%>%
  full_join(rep2,by=c("2022_rank"="rank","2022_rq"="score","company","industry","year")) %>%
  count(year,company,industry,"rank"=`2022_rank`,"score"=`2022_rq`,sort=T) %>%
  arrange(-year)



rank_plot <- function(data_rank) {
  data_rank %>%
ggplot(aes(x=fct_reorder(company,-rank),y=rank))+
  geom_col(aes(fill=rank),
           width =0.3,
           show.legend = F)+
  geom_text(aes(label=rank,color=rank),
            hjust=0,fontface="bold",
            show.legend = F)+
  scale_y_discrete(expand = c(0, 0, .5, 0))+
  coord_flip()+
  ggthemes::scale_fill_continuous_tableau(palette = "Green-Gold")+
  ggthemes::scale_color_continuous_tableau(palette = "Green-Gold")+
  labs(title="",
       x="",y="")+
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(face="bold"),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_line(size=2),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(size=2),
        plot.background = element_rect(color="grey95",fill="grey95"),
        panel.background = element_rect(color="grey92",fill="grey92"))
}



