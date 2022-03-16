library(tidyverse)
library(ggbump)
library(showtext)
library(sysfonts)
library(extrafont)

showtext::showtext_auto()
showtext::showtext_opts(dpi=320)

font_add_google(name="Noto Sans",family="notosans")


library(ggthemes)


erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

df <- erasmus %>%
  group_by(academic_year) %>%
  filter(between(x = participant_age,17,28),
         mobility_duration>3) %>%
  summarise(m_participants=mean(participants),
            sending_country_code,receiving_country_code,
            .groups="drop") %>%
  ungroup() %>%
  distinct()%>%
  filter(sending_country_code %in% c("DE","ES","IT","RO","UK")) %>%
  mutate(sending_country_name=case_when(sending_country_code=="DE" ~ "Germany",
                                        sending_country_code=="ES" ~ "Spain",
                                        sending_country_code=="IT" ~ "Italy",
                                        sending_country_code=="RO" ~ "Romania",
                                        TRUE~"UK")) %>%
  mutate(year_id=case_when(academic_year=="2014-2015"~1,
                           academic_year=="2015-2016"~2,
                           academic_year=="2016-2017"~3,
                           academic_year=="2017-2018"~4,
                           academic_year=="2018-2019"~5,
                           academic_year=="2019-2020"~6))%>%
  count(year_id,academic_year,sending_country_name) %>%
  group_by(academic_year)%>%
  mutate(rank=rank(x=n,ties.method = "random"))%>%
  ungroup()



df %>%
ggplot(mapping=aes(academic_year,rank,
                   group=factor(sending_country_name),
                   color=factor(sending_country_name))) +
  geom_point(size = 7) +
  geom_text(data = df %>% filter(year_id == min(year_id)),
            aes(x = year_id - .1, label = sending_country_name), 
            size = 4, hjust = 1) +
  geom_text(data = df %>% filter(year_id == max(year_id)),
            aes(x = year_id + .1, label = sending_country_name), 
            size = 4, hjust = 0,check_overlap = T) +
  geom_bump(size = 2, smooth = 8) +
  labs(y = "RANK",
       x = "Academic Year",
       title="Erasmus Top 5 student exchange countries",
       subtitle="Ranks of the highest sending frequency",
       caption="DataSource: Erasmus student mobility | Data.Europa.eu | Wimdu.co\nDataViz: Federica Gazzelloni | #TidyTuesday Week 10 Erasmus") +
  scale_y_reverse() +
  scale_color_manual(values = wesanderson::wes_palette(5, name = "Royal2"))+
  cowplot::theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(legend.position = "none",
        panel.grid.major = element_blank(),
        plot.title = element_text(color="#ffc7ba"),
        plot.subtitle = element_text(color="#ffc7ba"),
        plot.caption = element_text(color="#ffc7ba",size=8),
        axis.text = element_text(color="#ffc7ba"),
        axis.title = element_text(color="#ffc7ba"),
        plot.background = element_rect(color="black",fill="black"),
        panel.background = element_rect(color="black",fill="black"))
 

ggsave("er-network_a.png",height =6,width = 10 )  

