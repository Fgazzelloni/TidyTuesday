
## This script is to make the VIP plot by gender.



library(tidyverse)
library(tidymodels)

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')

frogs1 <- frogs %>%
  janitor::clean_names() %>%
  mutate(survey_date=as.Date(survey_date,"%m/%d/%Y"),
         female=as.factor(female))%>%
  arrange(ordinal) %>%
  select(-site) %>%
  mutate(subsite=case_when(subsite=="W Res"~"West Reservoir",
                           subsite=="SE Pond"~"South East Pond",
                           subsite=="NE Res"~"North East Reservoir",
                           subsite=="N Res"~"North Reservoir",
                           TRUE ~ subsite)) 


vip_plot <- recipe(female ~ . ,frogs1) %>%
  prep() %>%
  bake(new_data=NULL) %>%
  mutate(female=ifelse(female==0,"Female","Male")) %>%
  filter(detection=="Captured") %>%
  rename(habitat=hab_type) %>%
  select(female,subsite,habitat,water,structure,substrate) %>% #count(subsite)
  pivot_longer(subsite:substrate,
               names_to="feature",values_to="value") %>%
  mutate(feature=toupper(feature)) %>%
  ggplot(aes(y=value,fill=female))+
  geom_bar(position = "fill")+
  geom_vline(aes(xintercept=0.5))+
  facet_grid(rows=vars(feature),scales="free_y",space="free_y")+
  labs(fill="Who is more likely\nto be captured?",y="",x="",
       caption = "Gender dependent VIP classification")+
  scale_fill_brewer(type="qual")+
  scale_x_continuous(labels = scales::percent,expand = c(0,0))+
  theme(text = element_text(family = "Roboto Condensed",
                            size=20),
        plot.caption= element_text(vjust=5),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold",size=12),
        legend.title = element_text(face="bold"),
        plot.background = element_rect(color="white",fill="white"),
        panel.background = element_rect(color="white",fill="white"))


vip_plot
# ggsave(here::here("data/2022/w31_frogs/container/images/vip_plot.png"),height = 9)
