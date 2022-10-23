
## This script is to make the network plot.


library(tidyverse)
library(igraph)
library(ggraph)

frogs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-02/frogs.csv')

frogs1 <- frogs %>%
  janitor::clean_names() %>%
  mutate(survey_date=as.Date(survey_date,"%m/%d/%Y"),
         female=as.factor(female)) %>%
  arrange(ordinal) %>%
  select(-site) %>%
  mutate(subsite=case_when(subsite=="W Res"~"West Reservoir",
                           subsite=="SE Pond"~"South East Pond",
                           subsite=="NE Res"~"North East Reservoir",
                           subsite=="N Res"~"North Reservoir",
                           TRUE ~ subsite)) 

network_plot <- frogs1 %>% 
  filter(detection=="Captured") %>% 
  count(hab_type,water,substrate,frequency) %>%
  igraph::graph_from_data_frame() %>% 
  ggraph('auto') +
  geom_edge_fan2(aes(color=rev(stat(index))),
                 alpha=0.5,
                 strength = 1.2,lineend = "round") +
  ggraph::geom_node_label(aes(label=name),vjust=0.5,hjust=0.6,size=4) +
  ggraph::scale_edge_color_viridis(name="Index") +
  ggraph::scale_edge_alpha_continuous(name="Index") +
  ggraph::theme_graph() +
  labs(title="More frogs are captured in shallow water type",
       subtitle="favourite habitat is pond or reservoir",
       caption = "made with {ggraph}") +
  theme(text=element_text(family="Roboto Condensed"),
        legend.position = c(0,0.4))

network_plot
# ggsave(here::here("data/2022/w31_frogs/container/images/network_plot.png"),dpi=320, width = 8,height = 6)
