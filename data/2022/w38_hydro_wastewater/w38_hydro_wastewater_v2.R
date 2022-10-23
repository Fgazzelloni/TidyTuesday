#TidyTuesday 2022 week38 HydroWaste
setwd("~/Documents/R/R_general_resources/TidyTuesday/data/2022/w38_hydro_wastewater")


library(tidyverse)
HydroWASTE_v10 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv')

##########################################
geo_out%>%
  ggplot(aes(x_o,y_o))+
  geom_point(size=0.05) +
  geom_point(data=geo_wwtp,aes(x_w,y_w),size=0.05,color="red")


HydroWASTE_v10%>%
  janitor::clean_names()%>%
  filter(country=="Australia")%>%
  select(lon_wwtp,lat_wwtp,lon_out,lat_out)%>%
  ggplot()+
  geom_point(aes(lon_wwtp,lat_wwtp),size=0.05) +
  geom_point(aes(lon_out,lat_out),size=0.05,color="red")
##########################################
HydroWASTE_v10%>%
  janitor::clean_names()%>%
  filter(country=="Australia")%>%
  select(lon_wwtp,lat_wwtp,lon_out,lat_out)%>%
  ggplot()+
  geom_polygon(data=world,mapping=aes(long,lat,group=group))+
  geom_point(aes(lon_wwtp,lat_wwtp),size=0.05) +
  geom_point(aes(lon_out,lat_out),size=0.05,color="red")+
  geom_segment(aes(x=lon_wwtp,xend=lon_out,
                   y=lat_wwtp,yend=lat_out),color="green")
##########################################
##########################################
world <- map_data("world") %>%
  filter(!region=="Antarctica")


HydroWASTE_v10%>%
  janitor::clean_names()%>%
  #filter(country=="Australia")%>%
  select(lon_wwtp,lat_wwtp,lon_out,lat_out)%>%
  ggplot()+
  geom_polygon(data=world,
               mapping=aes(long,lat,group=group),
               fill="#8c8b8b",color="#706F6F",size=0.1)+
  geom_point(aes(lon_wwtp,lat_wwtp),shape=".",color="#efdaa0") +
  geom_point(aes(lon_out,lat_out),shape=".",color="#003d59")+
  geom_segment(aes(x=lon_wwtp,xend=lon_out,
                   y=lat_wwtp,yend=lat_out),color="#d68a7f") +
  labs(title="Distribution of treated wastewater in global rivers",
       subtitle="route of the discharged effluents along the global river network",
       caption="DataSource: #TidyTuesday 2022 week38 Hydro Wastewater | V2 DataViz: Federica Gazzelloni @fgazzelloni",
       color="Source of\nwaterwaste")+
  ggthemes::theme_map() +
  theme(plot.title = element_text(size=14,face="bold"),
        plot.subtitle = element_text(size=12,face="bold"),
        text=element_text(family="Roboto Condensed",color="#d68a7f"),
        panel.grid = element_line(color="#bfd0dd",size=0.05),
        plot.background = element_rect(fill="#f0f0f0",color="#f0f0f0"),
        panel.background = element_rect(fill="#f0f0f0",color="#f0f0f0"),
        legend.background = element_blank(),
        legend.position = c(-0.05,0.05),
        legend.key = element_rect(size=4))
  
ggsave("w38_hydro_wastewater_v2.png",
       dpi=320,
       width = 6,
       height = 3.5)


