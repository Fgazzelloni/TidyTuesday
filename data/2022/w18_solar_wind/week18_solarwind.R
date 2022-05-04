
library(tidyverse)


wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')



cum_solar <- solar%>% # DataExplorer::profile_missing()
  group_by(date)%>%
  summarize(tot_solar_mwh=sum(solar_mwh),
            tot_solar_capacity=sum(solar_capacity),.groups="drop")%>%
  ungroup() %>%
  mutate(cum_solar_mwh=cumsum(tot_solar_mwh)) 


cum_wind <- wind%>% # DataExplorer::profile_missing()
  group_by(date)%>%
  summarize(tot_wind_mwh=sum(wind_mwh),
            tot_wind_capacity=sum(wind_capacity),.groups="drop")%>%
  ungroup() %>%
  mutate(cum_wind_mwh=cumsum(tot_wind_mwh)) 


cum_solar%>%#summary()
  ggplot(aes(x=date,y=tot_solar_mwh))+
  geomtextpath::geom_textline(aes(y=cum_solar_mwh),col="red",label="Solar",hjust=1,size=6,family = "sans") +
  geomtextpath::geom_textline(data=cum_wind, aes(y=cum_wind_mwh),inherit.aes = T,label="Wind",hjust=1,size=6,family = "sans")+
  labs(title="Projected cumulative price",y="Price ($/MWh)",x="",
       caption="#TidyTuesday w18 Solar/Wind | DataSource: Berkeley Lab\nPrices are in $/MWh from 2009 to 2021 | DataViz: Federica Gazzelloni (@fgazzelloni)") +
  ggthemes::theme_wsj()+
  theme(panel.grid = element_line(size=0.3),
        plot.caption = element_text(size=8,hjust=0))

ggsave("w18_solar_wind.png",
       dpi=320,
       width = 8,
       height = 5)