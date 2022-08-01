# TidyTuesday week49 Cricket
# author: Federica Gazzelloni
rm(list=ls())
# libraries----------
library(tidyverse)
library(sf)
library(extrafont)
loadfonts()
library(xkcd)

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv')

# matches%>%View

# data wrangling------------
# 113 ground, 109 ground_city, 21 ground_country
matches <- matches%>% # count(ground,ground_city,ground_country)%>%View
  mutate(date=lubridate::mdy(match_date))%>%
  filter(!date==is.na(date)) %>% 
  mutate(points=ifelse(winner==team1,score_team1,score_team2),
         year=lubridate::year(date))

# exploratory data analysis----------
# points plot
points_yr<-matches %>% # count(date,winner)
  ggplot(aes(x=(date),y=points))+
  geom_point(aes(color=factor(year)),show.legend = F,shape=".")+
  scale_x_date(date_breaks = "1 year", date_labels = "%Y")+
  geom_smooth(size=0.2)+
  theme_xkcd()+
  labs(x="Years",y="Points rating",
       title="Points by Years",
       caption = "Observed Values")+
  theme(text = element_text(size=12),
        panel.grid.minor.x = element_line(size=6,color="darkseagreen3"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size=14),
        axis.line = element_line(),
        axis.text.x = element_text(size=7,angle = 15))

# time series | for extra plots------------
# model sesonality 
pr_df<- matches%>%select(ds=date,y=points)
library(prophet)
m <-prophet(pr_df)
future <- prophet::make_future_dataframe(m, periods=365)
forecast <- predict(m,future)
# tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

forecast%>%
  mutate(ds=as.Date(ds,"%Y-%m-%d"))%>%
  ggplot(aes(ds,yhat))+
  geom_point(data=pr_df,aes(x=ds,y=y),size=0.05)+ # original data
  geom_line(aes(x=ds,y=yhat_upper),size=0.03)+
  geom_line(aes(x=ds,y=yhat_lower),size=0.03)+
  geom_line(col="violet",size=0.5)+
  geom_smooth()

prophet_plot_components(m, forecast)

# week_trend plot
week_trend<- forecast%>%
  mutate(day=lubridate::wday(ds,label = T,abbr = T,
                             week_start = getOption("lubridate.week.start", 7),
                             locale = Sys.getlocale("LC_TIME")),
         .after=ds) %>%
  select(ds,day,weekly,yearly,trend) %>%
  
  ggplot(aes(x=day,y=weekly,group = 1))+
  geom_line(color="black")+
  theme_xkcd()+
  labs(x="Weekdays",y="Points rating",
       title="Days of the week with higher points",
       caption = "Prophet Time Series")+
  theme(text = element_text(size=12),
        plot.background = element_blank(),
        plot.title = element_text(size=14),
        axis.line = element_line(),
        axis.text.x = element_text(size=8))


# flags----------
# add country code
library(ggflags)
library(countrycode)
#countrycode::codelist_panel%>%View
countrycode::codelist%>%filter(str_detect(country.name.en,"United"))
# countrycode::countrycode(matches)
library(maps)

matches%>%
  select(date,year,winner,points,ground,ground_city,ground_country,match_date)%>%
  mutate(country_code = countrycode(ground_country, 
                                    origin = 'country.name', 
                                    destination = 'iso2c'),
         country_code=tolower(country_code))

# not matched unambiguously England, Midlothian, Wales, West Indies
# England, Midlothian, Wales == United Kingdom
# West Indies ?
ambiguous_values <- matches%>%
  select(ground,ground_city,ground_country)%>%
  filter(ground_country=="West Indies")%>%
  count(ground,ground_city,ground_country)

my_missing_cities<- ambiguous_values%>%
  pull(ground_city)%>%
  unlist()


flags_df <- matches%>% #count(ground_country)
  select(date,year,winner,points,ground,ground_city,ground_country,match_date)%>%
  mutate(country=case_when(ground_country=="England"~"United Kingdom",
                                  ground_country=="Midlothian"~"United Kingdom",
                                  ground_country=="Wales"~"United Kingdom",
                                  TRUE~ground_country))%>%
  mutate(country=case_when(ground_city=="Bridgetown"~"Barbados",
                           ground_city=="Gros Islet"~"Saint Lucia",
                           ground_city=="Kingston"~"Jamaica",
                           ground_city=="Port of Spain"~"Trinidad and Tobago",
                           ground_city=="Arnos Vale"~"Saint Vincent e Grenadine",
                           ground_city=="St John's"~"Antigua e Barbuda",
                           TRUE~country))%>%
  mutate(country_code = countrycode(country, 
                                    origin = 'country.name', 
                                    destination = 'iso2c'),
         country_code=tolower(country_code),.after=winner)


# ground country winners dataset--------
world1_geo<- rnaturalearth::ne_countries(scale=110,
                                         returnclass = "sf")


world1_geo <- world1_geo%>%filter(!name==c("Antarctica","Fr. S. Antarctic Lands"))

polygon_df <- matches%>%
  left_join(world1_geo,by=c("ground_country"="name"))%>%
  select(date,year,points,winner,ground_country,geometry)

# centroids and coords with spData::world-----
world2_geo<- spData::world
world2_ctr<- st_centroid(world2_geo)
world2_ctr_coords<- st_coordinates(world2_ctr)%>%
  as.data.frame()

countries <- matches$ground_country

world2_ctr_coords_my_countries<-cbind(world2_ctr_coords,ground_country=world2_geo$name_long)%>%
  filter(ground_country%in%countries)%>%
  left_join(polygon_df%>%select(year,ground_country,winner,points),by="ground_country")

# flags data sets
flags_df_coords<- flags_df%>%
  inner_join(world2_ctr_coords_my_countries,by=c("ground_country","year","winner","points"))

# this set will be used
flags_df_coords2<- flags_df_coords%>%count(country_code,X,Y)



# map plot----------------
  # world polygons
map_plot <- ggplot(world2_ctr_coords_my_countries)+
  geom_sf(data=world1_geo,
          aes(geometry=geometry),
          fill="#f0ebc7",size=0.2) +
  # my polygons
  geom_sf(data=polygon_df,
          aes(geometry=geometry),color="red",
          fill="#d9ed53",
          show.legend = F) +
  # polygons centroids
  geom_point(data=world2_ctr_coords,
             aes(x=X,y=Y),shape=".") +
  # my polygons centroids
  geom_point(data=world2_ctr_coords_my_countries,
             aes(x=X,y=Y),size=0.3,color="red") +
  # my country names
  geom_text(data=world2_ctr_coords_my_countries%>%  #count(ground_country)
             filter(!ground_country%in%c("Ireland",
                                      "United Arab Emirates",
                                      "Bangladesh")),
            aes(x=X,y=Y,label=ground_country),
            check_overlap = F,
            vjust=-1.5,hjust=0.5,family="Roboto Condensed") +
  # flags
  ggflags::geom_flag(data=flags_df_coords2,
                     aes(x=X,y=Y,country=country_code), size=4.5) +
  coord_sf()+
  ggthemes::theme_map() +
  theme(text = element_text(family="Roboto Condensed"),
        plot.background = element_blank(),
        panel.background = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face="bold"))

# green background plot------------
backround_plot<- ggplot()+
  geom_blank()+
  ggthemes::theme_map() +
  theme(plot.background = element_rect(fill="darkseagreen3",color="#42f59b"),
        panel.background = element_rect(fill="darkseagreen3",color="#42f59b"))
  

# draw plot------------
library(cowplot)
final<- ggdraw()+
  #plot background
  draw_image("R_general_resources/TidyTuesday/TidyTuesday/w49_world_cup_cricket/ball.jpg",
             x=0,y=0,scale=1)+
  draw_image("R_general_resources/TidyTuesday/TidyTuesday/w49_world_cup_cricket/ball.jpg",
             x=0.4,y=0.4,scale=0.2,width = 1,height = 0.98)+
  draw_plot(backround_plot,width=1,heigh=0.75,x=0,y=0.1) +
  # main playing area
  draw_line(x=c(0.05,0.95),y=c(0.5,0.5),
            size=90,color="#a8e657",alpha=0.4)+
  # map with extra labels
  draw_plot(map_plot) +
  draw_label("Ireland",x=0.45,y=0.68,size=10,color="black")+
  draw_label("United Arab Emirates",x=0.57,y=0.58,size=10,color="black")+
  draw_label("Bangladesh",x=0.7,y=0.57,size=10,color="black")+
  # red lines 
  draw_line(x=c(0.05,0.05),y=c(0.33,0.665),
            size=0.2,color="red",alpha=1)+
  draw_line(x=c(0.95,0.95),y=c(0.33,0.665),
            size=0.2,color="red",alpha=1)+
  # crease: popping crease
  draw_label("Popping Crease",x=0.19,y=0.28,size=12)+
  draw_line(x=c(0.19,0.18),y=c(0.29,0.37),size=0.4,color="red")+
  draw_line(x=c(0.18,0.18),y=c(0.38,0.635),
            size=0.5,color="white",alpha=1)+
  draw_line(x=c(0.82,0.82),y=c(0.38,0.635),
            size=0.5,color="white",alpha=1)+
  # crease: return crease
  draw_label("Return Crease",x=0.138,y=0.32,size=12)+
  draw_line(x=c(0.17,0.15),y=c(0.33,0.42),size=0.4,color="red")+
  draw_line(x=c(0.05,0.18),y=c(0.42,0.42),
            size=0.5,color="white",alpha=1)+
  draw_line(x=c(0.82,0.95),y=c(0.42,0.42),
            size=0.5,color="white",alpha=1)+
  draw_line(x=c(0.05,0.18),y=c(0.6,0.6),
            size=0.5,color="white",alpha=1)+
  draw_line(x=c(0.82,0.95),y=c(0.6,0.6),
            size=0.5,color="white",alpha=1)+
  # crease: bowling crease
  draw_label("Bowling Crease",x=0.126,y=0.62,size=12)+
  draw_line(x=c(0.13,0.15),y=c(0.61,0.58),size=0.4,color="red")+
  draw_line(x=c(0.15,0.15),y=c(0.42,0.6),
            size=0.5,color="white",alpha=1)+
  draw_line(x=c(0.85,0.85),y=c(0.42,0.6),
            size=0.5,color="white",alpha=1) + 
  draw_plot(week_trend,x=0.25,y=-0.3,scale=0.2,width=1,heigh=1)+
  draw_plot(points_yr,x=0.01,y=-0.3,scale=0.2,width=1,heigh=1)+
  # title & caption
  draw_label("Cricket Ground Country Winners \n1996-2005",
             x=0.4,y=0.93,size=34,fontfamily="Impact",
             fontface="bold")+
  draw_label("Datasource: World Cup Cricket | ESPN Cricinfo\n#TidyTuesday w49\nInfographics: Federica Gazzelloni",
             x=0.82,y=0.05,size=12,fontfamily="Impact",
             fontface="plain")


# save the plot---------
ragg::agg_png(here::here("TidyTuesday/w49/cricket.png"),
              res = 320, width = 12, height = 8, units = "in")
final
dev.off()


