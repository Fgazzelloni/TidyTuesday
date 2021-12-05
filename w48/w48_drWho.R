########## DR. WHO ######### TidyTuesday week 48
rm(list=ls())

library(tidyverse)
library("datardis")

directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')

my_df <- directors%>%
  full_join(episodes,by="story_number")%>%
  full_join(writers,by="story_number")

# my_df%>%head

uk_viewers<- my_df$uk_viewers
uk_viewers[is.na(uk_viewers)]<- 0


# https://www.r-graph-gallery.com/59-nifty-graph.html
moxbuller = function(uk_viewers) {   
  u = runif(uk_viewers)   
  v = runif(uk_viewers)   
  x = cos(2*pi*u)*sqrt(-2*log(v))  
  y = sin(2*pi*v)*sqrt(-2*log(u))
  r = list(x=x, y=y)
  return(r) 
}
r = moxbuller(5000) 


# https://github.com/coolbutuseless/threed
library(threed)
camera_to_world <- threed::look_at_matrix(eye = c(4, 2, 5), 
                                          at = c(0, 0, 0))
obj <- threed::mesh3dobj$cube %>%
  transform_by(invert_matrix(camera_to_world)) %>%
  perspective_projection()


plot<-  data.frame(r)%>%
  ggplot(aes(x=x,y=y,z=0))+
  geom_point(shape=".",col="blue")+
  geom_polygon(data=obj,aes(x = x*25, y = y*25, group = zorder,
                            linetype = hidden,  size = hidden), #zorder the drawing order of the elements from back to front
               fill = NA, colour = 'blue', size = 0.7,
               show.legend = F) +
  geom_rect(mapping=aes(xmin=-30,xmax=0.7,ymin=0.2,ymax=0.8),
            fill=NA)+  
  theme_void() +
  theme(plot.background = element_rect(color="black",
                                       fill="black"))
 

library(showtext)

# Import fonts
font_add_google("Luckiest Guy", "Luckiest")
font_add_google("Poiret One", "Poiret")
showtext_auto()
showtext_opts(dpi = 320)


plot2<-my_df%>%
ggplot(aes(x=uk_viewers,y=rating))+
  geom_jitter(aes(color=duration),show.legend = T)+
  geom_smooth()+
  theme_void()+
  labs(x="rating",y="viewers",color="")+
  scale_color_viridis_c()+
  theme(plot.background = element_rect(color=NA,
                                       fill=NA),
        text = element_text(family="Luckiest",color="blue"),
        legend.position = c(0.5,-0.4),
        legend.direction = "horizontal",
        legend.title = element_blank())



my_df%>%
  mutate(year=lubridate::year(first_aired))%>%
  group_by(year)%>%
  summarize(tot_view=sum(uk_viewers))%>%
  arrange(-tot_view)#,rating,director)%>%arrange(-uk_viewers,-rating)

######################################
library(prophet)
m <-prophet(df)
future <- prophet::make_future_dataframe(m, periods=365)
forecast <- predict(m,future)

#### PROPHET PLOT COMPONENTS ###############################################
plot3<-prophet_plot_components(m, forecast)
######################################
trend <- forecast%>%
  ggplot(aes(x=ds,y=trend))+
  geom_line(color="blue")+
  theme_void()+
  labs(x="rating",y="viewers")+
  theme(plot.background = element_rect(color=NA,
                                       fill=NA),
        text = element_text(family="Luckiest",color="blue"),
        axis.text.x = element_text(color="blue",family = "Luckiest"))
######################################
trend
######################################
weekly<- forecast%>%
  mutate(day=lubridate::wday(ds,label = T,abbr = F,
                             week_start = getOption("lubridate.week.start", 7),
                             locale = Sys.getlocale("LC_TIME")),
         .after=ds) %>%
  select(ds,day,weekly,yearly,trend) %>%
  
  ggplot(aes(x=day,y=weekly,group = 1))+
  geom_line(color="blue")+
  #geom_text(aes(label=day),color="blue",family="Luckiest")+
  theme_void()+
  labs(x="rating",y="viewers")+
  theme(plot.background = element_rect(color=NA,
                                       fill=NA),
        text = element_text(family="Luckiest",color="blue"),
        axis.text.x = element_text(color="blue",
                                   family = "Luckiest",
                                   angle=15,size=8))
######################################
weekly # m$changepoints
######################################

library(patchwork)
plot3<-trend/weekly


library(cowplot)
final <- ggdraw()+
  draw_plot(plot)+
  draw_plot(plot2,x = 0.02, y = 0,scale = 0.3)+
  draw_plot(trend,x = -0.4, y = 0.2,scale = 0.25,height = 0.7,width=1.2)+
  draw_plot(weekly,x = -0.4, y = -0.06,scale = 0.25,height = 0.7,width=1.2)+
  draw_text("Dr. WHO - UK Viewers", x=0.4,y=0.93,
            color="blue",size=42,family="Luckiest")+
  draw_text("Favorite months for watching are April, June and September
            While the Years with the highest numbers of views 
            are 2010 and 2008",
            x=0.34,y=0.8,size=18,family="Luckiest",color="blue")+
  draw_text("General trend declined after 2010 
            favorite days of the week for watching are Tuesday to Friday",
            x=0.4,y=0.1,size=18,family="Luckiest",color="blue")+
  draw_text("Trend",x=0.04,y=0.56,angle=90,color="blue",family = "Luckiest")+
  draw_text("Weekly",x=0.05,y=0.3,angle=90,color="blue",family = "Luckiest")+
  draw_text("Ratings",x=0.365,y=0.53,angle=90,color="blue",family = "Luckiest")+
  draw_text("Viewers",x=0.5,y=0.35,angle=0,color="blue",family = "Luckiest")+
  draw_text("Duration",x=0.43,y=0.25,angle=0,color="blue",family = "Luckiest",size=12)+
  draw_text("Nifty UK Viewers density",x=0.94,y=0.73,angle=88,
            color="blue",size=12,
            family="Luckiest")+
  draw_text("#TidyTuesday week48 Dr.Who - DataViz: Federica Gazzelloni", 
            x=0.4,y=0.03,color="blue",size=14,family="Luckiest")+
  draw_image("/Users/federica/Documents/R/R_general_resources/TidyTuesday/TidyTuesday/w48/drwho.png",
             x=-0.14,y=0.17,scale=0.15)+
  draw_image("/Users/federica/Documents/R/R_general_resources/TidyTuesday/TidyTuesday/w48/tardis.jpg",
             x=-0.42,y=0.42,scale=0.12)

ragg::agg_png(here::here("/Users/federica/Documents/R/R_general_resources/TidyTuesday/TidyTuesday/w48/dr_who.png"),
              res = 320, width = 12, height = 8, units = "in")
final
dev.off()










