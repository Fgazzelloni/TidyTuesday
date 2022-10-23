
# TidyTuesday week 6 2022
# original Dubois collection:
# https://www.loc.gov/collections/african-american-photographs-1900-paris-exposition/?sb=shelf-id_desc&sp=1&st=grid

# load data------------
library(tidyverse)
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')


# data wrangling--------
df <-airmen%>%
  select(name,graduation_date,graduated_from,
         pilot_type,
         military_hometown_of_record,state,
         number_of_aerial_victory_credits)%>%
  mutate(pilot_type=str_replace_all(pilot_type,"Liaison","Liason"))%>%
  filter(!is.na(graduation_date),!is.na(state))%>%
  arrange(desc(graduation_date))%>%
  mutate(id=row_number())%>%
  relocate(id)

# 48 states abbr to be chacked
df%>%count(state)

df%>%mutate(year=lubridate::year(graduation_date))%>%count(year)

# map data -----
us_states_df <-
  df%>%
  mutate(state_name=usdata::abbr2state(state),
         .after=state)%>% 
  filter(!is.na(state_name)) %>%   # 41 us states selected
  mutate(state_name = tolower(state_name)) 

my_states<-
  us_states_df%>%
  count(state_name)%>%
  select(-n)%>%unlist()

# US states coordinates 
states<- map_data("state")

my_states_coords<- 
  states%>%
  filter(region%in%my_states)%>%
  left_join(us_states_df,by=c("region"="state_name"))%>%
  mutate(number_of_aerial_victory_credits=ifelse(number_of_aerial_victory_credits==1.5,
                                                 2,number_of_aerial_victory_credits))

# World coordinates
world<-map_data(map = "world") %>%
  filter(!region=="Antarctica")


# alabama $ french morocco
al_frmo<-data.frame(region=c("french morocco","alabama"),
                    lat=c(30.427755,32.318230),
                    long=c(-9.598107,-86.902298))



# dubois colors -------
states_palette<-colorRampPalette(c("#654321","#d2b48c","#ffd700","#ffc0cb","#dc143c","#00aa00","#4682b4"))(5)

# fonts---------
library(extrafont)
library(showtext)
showtext::showtext_auto()
showtext::showtext_opts(dpi=320)
library(sysfonts)
#font_families_google()
font_add_google(name="Barlow Condensed",family="dubois")


# make the map plots ---------------------

world_west <-
  ggplot() +
  geom_polygon(data=world,aes(x=long,y=lat,group=group),
               fill="wheat2",color="#654321") +
  geom_polygon(data = states,
               aes(x = long, y = lat, group = group),
               fill="wheat2",color="#654321",size=0.1) +
    
  geom_polygon(data = subset(my_states_coords,number_of_aerial_victory_credits==1),
                 aes(x = long, y = lat, group = group,
                     fill=factor(number_of_aerial_victory_credits)),
                 color="#654321",size=0.05) +
   
   geom_polygon(data = subset(my_states_coords,number_of_aerial_victory_credits==2),
                 aes(x = long, y = lat, group = group,
                     fill=factor(number_of_aerial_victory_credits)),
                 color="#654321",size=0.05) +
    
  geom_polygon(data = subset(my_states_coords,number_of_aerial_victory_credits==3),
                 aes(x = long, y = lat, group = group,
                     fill=factor(number_of_aerial_victory_credits)),
                 color="#654321",size=0.05)+  
  geom_polygon(data = subset(my_states_coords,number_of_aerial_victory_credits==4),
                 aes(x = long, y = lat, group = group,
                     fill=factor(number_of_aerial_victory_credits)),
                 color="#654321",size=0.05)+
  
  geom_point(data=al_frmo,aes(x=long,y=lat),color="red")+
  
  scale_fill_manual(values=states_palette)+
  coord_map("ortho", orientation = c(3.849945, -103.525750, 0)) +

  labs(title=" \n \n ",
       subtile=" \n \n ",
       x="",y="",fill="Aerial victories") +
  theme_void() +
  theme(legend.position = c(0.5,1.2),
        legend.direction = "horizontal")


world_est <-  
  ggplot() +
  geom_polygon(data=world,aes(x=long,y=lat,group=group),
               fill="wheat2",color="#654321") +
  geom_polygon(data = states,
               aes(x = long, y = lat, group = group),
               fill="wheat2",color="#654321",size=0.1) +
  geom_polygon(data = subset(my_states_coords,number_of_aerial_victory_credits==1),
               aes(x = long, y = lat, group = group,
                   fill=factor(number_of_aerial_victory_credits)),
               color="#654321",size=0.05)+
  geom_polygon(data = subset(my_states_coords,number_of_aerial_victory_credits==2),
               aes(x = long, y = lat, group = group,
                   fill=factor(number_of_aerial_victory_credits)),
               color="#654321",size=0.05)+
  geom_polygon(data = subset(my_states_coords,number_of_aerial_victory_credits==3),
               aes(x = long, y = lat, group = group,
                   fill=factor(number_of_aerial_victory_credits)),
               color="#654321",size=0.05)+  
  geom_polygon(data = subset(my_states_coords,number_of_aerial_victory_credits==4),
               aes(x = long, y = lat, group = group,
                   fill=factor(number_of_aerial_victory_credits)),
               color="#654321",size=0.05) + 

  geom_point(data=al_frmo,aes(x=long,y=lat),color="red")+
  
  scale_fill_manual(values=states_palette)+
  coord_map("ortho", orientation = c(10.050474, 55.740732,0)) + #9.982182, 49.595135, 0)) + 
  labs(title=" \n \n ",subtile=" \n \n ",x="",y="",color="") +
  theme_void() +
  theme(legend.position = "none")



# draw curved text-----------------------
t<-2*pi

df_text_left <- 
  data.frame(x = cos(t),
             y = sin(t),
             xend = cos(t + 1.8),
             yend = sin(t + 1.8))

p <-ggplot(df_text_left) +
  geomtextpath::geom_textcurve(aes(x, y, xend = xend, yend = yend),
                               label = c("Afican-American pilots victory"),
                               curvature = 0.2, vjust = 1) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.2))


q <-ggplot(df_text_left) +
  geomtextpath::geom_textcurve(aes(-x, y, xend =- xend, yend = yend),
                               label = c("Army Air Force during WWII"),
                               curvature = -0.2, vjust = -0) +
  coord_equal(xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.2))

library(patchwork)
text_curved <-(p+q)
text_curved <- text_curved & theme_void()

# draw circle and a curve-----------
g <- grid::circleGrob(gp = grid::gpar(fill = "#baa388",color="#654321",size=0.5))


# final touches----------
library(cowplot)
final_plot<-ggdraw()+
  draw_image("data/2022/w6_airforce/duboisbg.jpeg",
            scale = 1.6)+
  draw_grob(g, scale=0.542,x=-0.223,y=-0.1) +
  draw_grob(g, scale=0.542,x=0.229,y=-0.1) +
  draw_plot(world_west,scale=0.6,x=-0.229,y=-0.07)+
  draw_plot(world_est,scale=0.6,x=0.229,y=-0.07) +
  draw_image("https://tile.loc.gov/storage-services/service/pnp/ppmsca/33800/33863r.jpg",
             scale=0.3,x=0.4,y=0.35) +
  draw_label("Tuskegee Airmen: Aerial Victories\nAir Force Historical Research Agency\nGraduation 1942 - 1948", 
             fontfamily="dubois",color="#291c10",
             x=0.5,y=0.9,size=25,fontface = "bold")+
  draw_label("DataSource: Tuskegee Airmen | Viz @fgazzelloni \n #DuBoisChallenge | #TidyTuesday 2022/06", 
             fontfamily="dubois",color="#291c10",
             x=0.5,y=0.12,size=14,fontface = "bold")+
  draw_plot(text_curved,x=0,y=0.05,scale = 0.5)+
  draw_label("This paper focus on their aerial victory credits. 
             The most famous of the 332d Fighter Group commanders was Col.Benjamin O. Davis,Jr.
             The 99th Fighter Squadron deployed from Tuskegee, Alabama, to French Morocco in April 1943.",
             fontfamily="dubois",color="#291c10",
             x=0.5,y=0.04,size=14,fontface = "bold")
  

# save the plot--------
ragg::agg_png("data/2022/w6_airforce/w6_airforce6.png",
              res = 320, width = 12, height = 10, units = "in")
final_plot

dev.off()

