# Week 26 Public Parks

# points for 14 measures across five categories: acreage, investment, amenities, access, and equity.
# OUTLIERS
# PARK INCLUSION CRITERIA


# load libraries --------------
library(tidytuesdayR)
library(tidyverse)
library(tidymodels)
tidymodels_prefer()

library(forcats)

library(DataExplorer)
library(ggthemes)
library(hrbrthemes)
library(viridis)
library(extrafont)

library(showtext)
#font_families_google()
font_add_google("Montserrat","Montserrat")
showtext.auto(enable = FALSE)

library(RColorBrewer)
library(ggwordcloud)

library(patchwork)
library(cowplot)


# load data ------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 26)

parks <- tuesdata$parks


head(parks)

# questions ------------

# - how parks evolved within the years
# - how many parks have a playground/sports amenities/dogpark/splashgrounds/general amenities/restrooms/benches
# - has the parks size changed within the years
# - how has the residents spending evolved
# - what is the rank by cities


# check of the data and tidy manipilation --------------------
names(parks)
str(parks)

plyr::count(parks$year)

# selecting the columns iwth just points as a measure to have omogeneous data, some of them are
# on a 50 max scale and other on a 100 max scale, so if I want to used them all I would need to rescale
# one of the two

  
minneapolis<-parks%>%
  filter(city=="Minneapolis")

missing<-profile_missing(minneapolis)
#View(missing)
kableExtra::kable(skimr::skim(minneapolis)) %>% kableExtra::scroll_box(width = '100%')

str(minneapolis)

minneapolis_long<-minneapolis%>%
  pivot_longer(cols=c(6,8,10),names_to="data",values_to="percent")

minneapolis_long$data2 = factor(minneapolis_long$data, 
                           levels=c("spend_per_resident_data","park_pct_city_data","pct_near_park_data"))

new_labels=c("spend_per_resident_data"="Spending","park_pct_city_data"="Park points","pct_near_park_data"="Neighborhood")

# first plot ----------------------------
first<-ggplot(data=minneapolis_long,aes(x=year,y=percent,group=data2,colour=data2))+
  geom_point(aes(size=total_pct))+
  geom_line()+
  geom_text(aes(label=percent),stat="identity",vjust=-1.5,show.legend = F,size=2)+
  guides(colour = FALSE, size = FALSE)+
  scale_color_viridis(discrete=TRUE)+
  labs(title="\nPark spending, points, neighborhood\n",
       # subtitle="\n2012 - 2020\n",
       caption="",
       colour="",
       # tag="The \nTrust \nfor \nPublic \nLand",
       x=seq(2012,2020,1), y="")+
  facet_wrap(~data2,labeller = labeller(data2=new_labels))+
  theme_fivethirtyeight()+
  theme(axis.text.x = element_text(angle=90,family="Montserrat"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(color="black",family="Montserrat"),
        plot.title = element_text(family="Montserrat",hjust = 0.5,size=12),
        plot.subtitle = element_text(family="Montserrat",hjust = 0.1),
        # plot.tag = element_text(family="Montserrat",size=10),
        # plot.tag.position = c(0.05, 0.9),
        #panel.grid = element_blank(),
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        plot.background = element_rect(color="#a1d99b",fill="#a1d99b"),
        panel.background = element_rect(color="#a1d99b",fill="#a1d99b"),
        strip.text.y  = element_text(family="Montserrat"),
        plot.margin = margin(0,0,0,0,unit="pt"))


#####################

sub_minneapolis <- minneapolis%>%
  filter(year==c(2020,2019,2018))%>%
  select(city,matches(c("year","rank","points")),-total_points)%>%
  mutate(med_park_size_points=med_park_size_points/50*100,
         park_pct_city_points=park_pct_city_points/50*100)%>%
    group_by(year,rank,amenities_points)%>%
   mutate(total= rowMeans(across(where(is.numeric))))%>%
    ungroup()%>%
  rename(c("Basketball"="basketball_points","Dogpark"="dogpark_points",
           "Playground"="playground_points","Recreation and senior centers"="rec_sr_points",
           "Splashground"="splashground_points","Restroom"="restroom_points"))
  
######################  
min=1
max=100

amenities_per_year <-minneapolis%>%
  filter(year==c(2020,2019,2018))%>%
  select(city,matches(c("year","rank","points")),-total_points)%>%
  rename(c("Basketball"="basketball_points","Dogpark"="dogpark_points",
           "Playground"="playground_points","Recreation and senior centers"="rec_sr_points",
           "Splashground"="splashground_points","Restroom"="restroom_points"))%>%
    janitor::clean_names()%>%
  mutate(basketball=basketball*10000/10000,
         dogpark=dogpark*100000/10000,
         playground=playground*10000/10000,
         recreation_and_senior_centers=recreation_and_senior_centers*20000/10000,
         restroom=restroom*100000/10000,
         splashground=splashground*100000/10000)%>%
    #select(8:13)%>%
  pivot_longer(cols=c(8:13),names_to="data_points",values_to="points")%>%
  select(year,data_points,points,amenities_points)%>%
  group_by(data_points)%>%
  mutate(#normalized = ((points-min(points))/(max(points)-min(points)))*100,
         normalized100 = ((points-min)/(max-min)))%>%ungroup()


normalized_per_year_plot <- ggplot(data=amenities_per_year) +
  geom_col(aes(x=data_points,y=normalized100,fill=factor(year),color=factor(year))) + 
  scale_color_viridis(discrete=TRUE,option="G")  +
    guides(color=FALSE)+
    scale_fill_viridis(discrete=TRUE,option="G") +
  scale_x_discrete(label=c("Basketball","Dogpark","Playground","Recreation","Splashground","Restroom"))+
    #annotate("text",label="improvements",x=2019.5,y=700,family="Montserrat")+
    labs(title="Amenities points Normalized100",
         #subtitle=" 2018 - 2020 ",
         caption="",
         fill="",
         color="",
         tag="")+
    theme_fivethirtyeight()+
    theme(axis.ticks.x = element_line(size=2,color="red"),
          axis.text.x = element_text(family="Montserrat",angle=20,size=5),
          axis.text.y = element_text(family="Montserrat"),
          axis.ticks.y = element_blank(),
          plot.title = element_text(family="Montserrat",size=8),
          plot.subtitle = element_text(family="Montserrat",vjust=-0.5,hjust=0.3,size=10),
          plot.caption = element_text(family="Montserrat"),
          plot.tag = element_text(family="Montserrat",size=10),
          plot.tag.position = c(0.08, 0.9),
          plot.background = element_rect(color="#a1d99b",fill="#a1d99b"),
          panel.background = element_rect(color="#a1d99b",fill="#a1d99b"),
          legend.title = element_text(family="Montserrat",hjust=0),
          legend.background = element_blank(),
          legend.position = "top",
          legend.key.size = unit(0.3, 'cm'),
          legend.text = element_text(family="Montserrat",size=8),
          plot.margin = margin(30,30,30,30,unit="pt"))

##########################   

# sub_minneapolis%>%pivot_longer(cols=c(8:13),names_to="data_points",values_to="points")
  
# Level of Minneapolis park feautures  
amenities_per_year_plot <- ggplot(data=amenities_per_year,aes(x=year,y=points)) +
  geom_col(aes(color=data_points,fill=data_points)) +
  geom_smooth(aes(y = amenities_points),color="red",method="lm",formula = y ~ splines::bs(x, 5)) +
  scale_color_viridis(discrete=TRUE)  +
  guides(color=FALSE)+
  scale_fill_viridis(discrete=TRUE,label=c("Basketball","Dogpark","Playground","Recreation","Splashground","Restroom")) +
  labs(title="Amenities points by year",
       #subtitle="three years of points increase",
       caption="Viz @fgazzelloni DataSource: TidyTuesday week26 Public Park Access,TPL,CityLab",
       fill="",
       color=""
       #tag="Amenities points 2018-2020"
       )+
  theme_fivethirtyeight() +
  theme(axis.ticks.x = element_line(size=2,color="red"),
        axis.text.x = element_text(family="Montserrat"),
        axis.text.y = element_text(family="Montserrat"),
        axis.ticks.y = element_blank(),
        plot.title = element_text(family="Montserrat",size=8),
        plot.subtitle = element_text(family="Montserrat",vjust=-0.5,hjust=0.3,size=10),
        plot.caption = element_text(family="Montserrat",face="bold"),
        plot.tag = element_text(family="Montserrat",size=10),
        plot.tag.position = c(0.08, 0.9),
        plot.background = element_rect(color="#a1d99b",fill="#a1d99b"),
        panel.background = element_rect(color="#a1d99b",fill="#a1d99b"),
        legend.title = element_text(family="Montserrat",hjust=0),
        legend.background = element_blank(),
        legend.text = element_text(family="Montserrat",size=8),
        legend.key.size = unit(0.3, 'cm'),
        legend.position = "top",
        plot.margin = margin(30,30,30,30,unit="pt"))
  
second <- normalized_per_year_plot | amenities_per_year_plot


third<- ggplot()+
  geom_blank() +
  labs(title="Minneapolis")+
  theme_fivethirtyeight()+
  theme(plot.background = element_rect(color="#a1d99b",fill="#a1d99b"),
        panel.background = element_rect(color="#a1d99b",fill="#a1d99b"),
        plot.title = element_text(family="Montserrat",color="yellow"))

fourth <- parks%>%
  filter(rank<=10)%>%
  count(city)%>%
  ggplot(aes(label=city,color=city))+
geom_text_wordcloud(family="Montserrat") +
  labs(title="Minneapolis ranked the first best 7 times between 2012 - 2020",
       tag="The \nTrust \nfor \nPublic \nLand")+
  theme_fivethirtyeight()+
  theme(plot.background = element_rect(color="#a1d99b",fill="#a1d99b"),
        panel.background = element_rect(color="#a1d99b",fill="#a1d99b"),
        plot.title = element_text(family="Montserrat",size=12,hjust=0.5),
        plot.tag = element_text(family="Montserrat",size=10,color="white"),
        plot.tag.position = c(0.04, 0.9))
 
library(magick)
library(ggimage)



img = "minneapolis_park.png"
third<-ggbackground(third, img,alpha=.9)

one <-third/first 

two<-fourth/second

final <-one|two

bck_color <- "#a1d99b"

final <- final + plot_annotation(
  title = "\nMinneapolis Park improvment racing\n",
  subtitle="'The Trust for Public Land creates parks and protects land for people, ensuring healthy, livable communities for generations to come' ",
  theme = theme(
    plot.margin = margin(10,10,10,10),
    plot.background = element_rect(fill = bck_color, color = NA),
    panel.background = element_rect(color="#a1d99b",fill="#a1d99b"),
    plot.title = element_text(family = "Montserrat",vjust=-0.5,face="bold"),
    plot.subtitle = element_text(family = "Montserrat"),
    plot.caption = element_text(family = "Montserrat", size = 9, color = bck_color, 
                                margin = margin(15,0,0,0), hjust = 0.95)
  )
)

final_plot <- stamp(final,label="#TidyTuesday week 26",color="red",alpha = 1,
      vjust = 1.8,
      hjust = 1.2,
      size = 14,
      family = "Montserrat",
      fontface = "bold",
      clip = "on")



###################### SAVING ############################


ragg::agg_png(here::here("w26","w26_parks.png"),
              res = 320, width = 14, height = 8, units = "in")
final_plot

dev.off()



##################################################





















