
# Week 25 

# DataSource:
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-06-15/readme.md

# Dubois Style
# https://github.com/ajstarks/dubois-data-portraits/blob/master/dubois-style.pdf

# Add fonts
# https://cran.rstudio.com/web/packages/showtext/vignettes/introduction.html
# https://fonts.google.com/specimen/Shadows+Into+Light#standard-styles


# inspirations
# https://github.com/ivoruaro/tidytuesday/blob/main/2021w25.R
# https://github.com/nrennie/tidytuesday/blob/main/2021/15-06-2021/15062021.R
# https://gist.github.com/clauswilke/783e1a8ee3233775c9c3b8bfe531e28a


# image background
# https://www.pngwing.com/en/free-png-zfvci


# load libraries ------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(DataExplorer)
library(ggtext)
library(DataEditR)
library(tidyquant)

library(RColorBrewer)

library(maps)
library(rnaturalearth)
library(sp)
library(sf)

library(extrafont)
#library(ggrepel)

#library(viridis)
library(hrbrthemes)

library(ggimage)
library(patchwork)
library(cowplot)

library(ggpattern)


# add font -------------------------------------------------
library(showtext)
font_add_google("Shadows Into Light","shadow_into_light")
font_add_google("Schoolbell", "bell")
showtext_auto(enable = TRUE)

# load data ----------------------------------------
tuesdata <- tidytuesdayR::tt_load('2021-06-15')

tweets <- tuesdata$tweets

# check data -----------------------
head(tweets)

profile_missing(tweets)

# tidying -----------------------------------
df_tweets<-tweets%>%
  arrange(-like_count,-followers)%>%
  drop_na()

plyr::count(df_tweets$location)

# load map data ------------------------------

# world data full 
world_full <- ne_countries(scale = "medium", returnclass = "sf")
world_data <- filter(world_full, continent != "Antarctica")

# world lat&long
world<-map_data(map = "world") 

my_world_data<- world%>%
  full_join(world_data, by = c("region"="name"))%>%
  select(long,lat,group,order,region,region_wb)

# states lat&long
states <- map_data("state")

# choose palette colors ----------------------
# dubois palette taken from Dubois Style webpage
dubois_pal <- c("black" = "#000000", 
                "brown" = "#654321", 
                "tan" = "#d2b48c",
                "gold" = "#ffd700",
                "pink"="#ffc0cb",
                "red"="#dc143c",
                "green"="#00aa00",
                "blue"="#4682b4")

palette<-c("#000000","#654321","#d2b48c","#ffd700","#ffc0cb","#dc143c","#00aa00","#4682b4")

states_palette<-colorRampPalette(c("#654321","#d2b48c","#ffd700","#ffc0cb","#dc143c","#00aa00","#4682b4"))(15537)

pal_west <- c("#000000", "#4682b4")
pal_est <- c("#000000", "#4682b4")

pal_fill<-c("background"="#e8d8c3",
            "water"="#d9c09e",
            "europe"="#ffd700",
            "canada"="#654921",
            "southAmerica"="#1b1209")


# make the plot ---------------------

world_west <-ggplot() +
  geom_polygon(data=my_world_data,aes(x=long,y=lat,group=group,fill=region_wb)) + scale_fill_manual(values=palette) + guides(fill=FALSE)+
  
  geom_polygon(data = states,aes(x = long, y = lat, group = group),fill=states_palette,color="#000000",size=0.3)+#scale_fill_manual(values=states_palette) +guides(fill=FALSE)+
  
  geom_path(data=world,aes(x=long,y=lat,group=group),size=0.3) +
  
  geom_point(data = df_tweets,mapping=aes(x=long, y=lat, size=followers, shape=verified,color=verified),alpha=0.7) +
  
  geom_path(data = df_tweets,mapping=aes(x=long, y=lat,group=location),size=0.3,color="#654321") + 
  
  coord_map("ortho", orientation = c(3.849945, -103.525750, 0)) + 
  
  #geom_text(data = df_tweets,aes(x=long, y=lat, group=location, label=location), check_overlap = TRUE,color="#654321",size = 3, hjust=0.5, vjust=-1)+
  
  #geom_curve(data = df_tweets,aes(x=long, y=lat, group=location, label=location))+
  
 # annotate("text", x = 0, y = 0,family= "shadow_into_light",label="World map of ",color = "#ffc0cb", size = 4,fontface="bold") +
  
  scale_size(guide=FALSE, range = c(1,9)) +
  scale_shape(guide=FALSE) +
  scale_color_manual(values = pal_west, aesthetics = c("colour")) +
  labs(x="",y="",color="Verified accounts") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#e8d8c3", colour = "#e8d8c3"),#   element_rect(color="#e8d8c3",fill="#e8d8c3"),
        panel.background = element_rect(color="#e8d8c3",fill="#d9c09e"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(family="shadow_into_light"),
        legend.title = element_text(family="shadow_into_light"))

# world_west



world_est <-ggplot() +
  geom_polygon(data=my_world_data,aes(x=long,y=lat,group=group,fill=region_wb)) + scale_fill_manual(values=palette) + guides(fill=FALSE)+
  
  geom_polygon(data = states,aes(x = long, y = lat, group = group),fill=states_palette,color="#000000",size=0.3)+#scale_fill_manual(values=states_palette) +guides(fill=FALSE)+
  
  geom_path(data=world,aes(x=long,y=lat,group=group),size=0.3) +
  
  geom_point(data = df_tweets,mapping=aes(x=long, y=lat, size=followers, shape=verified,color=verified),alpha=0.7) +
  
  geom_path(data = df_tweets,mapping=aes(x=long, y=lat,group=location),size=0.3,color="#654321") + 
  
  coord_map("ortho", orientation = c(19.982182, 46.595135, 0)) +
  
  #geom_text(data = df_tweets,aes(x=long, y=lat, group=location, label=location), check_overlap = TRUE,color="#654321",size = 3, hjust=0, vjust=-1)+
  
  #geom_curve(data = df_tweets,aes(x=long, y=lat, group=location, label=location))+
  
  #annotate("text", x = 0, y = 0,family= "shadow_into_light",label="Tweets sized by followers",color = "#ffc0cb", size = 4,fontface="bold") +
 
  scale_size(guide=FALSE, range = c(1,9)) +
  scale_shape(guide=FALSE)+
  scale_color_manual(values = pal_est,aesthetics = c("colour"))+ #, "fill")) +
  labs(caption="Viz @fgazzelloni DataSource: WEB Du Bois and Juneteenth	#DuBoisChallenge tweets	The Intercept #TidyTuesday 2021/25",
       x="",y="",color="") +
  theme_void() +
  theme(plot.background = element_rect(fill = "#e8d8c3", colour = "#e8d8c3"),
        panel.background = element_rect(color="#e8d8c3",fill="#d9c09e"),
        plot.caption = element_text(color="#00aa00",face="bold",family="shadow_into_light"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none")

# world_est


main_plot <- (world_west + world_est)

plot <-plot_grid(main_plot)

final <- plot +
  theme(plot.background = element_rect(fill = "#e8d8c3", colour = "#e8d8c3"),
        plot.margin = margin(t=0,r=1,b=0,l=1,unit="pt"))


title<- ggplot()+
  labs(title="\n\n#DuBoisChalllenge 2021 Twitter Metrics",
       subtitle="Tweets by number of followers (verified or not) and by selected locations\n\nINSPIRED BY:\n",
       tag="\nTHE GEORGIA NEGRO \nA SOCIAL STUDY \nby W.E. BURGHARDT\n\n") +
  theme_void()+
  theme(plot.background = element_rect(fill = "#e8d8c3", colour = "#e8d8c3"),# element_rect(color="#e8d8c3",fill="#e8d8c3"),
        panel.background = element_rect(color="#e8d8c3",fill="#e8d8c3"),
        plot.title = element_text(color="#000000",size=20,face="bold",family="bell",hjust = 0.5,vjust = 0),
        plot.subtitle = element_text(color="#000000",size=12,face="bold",family="bell",vjust = 0,hjust = 0.5),
        plot.tag = element_text(color="#000000",size=12,face="bold"),
        plot.tag.position = "bottom",
        plot.margin = margin(t=0,r=1,b=1,l=1,unit="pt"))

caption<- ggplot()+
  labs(title="ROUTES OF THE TWEETS BY LOCATIONS",
       subtitle="THIS CASE IS DEVOTED TO A SERIES OF CHARTS, MAPS AND OTHER DEVICES DESIGNED TO ILLUSTRATE \nTHE DEVELOPMENT OF THE AMERICA NEGRO IN A SIGLE TYPICAL STATE OF THE UNITED STATES",
       tag="THE PROBLEM OF THE 20^th CENTURY IS THE PROBLEM OF THE COLOR-LINE\n") +
  theme_void()+
  theme(plot.background = element_rect(fill = "#e8d8c3", colour = "#e8d8c3"),# element_rect(color="#e8d8c3",fill="#e8d8c3"),
        panel.background = element_rect(color="#e8d8c3",fill="#e8d8c3"),
        plot.title = element_text(color="#000000",size=11,face="bold",family="bell",hjust=0.5),
        plot.subtitle = element_text(color="#000000",size=10,face="bold",family="bell",vjust = -1,hjust = 0.5),
        plot.tag = element_text(color="#000000",size=8,face="bold"),
        plot.tag.position = "bottom",
        plot.margin = margin(t=0,r=1,b=0,l=1,unit="pt"))
  

        
final_plot<-plot_grid(title,final,caption,ncol=1,rel_heights = c(0.4,1,0.15))




###################### SAVING ############################


ragg::agg_png(here::here("w25","w25_tweets.png"),
              res = 320, width = 14, height = 8, units = "in")
final_plot

dev.off()



#### ATTACHING LOGO ############################
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


final_plot <- image_read("w25/w25_tweets.png")

attached_logo <- image_composite(final_plot, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "w25/w25_tweets.png",
            format = "png") # save final plot



##############################################################







