# Week 20 US Broadband
# source of data:
# https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-05-11/readme.md
# https://github.com/microsoft/USBroadbandUsagePercentages

# load libraries ------------------------------------
library(tidytuesdayR)
library(tidyverse)
library(DataExplorer)

library(zipcodeR)
library(janitor)
library(stringr)


# load data -----------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 20)

broadband <- tuesdata$broadband
broadband_zipcode <- tuesdata$broadband_zip

#broadband_zipcode<-read.csv("https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/master/dataset/broadband_data_zipcode.csv")
# contains mean absolute error (MAE), mean signed deviation (MSD)

# check missing data and str------------------------------
profile_missing(broadband);head(broadband)
profile_missing(broadband_zipcode);head(broadband_zipcode)

str(broadband);dim(broadband)
str(broadband_zipcode);dim(broadband_zipcode)


# data wrangling -------------------------------
names(broadband)<-make.names(tolower(names(broadband)))
names(broadband_zipcode)<-make.names(tolower(names(broadband_zipcode)))

broadband%>%filter(str_detect("-",broadband$broadband.availability.per.fcc))
broadband$broadband.availability.per.fcc[broadband$broadband.availability.per.fcc=="-"]<-"0"
broadband$broadband.availability.per.fcc<-as.double(broadband$broadband.availability.per.fcc)

broadband%>%arrange(broadband.usage)
broadband$broadband.usage[broadband$broadband.usage=="-"]<-"0"
broadband$broadband.usage<-as.double(broadband$broadband.usage)

###########################################################

broadband <- broadband %>%
  mutate(county.name=sub(" County","",county.name)) %>%
  separate(county.id,into=c("state.id","county.id"),sep=-3) 
  
  
broadband <- broadband %>%
  mutate(county.name = case_when(
    county.name=="LaSalle Parish" ~ "La Salle Parish",
    TRUE~county.name))
    
# separate the county.id by state and county---------------
  broadband_zipcode <- broadband_zipcode %>%
  separate(county.id,into=c("state.id","county.id"),sep=-3) 
  

#####################################
# in broadband_zipcode
# some postal codes are made of 4 digits, to use "geocode_zip"
# need 5 digits, so add a zero at the begin of the string

# plyr::count(sprintf("%05d", broadband_zipcode$postal.code))

broadband_zipcode$postal.code<-sprintf("%05d", broadband_zipcode$postal.code)

# broadband%>%filter(str_detect(county.name,"Bedford"))


##################################################
# make a unified dataset with broadband, postal codes and geodada------------------------------
my_df <- broadband%>%
  full_join(broadband_zipcode, by= c("st","state.id","county.id"))%>%
  select(1,2,3,7,8,5,6,9:12) %>%
  drop_na()


names(my_df)<-c("st","state.id","county.id","county.name","postal.code","brd_available","usage_micro","usage.general","mae","alpha","msd")

head(my_df);dim(my_df)


#########################################
# check of the counties in the data sets ----------
c$county.name.x[!c$county.name.x%in%d$county.name.y]
d$county.name.y[!d$county.name.y%in%c$county.name.x]

ss<-broadband%>%
  full_join(broadband_zipcode, by= c("st","state.id","county.id")) %>%
  filter(county.name.x %in% c("Bedford city",
                              "Covington city",
                              "Emporia city",
                              "Fairfax city",
                              "Kusilvak Census Area",
                              "Lexington city",
                              "Manassas Park city",
                              "Martinsville city",
                              "Oglala",
                              "Otter Tail")) %>%
  group_by(st,state.id,county.id) %>%
  summarize(unique(county.name.x),unique(county.name.y),unique(postal.code))
####################################


# make a new column with y/n broadband in the county
my_df <- my_df %>% 
  mutate(broadband.id=ifelse(brd_available==0,"no","yes"))


# find the geocodes with postal codes -------------------
geocode_zip<-geocode_zip(my_df$postal.code)


# add the geocodes------------------------------
s<-my_df%>%
  inner_join(geocode_zip,by=c("postal.code"="zipcode"))%>%
  plyr::gather(key="id",value="")

dim(s)
dim(my_df)

head(s)

# unite the state.id and county.id again ---------
s<-s%>%unite("id",state.id:county.id,sep= "")

# load the libraries for plotting ---------------------
library(sf)
library(raster)
library(spData)
library(spDataLarge)

library(maps)
library(viridis)


# mapping --------------------------------
us_map <- map_data("state")
us_map %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") +
  theme_void()

head(us_map)
head(my_df)

s %>%
  full_join(us_map, by = c("county.name" = "region")) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat.y),color = "black") +
  geom_point(aes(x = lng, y = lat.x, group = group,fill = "white")) +
  theme_void() +
  scale_fill_viridis(name = "US Broadband (%)",discrete=TRUE)


  ggplot(us_map,aes(x = long, y = lat,group = group)) +
  geom_polygon(color = "white") +
  geom_point(data= s, aes(x = lng, y = lat ,group = postal.code,color=county.name)) +
  theme_void() +
  scale_fill_viridis(name = "US Broadband (%)",discrete=TRUE)+
  theme(legend.position = "none")


  library(choroplethr)
  library(choroplethrMaps)



 s$id<-as.double(s$id) 
  
  my_map<-df_pop_county%>%
    inner_join(s,by=c("region"="id")) %>%
    mutate(value.x=value, value=brd_available)%>%
    dplyr::select(region,value)%>%
      dplyr::group_by(region) %>%
      dplyr::summarize(value = median(value)) %>%
      dplyr::mutate(region = as.numeric(region))


    group_by(region)%>%
    summarize(brd=median(value))


# first plot with broadband in counties ---------
county_choropleth(my_map,
                    title  = "Broadband availlability in US Counties",
                    legend = "percentage",
                    num_colors = 0)

#################################################
   
# second plot with usage by microsoft---------------     
    my_map2<-df_pop_county%>%
      inner_join(s,by=c("region"="id")) %>%
      mutate(value.x=value, value=usage_micro)%>%
      dplyr::select(region,value)%>%
      dplyr::group_by(region) %>%
      dplyr::summarize(value = median(value)) %>%
      dplyr::mutate(region = as.numeric(region))    
    
county_choropleth(my_map2,
                      title  = "Broadband usage by Microsoft in US Counties",
                      legend = "percentage",
                      num_colors = 0)    
  #############################################  
  
  
choro = CountyChoropleth$new(my_map)
  choro$title = "America's Broadband"
  choro$ggplot_scale = scale_fill_brewer(name="Broadband", palette="gray62" , drop=T)
  choro$render()


#################################################
# third plot with value above 15% ------------------  
  my_map3<-my_map%>%mutate(value=ifelse(value<=0.15,0,value))
  choro = CountyChoropleth$new(my_map2)
  choro$title = "America's Broadband"
  choro$ggplot_scale = scale_fill_brewer(name="Broadband", palette="gray62" , drop=FALSE)
  choro$render()



# select parish data in united states with broadband usage -----------------------
  
  brd_parish<-s%>%filter(str_detect(county.name,"Parish"))
  
  my_map_parish<-df_pop_county%>%
    inner_join(brd_parish,by=c("region"="id")) %>%
    mutate(value.x=value, value=usage_micro)%>%
    dplyr::select(region,value)%>%
    dplyr::group_by(region) %>%
    dplyr::summarize(value = median(value)) %>%
    dplyr::mutate(region = as.numeric(region))  
  
  
  county_choropleth(my_map_parish,
                    title  = "Broadband usage by Microsoft in US Parish",
                    legend = "percentage",
                    num_colors = 0)   



