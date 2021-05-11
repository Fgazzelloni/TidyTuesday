# Week 20 US Broadband

library(tidytuesdayR)
library(tidyverse)
library(DataExplorer)

library(zipcodeR)
library(janitor)
library(stringr)

tuesdata <- tidytuesdayR::tt_load(2021, week = 20)

broadband <- tuesdata$broadband
broadband_zipcode<-read.csv("https://raw.githubusercontent.com/microsoft/USBroadbandUsagePercentages/master/dataset/broadband_data_zipcode.csv")

profile_missing(broadband);head(broadband)
profile_missing(broadband_zipcode);head(broadband_zipcode)


str(broadband);dim(broadband)
str(broadband_zipcode);dim(broadband_zipcode)

names(broadband)<-make.names(tolower(names(broadband)))
names(broadband_zipcode)<-make.names(tolower(names(broadband_zipcode)))

as.character(broadband$broadband.availability.per.fcc)
broadband%>%filter(str_detect("-",broadband$broadband.availability.per.fcc))
broadband$broadband.availability.per.fcc[broadband$broadband.availability.per.fcc=="-"]<-"0"
broadband$broadband.availability.per.fcc<-as.double(broadband$broadband.availability.per.fcc)

as.character(broadband$broadband.usage)
broadband%>%arrange(broadband.usage)
broadband$broadband.usage[broadband$broadband.usage=="-"]<-"0"
broadband$broadband.usage<-as.double(broadband$broadband.usage)


plyr::count(broadband$county.name)
plyr::count(broadband_zipcode$county.name)

my_df<-broadband%>%
  right_join(broadband_zipcode, by= c("st","county.id")) %>%
  select(1,2,6,7,4,5,8,9,10)

my_df[is.na(my_df)] <- 0

glimpse(my_df)
head(my_df);dim(my_df)
profile_missing(my_df)

names(my_df)<-c("st","id","county","zip","broadband","usage.m","usage.z","mae","alpha")

my_df<-my_df%>%mutate(broadband.id=ifelse(broadband==0,"no","yes"))


length(my_df$zip)
geocode_zip<-geocode_zip(my_df$zip)
geocode_zip$zipcode<-as.integer(geocode_zip$zipcode)

my_df<-my_df%>%
  inner_join(geocode_zip,by=c("zip"="zipcode"))


library(sf)
library(raster)
library(spData)
library(spDataLarge)

library(maps)
library(viridis)

us_map <- map_data("state")
us_map %>%
  ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "lightblue", color = "black") +
  theme_void()

head(us_map)
head(my_df)

my_df %>%
  right_join(us_map, by = c("county" = "region")) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat.y),color = "black") +
  geom_point(aes(x = lng, y = lat.x, group = group,fill = "white")) +
  theme_void() +
  scale_fill_viridis(name = "US Broadband (%)",discrete=TRUE)


  ggplot(us_map,aes(x = long, y = lat,group = group)) +
  geom_polygon(color = "black") +
  geom_point(data= my_df, aes(x = lng, y = lat ,group = zip,fill = county,color=county)) +
  theme_void() +
  scale_fill_viridis(name = "US Broadband (%)",discrete=TRUE)+
  theme(legend.position = "none")



  ## install.packages("ggmap")
  library(ggmap)

  ## Need a Google Maps API Key for this to work!
  ## NOTE: Registering the API key only needs to be done once.
  maps_api_key <- Sys.getenv("GOOGLEMAPS_API_KEY")
  register_google(key = maps_api_key)

  US <- get_map("US", zoom = 12)


  library(choroplethr)
  library(choroplethrMaps)

  data(df_pop_county)
  df_pop_county %>% slice(1:3)

  head(df_pop_county)

  county_choropleth(df_pop_county)

  my_map<-df_pop_county%>%
    inner_join(my_df,by=c("region"="id"))%>%
    mutate(value.x=value, value=broadband)%>%
    dplyr::select(region,value)%>%
      dplyr::group_by(region) %>%
      dplyr::summarize(value = median(value)) %>%
      dplyr::mutate(region = as.numeric(region))


    group_by(region)%>%
    summarize(brd=median(value))



  county_choropleth(my_map,
                    title  = "US 2012 County Population Estimates",
                    legend = "Population",
                    num_colors = 0)


  choro = CountyChoropleth$new(my_map)
  choro$title = "America's Broadband"
  choro$ggplot_scale = scale_fill_brewer(name="Broadband", palette="gray62" , drop=FALSE)
  choro$render()

  palette()

  my_map2<-my_map%>%mutate(value=ifelse(value<=0.15,0,0.15))
  choro = CountyChoropleth$new(my_map2)
  choro$title = "America's Broadband"
  choro$ggplot_scale = scale_fill_brewer(name="Broadband", palette="gray62" , drop=FALSE)
  choro$render()









