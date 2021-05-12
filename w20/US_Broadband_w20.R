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


broadband_zipcode%>%mutate(ifelse())

###########################################################

broadband<-broadband %>%
  mutate(county.name=sub(" County","",county.name))%>%
  mutate(county.name = case_when(
    county.name=="Bedford city" ~ "Bedford",
    county.name=="Fairfax city" ~ "Fairfax",
    county.name=="Covington city" ~ "Covington",
    county.name=="Lexington city" ~ "Lexington",
    county.name=="LaSalle Parish" ~ "La Salle Parish",
    county.name=="Manassas Park city" ~ "Manassas city",
    TRUE~county.name))


# broadband%>%filter(str_detect(county.name,"La Salle Parish"))
# broadband_zipcode%>%filter(str_detect(county.name,"La Salle Parish"))

c<-broadband_zipcode%>%filter(str_detect(county.name,"Bedford"))
profile_missing(c)

filter(!county.name%in%c("Watonwan",
                           "Kusilvak Census Area",
                           "Martinsville city",
                           "Oglala",
                           "Otter Tail",
                           "Emporia city"))


########### check duplicate
a<-plyr::count(broadband$county.name)
a<-a%>%arrange(x)
head(a)
dim(plyr::count(broadband$county.name))

b<-plyr::count(broadband_zipcode$county.name)
head(b)
b<-b%>%arrange(x)

dim(plyr::count(broadband_zipcode$county.name))

a$x[!a$x%in%b$x]
b$x[!b$x%in%a$x]
#####################################
# in broadband_zipcode
# some postal codes are made of 4 digits, to use "geocode_zip"
# need 5 digits, so add a zero at the begin of the string

broadband_zipcode$state.id<-str_sub(broadband_zipcode$county.id, end=-4)

broadband_zipcode$postal.code<-sprintf("%05d", broadband_zipcode$postal.code)

broadband%>%filter(str_detect(county.name,"Bedford"))

broadband_zipcode%>%
  filter(str_detect(county.name,"Bedford"))%>%
  group_by(st,county.id)%>%summarize()

my_df<-broadband%>%
  full_join(broadband_zipcode, by= c("st","county.id","county.name"))


profile_missing(my_df)

my_df%>%filter(is.na(postal.code))

my_df[is.na(my_df)] <- 0

glimpse(my_df)
head(my_df);dim(my_df)


names(my_df)<-c("st","id","county","brd_available","usage_micro","zip","usage.general","mae","alpha","msd")
my_df<-my_df%>%select(1,2,3,6,4,5,7:10)

plyr::count(my_df$brd_available)

my_df<-my_df%>%mutate(broadband.id=ifelse(brd_available==0,"no","yes"))




length(my_df$zip)
dim(plyr::count(my_df$zip))

geocode_zip<-geocode_zip(my_df$zip)
geocode_zip$zipcode<-as.numeric(geocode_zip$zipcode)
dim(geocode_zip)

z<-my_df$zip
h<-geocode_zip$zipcode

my_df$zip[!my_df$zip%in%geocode_zip$zipcode]
geocode_zip$zipcode[!geocode_zip$zipcode%in%my_df$zip]

geocode_zip("00000")

s<-my_df%>%
  inner_join(geocode_zip,by=c("zip"="zipcode"))
dim(s)
dim(my_df)

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



head(broadband_zipcode)

my_map<-geocode_zip(broadband_zipcode$POSTAL.CODE)
# my_map2<-geocode_zip(broadband$)




# select parish data in united states with broadband usage -----------------------
brd_Parish<-broadband%>%filter(str_detect(county.name,"Parish"))
brd_zip_Parish<-broadband_zipcode%>%filter(str_detect(county.name,"Parish"))

brd_Parish$county.name[!brd_Parish$county.name%in%brd_zip_Parish$county.name] #"La Salle Parish"
brd_zip_Parish$county.name[!brd_zip_Parish$county.name%in%brd_Parish$county.name]
dim(brd_Parish)
dim(brd_zip_Parish)

brd_parish<-brd_Parish%>%right_join(brd_zip_Parish,by=c("st","county.id","county.name"))
head(brd_parish)
dim(brd_parish)

profile_missing(brd_parish)

brd_parish[is.na(brd_parish)]<-0

geocode_parish_zip<-geocode_zip(brd_parish$postal.code)

brd_parish%>%left_join(geocode_parish_zip,by=c("postal.code"="zipcode"))



########################################################
library(ggmap)

## Need a Google Maps API Key for this to work!
## NOTE: Registering the API key only needs to be done once.
maps_api_key <- Sys.getenv("GOOGLEMAPS_API_KEY")
register_google(key = maps_api_key)

beijing <- get_map("Beijing", zoom = 12)
########################################
