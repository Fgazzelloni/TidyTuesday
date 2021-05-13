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
  separate(county.id,into=c("state.id","county.id"),sep=-3) %>%
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


names(my_df)<-c("st","state.id","county.id","county.name","postal.code","brd.available","usage.1119","usage.1020","mae","alpha","msd")

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
  mutate(broadband.id=ifelse(brd.available==0,"no","yes"))


# find the geocodes with postal codes -------------------
geocode_zip<-geocode_zip(my_df$postal.code)


# add the geocodes------------------------------
s<-my_df%>%
  inner_join(geocode_zip,by=c("postal.code"="zipcode"))%>%
  unite("id",state.id:county.id,sep= "")


# load the libraries for plotting ---------------------
library(sf)
library(raster)
library(spData)
library(spDataLarge)

library(maps)
library(viridis)
library(ggthemes)
library(extrafont)
fonts()

# mapping --------------------------------

us_county_map <- map_data("county")

ggplot()+
  geom_polygon(data=us_county_map,aes(x=long,y=lat,group = group),
               fill=NA,color = "lightblue")+
  geom_point(data=subset(s,lat>25&lat<50),
             aes(x=lng,y=lat, group =st,color=brd.available),
             alpha=0.3,size=0.5)+
  scale_color_viridis()+
  labs(title="America's Broadband",
       subtitle="available values by County",
       caption="",
       color="")+
  theme_map()+
  theme(plot.title =element_text(size=40,face="bold",family ="World of Water"),
        plot.subtitle =element_text(size=25,face="bold",family ="World of Water"),
        plot.title.position = "panel",
        plot.margin = margin(5,5,5,5),
        legend.text = element_text(size=8,family ="World of Water"))


################################################################################


head(s)
fit<-lm(usage.1020~usage.1119,data=s)
summary(fit)
plot(fit)
confint(fit)

mu<-s$mae
plot(density(mu))
lines(density(std),col="red")
lines(density(z))

ggplot(data=s) +
  geom_density(aes(y=mae,group=st,color=st)) +
  #geom_line(aes(x=brd.available,y=mae)) +
  coord_flip()




mean<-mean(mu)
std<-s$msd
mean(std)
x<-s$brd.available
mean(x)
range(std)
z<-log(s$brd.available-mu)/std


range(z)
h<-dnorm(z,mu,std)
v<-pnorm(z,mu,std)
plot(h)
plot(v)
qqplot(h,v)
x <- rnorm(z, mu, std)
range(x);length(x)
x[is.nan(x)]<-0
hist(x, probability=TRUE)
xx <- seq(min(x), max(x), length=32707)
lines(xx, dnorm(xx,mu, std),col="pink")











