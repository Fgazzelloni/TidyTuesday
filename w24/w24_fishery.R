# TidyTuesday week 24 - 

# Data Source: Great Lakes Fish,	Great Lakes Database,	Detroit Free Press
#database of all fish stocked from artificial propagation into the Great Lakes

##### libraries #######################
library(tidytuesdayR)
library(tidyverse)
library(stringr)
library(hrbrthemes)

# load data ##############################
tuesdata <- tidytuesdayR::tt_load(2021, week = 24)


stocked <- tuesdata$stocked
fishing <- tuesdata$fishing

# check #################################
head(stocked);dim(stocked)
head(fishing);dim(fishing)

library(DataExplorer)
profile_missing(fishing)

fishing$grand_total[is.na(fishing$grand_total)]<-0
fishing$values[is.na(fishing$values)]<-0

# wrangling ###########################
my_df <- fishing%>%
  arrange(year)%>%
  mutate(
    species=case_when(species=="Amercian Eel"~"American Eel",
              species=="Bullhead"~"Bullheads",
              species=="Channel catfish"~"Channel Catfish",
              species=="Cisco and chubs"~"Cisco and Chubs",
              species=="Cisco and Chub"~"Cisco and Chubs",
              species=="Crappie"~"Crappies",
              species=="Drum"~"Freshwater Drum",
              species=="Lake Trout - siscowet"~"Lake Trout",
              species=="Pacific salmon"~"Pacific Salmon",
              species=="White bass"~"White Bass",
              TRUE~species))


# plotting #################################
final <- my_df%>%
  filter(!str_detect(species,"and")) %>%
  group_by(year,species)%>%
  summarize(total_number=sum(grand_total),total_production=sum(values))%>%
  ungroup()%>%
  arrange(total_number)%>%
  filter(!total_number==0)%>%
  mutate(percent=total_production/total_number)%>%
  filter(!percent>1)%>%
  arrange(year,percent)%>%#filter(species==c("Pacific Salmon","White Bass","Carp","Lake Trout"))%>%
  ggplot(aes(x=year,y=reorder(species,-total_production),group=species,color=species))+
  geom_line()+
  geom_point(aes(size=percent),alpha=0.5)+
  labs(title="Fisheries species total production by Year",subtitle="sized by percentage of production",
       tag="fig1",
       x="Time(Year)",y="Species",
       color="Species",size="Percent",
       caption="Viz @fgazzelloni, DataSource: TidyTuesday Week24 - Great Lakes Fish,Great Lakes Database,Detroit Free Press")+
  scale_x_continuous(breaks=seq(1867, 2015, 8))+
  theme_ft_rc()+
  theme(axis.text.x = element_text(angle=90))




###################### SAVING ############################


ragg::agg_png(here::here("w24","w24_fisheries.png"),
              res = 320, width = 14, height = 8, units = "in")
final

dev.off()



#### ATTACHING LOGO ############################
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


final_plot <- image_read("w24_fisheries.png")

attached_logo <- image_composite(final_plot, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "w24_fisheries.png",
            format = "png") # save final plot



##############################################################











