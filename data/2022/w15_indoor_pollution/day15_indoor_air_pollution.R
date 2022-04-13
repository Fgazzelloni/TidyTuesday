
#30DayChartChallenge 2022 Day12 - theme day: The Economist
#TidyTuesday Week15 - Indoor Air Pollution
# Author: Federica Gazzelloni

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load libraries and options
options(scipen = 999) # this is to avoid number truncation
library(countrycode)
library(tidyverse)

# set the colors
mycolors <- colorRampPalette(RColorBrewer::brewer.pal(n=9, name="Set1"))(5)

# load data
death_timeseries<-read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-12/death_timeseries.csv")

# data wrangling
death_timeseries1<-death_timeseries%>%
  rename(Deaths=`Deaths...Cause..All.causes...Risk..Household.air.pollution.from.solid.fuels...Sex..Both...Age..All.Ages..Number.`,
         Deaths1=`Deaths...Cause..All.causes...Risk..Household.air.pollution.from.solid.fuels...Sex..Both...Age..All.Ages..Number..1`)


death_timeseries2 <- death_timeseries1%>%
  filter(Year==1990,Year.1==2019) %>%
  filter(!str_detect(Entity,"World|Region|Central|Countries|European"),
         !str_detect(Entity,"Western|Southern|Eastern|Northen"),
         !str_detect(Entity,"North|South|Southeast"),
         !str_detect(Entity,"income|High|Low"),
         !str_detect(Entity,"Commonwealth"),
         !str_detect(Entity,"G20|SDI"),
         !str_detect(Entity,"Europe|Asia|Africa|America|Oceania")
         ) 

# add missing continents' names with countrycode() function -----------
death_timeseries2$continent <- countrycode(sourcevar = death_timeseries2[, "Entity"],
                                           origin = "country.name",
                                           destination = "continent")


death_timeseries3 <- death_timeseries2%>%
  # check for some values that were not matched unambiguously ---------
  # filter(Entity%in%c("Australasia", "Caribbean", 
  #                    "England", "Micronesia (country)", 
  #                    "Scotland", "Timor", "Wales")) %>%
  mutate(Entity=as.character(Entity), # mutate Entity back as a character to use case_when() function
         continent=case_when(Entity%in%c("England","Scotland","Wales")~"Europe", 
                          Entity=="Australasia"~"Oceania", 
                          Entity=="Caribbean"~"Americas", 
                          Entity=="Micronesia (country)"~"Asia", 
                          Entity=="Timor"~"Asia",
                          TRUE ~ continent),
         Entity=as.factor(Entity)) %>%
  filter(Deaths1>=1,Deaths>=1) 

# make the plot
death_timeseries3 %>%
  ggplot(aes(x=Deaths1,y=Deaths))+
  geom_jitter(size=1.7,aes(fill=continent),shape=21,alpha=0.7,color="grey45")+
  geom_smooth(method = "lm",se=F,color="grey60",
              linetype="dashed",size=0.5)+
  geom_text(aes(label=Entity,color=continent),
            hjust = "left",
            show.legend = F,
            vjust="top",
            check_overlap = T,
            size=3)+
  scale_x_log10(breaks=c(1,10,100,1000,10000,100000),
                expand=expansion(add=c(0,0.05)),
                label=scales::comma_format(accuracy = NULL))+
  scale_y_log10(breaks=c(1,10,100,1000,10000,100000),
                expand=expansion(add=c(0,0.8)),
                label=scales::comma_format(accuracy = NULL))+ 
  scale_fill_manual(guide=guide_legend(nrow = 1),
                    values=mycolors)+
  scale_color_manual(values=mycolors)+
  labs(title="Deaths due to Household air pollution\nfrom solid fuels",
       subtitle="1990 vs 2019 All Ages and Gender",
       caption="\n#30DayChartChallenge 2022 day12 - theme day: The Economist\nDataSource: Our World in Data | #TidyTuesday week15 - Indoor Air Pollution\nDataViz: Federica Gazzelloni",
       x="Deaths from indoor air pollution in 2019",
       y="Deaths from indoor air pollution in 1990",fill="") +
  ggthemes::theme_economist() +
  theme(legend.text = element_text(size=10),
        plot.title = element_text(size=23),
        axis.title.x = element_text(vjust = -0.8),
        axis.title.y = element_text(vjust = 0.8),
        plot.caption = element_text(vjust = -1,size=9),
        legend.position = c(0.4,0.85))

#save the plot
ggsave("day12_the_economist.png",
       dpi=320,
       width = 8,
       height = 6)


