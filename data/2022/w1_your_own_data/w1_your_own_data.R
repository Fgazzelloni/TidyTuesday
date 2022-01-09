# OMICRON INCREASE FROM 1 December 2021 TO 7 January 2022
# DataSources:
# https://www.ecdc.europa.eu/en/publications-data/data-virus-variants-covid-19-eueea
# https://www.gisaid.org/hcov19-variants/

library(tidyverse)
library(readr)
eu_data <- read_csv("/Users/federica/Documents/R/GBD/Comunicable_diseases/Covid19/variants/data.csv")
eu_data2 <- read_csv("/Users/federica/Documents/R/GBD/Comunicable_diseases/Covid19/variants/data2.csv")


eu_data%>%
  select(country,variant)%>%
  filter(str_detect(variant,"B.1.1."))%>% #variant B.1.1.529
  count(variant)


countrySubmissionCount <- read_csv("/Users/federica/Documents/R/GBD/Comunicable_diseases/Covid19/variants/countrySubmissionCount.csv")
countrySubmissionCount2 <- read_csv("/Users/federica/Documents/R/GBD/Comunicable_diseases/Covid19/variants/countrySubmissionCount2.csv")

# map data
library(sf)
world <- rnaturalearth::ne_countries(scale=110,returnclass = "sf")
world<-filter(world,!continent=="Antarctica")


# December 2021 data
new_data<- countrySubmissionCount%>%
  mutate(variant="B.1.1.529")%>%
  rename(percent_variant="%GR/484A (B.1.1.529) in past 4 weeks",
         number_detections_variant="Total #GR/484A (B.1.1.529)")%>%
  mutate(year_week="2021-46")%>%
  janitor::clean_names()%>%
  select(country,variant,percent_variant,year_week,number_detections_variant)

full_data <- eu_data%>%#count(country)%>%View()
  full_join(new_data,by=c("country","variant","percent_variant","year_week","number_detections_variant"))%>%
  filter(str_detect(variant,"B.1.1.5"))%>%
  filter(number_detections_variant>0)

full_data_geo<- full_data%>%
  inner_join(world,by=c("country"="name"))

# January 2022 data
new_data2<- countrySubmissionCount2%>%
  mutate(variant="B.1.1.529")%>%
  rename(percent_variant="%Omicron GRA (B.1.1.529+BA.*) in past 4 weeks",
         number_detections_variant= "Total #Omicron GRA (B.1.1.529+BA.*)")%>%
  mutate(percent_variant=gsub("%","",percent_variant),
         percent_variant=as.double(percent_variant))%>%
  mutate(year_week="2022-01")%>%
  janitor::clean_names()%>%
  select(country,variant,percent_variant,year_week,number_detections_variant)

full_data2 <- eu_data2%>%#names()
  select(country,year_week,new_cases,variant,percent_variant,number_detections_variant)%>%
  full_join(new_data2,by=c("country","variant","percent_variant","year_week","number_detections_variant"))%>%
  filter(str_detect(variant,"B.1.1.5"))%>%
  filter(number_detections_variant>0)

full_data_geo2<- full_data2%>%
  inner_join(world,by=c("country"="name"))



dec<-new_data%>%group_by(country)%>%summarise(median_dec=median(percent_variant))

jan<-new_data2%>%group_by(country)%>%summarise(median_jan=median(percent_variant))

merged<-dec%>%
  merge(jan,by="country")

countries<-merged%>%count(country)%>%select(-n)

# libraries for fonts
library(ggCyberPunk)
ggCyberPunk::import_aldrich()

library(extrafont)
library(showtext)
showtext::showtext_auto()
showtext::showtext_opts(dpi=110)
library(sysfonts)
sysfonts::font_families_google()
font_add_google(name ="Texturina" ,family = "my_font")
font_add_google(name ="Suravaram" ,family = "my_font2")

family<-"my_font"
family2<-"my_font2"

library(ggh4x)
barplot<-merged%>%
  pivot_longer(cols = 2:3,names_to="names",values_to="values")%>%
  ggplot()+
  geom_col(aes(x=fct_reorder(country,-values),values,fill=names),
           width = 0.5)+
  scale_fill_cyberpunk(palette= "laser sword",reverse=F,
                       labels=c("December","January"),
                       name="")+
  scale_x_discrete(expand = expansion(mult = c(0,0)))+
  # from: https://cran.r-project.org/web/packages/ggh4x/vignettes/PositionGuides.html
  guides(y = guide_axis_manual( label_size = c(12, 9),label_hjust=1))+
  coord_flip()+
  theme_void()+
  theme(text = element_text(family=family,size=14,color = "#FFE1FF"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(family=family2,
                                   size=8,color = "#FFE1FF",hjust = 0),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank(),
        legend.position = c(0.7,0.5),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.text = element_text(size=10))



plot <- ggplot(world)+
  geom_sf(aes(geometry=geometry),fill="white",size=0.01)+
  geom_sf(data=full_data_geo,
          mapping=aes(geometry=geometry,
                      fill=percent_variant,
                      color=percent_variant),
          size=0.01)+
  scale_fill_cyberpunk(palette = "laser sword",
                       reverse=F,discrete=F,
                       name="Percent Variant",
                       labels=c("<10%","20%","40%","60%"))+
  coord_sf()+
  labs(title="How fast is the New OMICRON variant spreading in the World?")+
  guides(color="none")+
  ggthemes::theme_map()+
  theme(text = element_text(family=family,size=14,color = "#FFE1FF"),
        plot.title = element_text(vjust=-1.5,size=12),
        plot.title.position = "panel", 
        plot.caption = element_text(vjust=8),
        legend.background = element_blank(),
        legend.position = "none")


plot2 <- ggplot(world)+
  geom_sf(aes(geometry=geometry),fill="white",size=0.01)+
  geom_sf(data=full_data_geo2,
          mapping=aes(geometry=geometry,
          color=percent_variant,fill=percent_variant),size=0.1)+ 
  scale_fill_cyberpunk(palette= "laser sword",
                       discrete = F,reverse = F,
                       labels=c(">0","25%","50%","75%","100%"),
                       name="")+
  coord_sf()+
  guides(color="none")+
  ggthemes::theme_map()+
  theme(text = element_text(family=family,size=14,color="#FFE1FF"),
        legend.direction = "horizontal",
        legend.background = element_blank(),
        legend.position =c(-0.35,0.5),
        legend.justification = "center",
        legend.text.align = 0.5,
        legend.title = element_text(face = "bold",size=14),
        legend.key.size = unit(0.5, 'cm'), #change legend key size
        legend.key.height = unit(0.5, 'cm'), #change legend key height
        legend.key.width = unit(0.5, 'cm'), #change legend key width
        legend.text = element_text(size=10))



library(cowplot)

file<-"data/2022/w1_your_own_data/your_own_data.png"
ragg::agg_png(file,
              res = 320, 
              width = 1200, 
              height = 675, 
              units = "px",
              background = "#5e3e4e",#"#4287f5",
              scaling = 0.5)

ggdraw()+
  draw_label("Omicron genome sequences with unprecedented speed - one month of infection",
             x=0.5,y=0.97,fontfamily = family,size=20,
             color="#eb3471")+
  draw_label("Omicron genome sequences with unprecedented speed - one month of infection",
             x=0.5,y=0.975,fontfamily = family,size=21,
             color="#FFE1FF")+
  draw_plot(plot,x=0.18,y=0.2,scale=0.65)+
  draw_plot(plot2,x=0.18,y=-0.25,scale=0.65)+
  draw_label("Percent Variant (median values)",
             x=0.15,y=0.88,fontfamily = family,size=12,
             color="#FFE1FF")+
  draw_plot(barplot,x=-0.28,y=0.15,scale=0.4)+
  draw_label("Datasource: ECDC & GISAID | Map: Federica Gazzelloni",
             x=0.5,y=0.025,fontfamily = family,size=11.5,
             color="#FFE1FF")+
  draw_label("Date: December 1st 2021",x=0.98,y=0.73,size=10,angle=-90,
             color="#FFE1FF",fontfamily = family)+
  draw_label("Date: January 7th 2022",x=0.98,y=0.3,size=10,angle=-90,
             color="#FFE1FF",fontfamily = family)+
  draw_label("Percent Variant",x=0.15,y=0.33,size=12,color="#FFE1FF",fontfamily = family)+
  draw_label("Percent Variant values are calculated\nconsidering the increase in\nCovid19 new cases due\nto the Omicron variant",
             x=0.15,y=0.1,size=10,color="#FFE1FF",fontfamily = family) +
  draw_image("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png",x=0.25,y=-0.45,scale=0.09)+
  draw_image("data/2022/w1_your_own_data/omicron.png",x=0.35,y=-0.45,scale=0.09)

invisible(dev.off())

