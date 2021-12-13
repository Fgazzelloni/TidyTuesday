library(tidyverse)
spiders <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-07/spiders.csv')

# spiders%>%View
sp_family_graph<-spiders%>%
  count(family,genus,species,sort=T)%>%
  select(-n)

# https://stackoverflow.com/questions/24173194/remove-parentheses-and-text-within-from-strings-in-r
my_spider_countries<- spiders%>%
  separate(distribution,into=c("country","region"),sep=",|;",remove = FALSE)%>%
  mutate(country=stringi::stri_trans_totitle(country))%>%
  mutate(country2=gsub("\\s*\\([^\\)]+\\)*","",country))%>%
  mutate(country2=gsub("^D.r.|Dr.","Dem. Rep. ",country2))%>%
  
  mutate(country2=gsub("^ ","",country2))%>%
  
  mutate(country2=gsub(" To .*$","",country2))%>% 
  
  mutate(country2=gsub(" Or .*$","",country2))%>% 
  
  mutate(country2=gsub("Is.$","Islands",country2))%>%
  
  mutate(country2=gsub("\\?$","",country2))%>%
  mutate(country2=gsub(" And .*$","",country2))%>%#
  
  mutate(country2=gsub(". Introduced$","",country2))%>%
  
  mutate(country2=case_when(str_detect(country2,"Austral")~"Australia",
                            #str_detect(country2,"Rep.")~"Republic",
                            str_detect(country2,"Bahama")~"Bahamas",
                            str_detect(country2,"Brazi")~"Brazil",
                            str_detect(country2,"Brezi")~"Brazil",
                            str_detect(country2,"Bosnia Herzegovina")~"Bosnia and Herz.",
                            #str_detect(country2,"Canary")~"Canary Islands",
                            str_detect(country2,"Caribbean")~"Caribbean",
                            str_detect(country2,"Czechia")~"Czech Rep.",
                            str_detect(country2,"Cina")~"China",
                            str_detect(country2,"Central Africa")~"Central African Rep.",
                            str_detect(country2,"Asia")~"Asia",
                            str_detect(country2,"Cape Verde")~"Cape Verde",
                            str_detect(country2,"Colombia")~"Colombia",
                            str_detect(country2,"Columbia")~"Colombia",
                            str_detect(country2,"Europ")~"Europe",
                            str_detect(country2,"Himalaya")~"Himalayas",
                            str_detect(country2,"Indonesia")~"Indonesia",
                            str_detect(country2,"Guinea")~"Guinea",
                            str_detect(country2,"Usa")~"United States",
                            str_detect(country2,"Britain")~"United Kingdom",
                            str_detect(country2,"Ecuador")~"Ecuador",
                            str_detect(country2,"Ghana")~"Ghana",
                            str_detect(country2,"Greece")~"Greece",
                            str_detect(country2,"Guadeloupe")~"Guadaloupe",
                            str_detect(country2,"Kyrgystan")~"Kyrgyzstan",
                            str_detect(country2,"Laos")~"Laos",
                            str_detect(country2,"Malaysia")~"Malaysia",
                            str_detect(country2,"Spain")~"Spain",
                            str_detect(country2,"Reunion")~"Réunion",
                            str_detect(country2,"Saint Lucia")~"Saint Lucia",
                            str_detect(country2,"São Tomé")~"São Tomé Príncipe",
                            str_detect(country2,"Kerguelen")~"Kerguelen Islands",
                            str_detect(country2,"St. Vincent")~"St. Vincent",
                            str_detect(country2,"Virgin Islands")~"Virgin Islands",
                            str_detect(country2,"Mexic")~"Mexico",
                            TRUE~country2))%>%
  filter(!str_detect(country2,"Unknown|West|North|Western|East|Poss|prob|Pres|Prob"))


it_to_from<-my_spider_countries%>%
  filter(country2=="Italy")%>%
  mutate(region=trimws(region))%>%filter(!is.na(region))%>% #count(region)%>%View
  mutate(region=gsub("\\?$","",region))%>%
  mutate(region=gsub("Central Europe to ","",region))%>%#count(region)%>%View
  filter(!str_detect(region,"Central to "),
         !region=="south-eastern Europe")%>%#count(region)%>%View
  mutate(region=gsub(" and|to*$","",region))%>%
  mutate(region=case_when(str_detect(region,"Greece")~"Greece",
                          str_detect(region,"Ukraine")~"Ukraine",
                          str_detect(region,"Russia")~"Russia",
                          TRUE~region))%>%
  mutate(region=gsub("\\)$","",region))%>%
  rename(from=country2,to=region)

#it_to_from%>%View

library(ggraph)
library(igraph)
library(tidyverse)

spiders_Balkans <-  c("Bulgaria","Albania","Greece","Bosnia","Kosovo","Macedonia",
                      "Montenegro","Romania","Serbia")%>%as_tibble()%>%
  rename(to=value)

italy_to<-it_to_from%>%count(to)%>%select(-n)
italy_to<- italy_to%>%filter(!to=="Balkans")
italy_to <- rbind(italy_to,spiders_Balkans)%>%unlist()


world <- map_data("world")%>%
  filter(!region=="Antarctica")

italy<- world%>%
  filter(region=="Italy")


library(sf)
# centroids and coords with spData::world-----
world2_geo<- spData::world

it_centroids<- world2_geo%>%
  filter(name_long=="Italy")%>%
  st_centroid()%>%
  st_coordinates()%>%
  as.data.frame()%>%
  mutate(from="Italy")%>%
  rename(long_from=X,lat_from=Y)


to_country_geom<- world2_geo%>%
  filter(name_long%in%italy_to) %>% #18 out of 24
  st_centroid()%>%select(name_long)


to_centroids<- to_country_geom%>%
  st_coordinates()%>%
  as.data.frame()%>%
  rename(long_to=X,lat_to=Y)

to_df<-cbind(to=to_country_geom$name_long,to_centroids)%>%
  filter(!to=="Kosovo")

to_and_from<- it_to_from%>%
  count(family,genus,species,subspecies,year,from,to)%>%
  select(-n)%>%
  filter(to%in%to_df$to) %>%
  left_join(it_centroids,by="from") %>%#count(to)
  left_join(to_df,by="to")


balkans_to<- to_df%>%
  filter(to%in%c("Serbia","Montenegro","Macedonia","Albania"))%>%
  merge(it_centroids)


lng_rng <- range(to_and_from$long_to)
lat_rng <- range(to_and_from$lat_to)

library(extrafont)
loadfonts()
library(showtext)
font_add(family = "Blackwidow", regular = "Blackwidow-o6ga.ttf") # https://www.fontspace.com/blackwidow-font-f23155
#font_add(family = "Montserrat", regular = "Montserrat-Regular.ttf") # https://fonts.google.com/specimen/Montserrat?category=Sans+Serif
showtext_auto()
showtext_opts(dpi = 320)

# map--------
italy_map<-ggplot(world)+
  # rest of the countries ploygons
  geom_polygon(aes(x=long,y=lat,group=group),
               alpha=0.5,fill="darkcyan",color="grey58") +
  # italy polygon
  geom_polygon(data=italy,
               aes(x=long,y=lat,group=subregion),
               color="azure4",fill="darkgoldenrod3") +
  # name of the countries (to)
  geom_text(data= to_df,
            aes(x=long_to,y=lat_to,label=to),
            color="black",nudge_y = 0.5,nudge_x=0.5,
            family="Blackwidow",size=6) +
  # to points
  geom_point(data= to_df,
             aes(x=long_to,y=lat_to),
             color="brown",size=4,alpha=0.9,shape=21,stroke=2)+
  # points connections
  geom_curve(data= to_and_from,
               aes(x = long_from, y = lat_from, 
                   xend = long_to, yend = lat_to),
             curvature = 0.2,size=0.3,
             color="dodgerblue4",
             alpha=0.4,
             arrow = arrow(length = unit(0.25, "cm"))) +
  # adding balkans countries points connections
  geom_curve(data= balkans_to,
             aes(x = long_from, y = lat_from, 
                 xend = long_to, yend = lat_to),
             curvature = 0.2,size=0.3,
             color="dodgerblue4",
             alpha=0.4,
             arrow = arrow(length = unit(0.25, "cm"))) +
  # stroke of centre point of italy
  geom_point(data= it_centroids,
             aes(x=long_from,y=lat_from),
             color="brown",size=3,shape=21,stroke=2,alpha=0.9) +
  # center point of italy
  geom_point(data= it_centroids,
             aes(x=long_from,y=lat_from),
             color="yellow2",size=1) +
  geom_text(data= it_centroids,
             aes(x=long_from,y=lat_from,
                 label="Italy"),
            family="Blackwidow",nudge_y = 1,
             color="black",size=10) +
  
  coord_cartesian(xlim=c(-8.42048,54.28545),ylim=c(28.18548,49.14882))+
  labs(caption="Datasource: World Spiders Database | Majer et al, 2015 | #TidyTuesday week50\nDataViz: Federica Gazzelloni")+
  ggthemes::theme_map()+
  theme(text = element_text(family="Roboto Condensed"),
        plot.caption = element_text(size=11))

italy_map


# taxonomy (species and suspecies in Italy)--------
spiders_italy <-  filter(my_spider_countries, grepl("Italy", country2))
spiders_italy<- spiders_italy%>%filter(!is.na(subspecies))

spiders_italy$pathString <- paste("Spiders", 
                                  spiders_italy$family,
                                  spiders_italy$genus,
                                  spiders_italy$species, 
                                  spiders_italy$subspecies,
                                  sep = "|")
#convert to Node
spiders_italy <- as.Node(spiders_italy, pathDelimiter = "|")
useRtreeList <- ToListExplicit(spiders_italy, unname = TRUE)

radialNetwork(useRtreeList,
              fontSize = 8,
              fontFamily = "Roboto Condensed", 
              linkColour = "#8898b3", 
              nodeColour = "brown",
              nodeStroke = "#acb507", 
              textColour = "black", 
              #opacity = -1/20,
              margin = list(2,2,2,2))

diagonalNetwork(useRtreeList,
              fontSize = 15,
              fontFamily = "Roboto Condensed", 
              linkColour = "#8898b3", 
              nodeColour = "brown",
              nodeStroke = "#acb507", 
              textColour = "brown", 
              #opacity = -1/20,
              margin = list(2,2,2,2))

# export the radial plot and save it as .png

# gt table -----------
# https://gt.rstudio.com/reference/tab_options.html
library(gt)
library(tidyverse)
library(glue)

# Define the start and end dates for the data range
start_date <- spiders%>%filter(year==min(year))%>%count(year)%>%select(-n)%>%unlist()
end_date <- spiders%>%filter(year==max(year))%>%count(year)%>%select(-n)%>%unlist()

spiders_italy <-  filter(my_spider_countries, grepl("Italy", country2))
spiders_italy<- spiders_italy%>%filter(!is.na(subspecies))

tax_tb<-spiders_italy %>%
  arrange(year)%>%
  select(Year=year,Family=family,Genus=genus,Species=species,Subspecies=subspecies) %>%
  gt() %>%
  tab_header(
    title = md("**Spiders Taxonomy**"),
    subtitle = glue("{start_date} to {end_date}")
  ) %>%
  tab_source_note(
    source_note = md("Datasource: **World Spiders Database** | Majer et al, 2015")
  )  %>%
  tab_options(table.background.color="darkcyan")%>%
  bstfun::as_ggplot()


my_spider_countries%>%
  filter(country2=="Italy")%>%
  filter(!is.na(subspecies))%>%
  ggplot()
  


# final touches----------
library(cowplot)
final_plot<- ggdraw(italy_map)+
  draw_label("Spiders from Italy to?",x=0.55,y=0.1,
             fontfamily = "Blackwidow",size=65)+
 draw_line(x=c(0.715,0.985),y=c(0.52,0.52),size=45,
           color="darkcyan")+
  
 draw_label("The history of Italian spiders formally begins in 1868,\na list of 404 species were reported at the time.\nThere were unbalanced discoveries between \nnorthen and southern Italy.\nKnowledge on Italian spiders increased rapidly, \nbetween 1901-1951\nSpiders are mostly found in Alto-Adige, Valle D'Aosta,\nLombardia,Veneto,Calabria and Sardegna.",
             x=0.85,y=0.52,size=10,color="white",
            fontfamily="Roboto Condensed")+
  draw_plot(tax_tb,scale=0.37,x=0.35,y=0.3)+
  draw_image("w50/arages_small.png",scale=0.1, x=-0.44,y=0.35)+
  draw_image("w50/ESA.jpg",scale=0.08,x=-0.44,y=0.25)+
  draw_image("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png",
             scale=0.1,x=-0.44,y=0.45) 

####### SAVING ######################################
ragg::agg_png(here::here("w50/spiders.png"),
              res = 320, width = 12, height = 8, units = "in")
final_plot

dev.off()
