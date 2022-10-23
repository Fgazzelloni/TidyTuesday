# TidyTueday Week20 - Eurovision TV
# DataViz: Federica Gazzelloni (@fgazzelloni)

# Remove everything (houskeeping)
rm(list=ls())
# Set the working directory (useful if you are in a .R script)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load the libraries
library(tidyverse)
library(showtext)
library(countrycode)
library(ggpattern)
library(cowplot)

# Set the font
showtext_auto(enable = T)
# sysfonts::font_families_google()
sysfonts::font_add_google("Oregano", "Oregano")

# Read the data
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

# Manipulate the data as needed
df <- eurovision %>%
  group_by(host_country) %>% 
  count(year,artist,song,artist_country,running_order,total_points,rank,qualified,winner) %>%
  ungroup() %>%   
  filter(!is.na(song),
         year>=1971,
         rank<=15) %>% 
  arrange(year)%>%
  mutate(rank_dsc=factor(desc(rank)))%>%
  filter(!host_country%in%c("Serbia & Montenegro", "Yugoslavia")) %>%
  mutate(host_country=case_when(host_country=="United Kingdom"~"UK",
                                host_country=="Bosnia & Herzegovina"~"Bosnia and Herzegovina",
                                TRUE~host_country)) %>%
  mutate(country_code_h = countrycode(host_country, 
                                      origin = 'country.name', 
                                      destination = 'iso2c'),
         country_code_h=tolower(country_code_h))%>%
  group_by(rank) %>%
  mutate(tot=sum(total_points))%>%
  arrange(tot)%>%
  ungroup() %>%
  filter(rank==1) %>%
  group_by(host_country,country_code_h) %>%
  summarize(tot=sum(total_points),.groups="drop")%>%
  arrange(tot) %>%
  ungroup() 

# Add the flags' image
# Source for flags' image https://github.com/lipis/flag-icons

# Take the vector with the countries for setting the flags' image vector
my_codes<-df$country_code_h
# Set the vector with flags' image
flags <- paste0("https://raw.githubusercontent.com/lipis/flag-icons/main/flags/1x1/",my_codes,".svg")

# Add the image vector to the df
mydf <- cbind(df,image=flags)

# Adjust the order of the host country total points and create an id vector for the angles
mydf1 <- mydf%>%
  mutate(host_country=fct_reorder(host_country,-tot))%>%
  mutate(id=seq(1, nrow(mydf)))%>%
  relocate(id) 

# Set the angles
angle <- 90 - 360*(mydf2$id - 0.5)/nrow(mydf2)

# Add a vector with angles  
mydf1$angle <- ifelse(angle< -90, angle + 180, angle)

# Make the circular plot
mydf1 %>%
  ggplot(aes(x=host_country,y=tot)) +
  ggpattern::geom_col_pattern(aes(pattern_filename= rev(host_country)),
                   inherit.aes = T,
                   pattern         = 'image',
                   width                = 1, 
                   pattern_type    = 'none',
                   fill            = '#1e88f7', 
                   colour          = 'white',
                   pattern_scale        = -1,
                   pattern_aspect_ratio = 1,
                   pattern_key_scale_factor = 1,
                   pattern_filter  = 'box',
                   pattern_gravity = mydf2$angle) +
  ylim(-800,2418)+
  coord_polar(start = 0,theta = "x",direction = 1) +
  ggpattern::scale_pattern_filename_discrete(choices = flags) +
  theme_void()+
  theme(legend.position = "none",
        plot.background = element_rect(fill=NA,color=NA),
        panel.background = element_rect(fill=NA,color=NA))

# Save the circular plot
ggsave("circular.png",
       dpi=320,
       bg = "black",
       width = 6,
       height = 6)

# Frame the plot and add annotations
ggdraw() +
  draw_image("circular.png")+
  # add the eurovision logo
  draw_image("Logo-ESC-Generico.jpeg",scale=0.1) +
  draw_label("Eurovision",
             color="white",fontfamily = "Oregano",
             x=0.25,y=0.92,size=180) +
  draw_label("Host Countries",
             color="white",fontfamily = "Oregano",
             x=0.3,y=0.8,size=94) +
  draw_label("The highest total points first from 1971 to 2020\nDataSource: #TidyTueday Week20 - Eurovision TV\nDataViz: Federica Gazzelloni (@fgazzelloni)",
             x=0.5,y=0.1,size=41,lineheight = 0.3,
             color="white",fontfamily = "Oregano")

# Save the final version
ggsave("w20_eurovision.png",
       dpi=320,
       bg = "black",
       width = 6,
       height = 6)

