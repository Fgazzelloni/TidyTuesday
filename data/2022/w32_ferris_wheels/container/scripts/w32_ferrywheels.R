# TidyTuesday 2022 week32

setwd("~/Documents/R/R_general_resources/TidyTuesday/data/2022/w32_ferrywheels")

library(tidyverse)
wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

#wheels%>%head
#wheels%>%skimr::skim()
wheels%>%dim # 73 22

# wheels%>%filter(country=="Japan",str_detect(name,"Nippon"))%>%View
# popular approach to missing data imputation is to use a model to predict the missing values
#(https://machinelearningmastery.com/knn-imputation-for-missing-values-in-machine-learning/)

my_df <- wheels%>%
  filter(!str_detect(name,"Nippon"))%>% # excluding in construction
  #select(name,country,location,construction_cost,ticket_cost_to_ride,height,diameter)%>%
  mutate(costs=stringr::str_extract(construction_cost, "\\d+\\.*\\d*"),
         costs=as.numeric(costs),
         tickets=stringr::str_extract(ticket_cost_to_ride, "\\d+\\.*\\d*"),
         tickets=as.numeric(tickets),
         location=case_when(name=="The Dubai Eye"~"Bluewaters Island",
                            TRUE~location),
         diameter=ifelse(is.na(diameter),height,diameter))%>%
  select(-construction_cost,-ticket_cost_to_ride)%>%
  arrange(country)

############################################################
############################################################
library(tidymodels)
bagging<- recipe(costs~.,my_df)%>%
  step_zv(all_predictors()) %>% 
  step_impute_bag(costs,tickets,number_of_cabins,passengers_per_cabin) %>%
  prep()%>%juice()%>% #DataExplorer::profile_missing()
  select(name,country,location,height,diameter,costs,tickets,number_of_cabins)%>%
  arrange(country)

knn <-recipe(costs~.,my_df)%>%
  step_zv(all_predictors()) %>% 
  step_impute_knn(costs,tickets,number_of_cabins,passengers_per_cabin) %>%
  prep()%>%juice()%>%
  select(name,country,location,height,diameter,costs,tickets,number_of_cabins)%>%
  arrange(country)%>%
  rename(knn_costs=costs,knn_tickets=tickets,knn_diameter=diameter,knn_height=height,knn_number_of_cabins=number_of_cabins)

imputed_df <- bagging%>%
  left_join(knn)

imputed_df2 <-imputed_df%>%
  mutate(y=knn_height-knn_diameter/2,
         r = knn_diameter / 2,.after=location) %>%
  group_by(country) %>%
  mutate(country_id=1,
         name=paste0(name,"\n",country),
         tot_wheels=sum(country_id),.after=country)%>%#count(tot_wheels,sort=T)
 # filter(tot_wheels>6)%>%
  ungroup()
  
### SCALED #### #### #### #### #### ####
imputed_df3 <- imputed_df2 %>%
#filter(country=="UK") %>%
  mutate(country=as.character(country))%>%
  select(-country,-location,-name,-number_of_cabins,-tot_wheels) %>%
  as.matrix()%>%
  scale(scale=T,center = F) %>%
  as_tibble() %>%
  cbind(name=imputed_df2$name,
        country=imputed_df2$country,
        number_of_cabins=imputed_df2$number_of_cabins,
        tot_wheels=imputed_df2$tot_wheels) 
# imputed_df3%>%count(name)


imputed_df4 <- imputed_df3 %>%
  pivot_longer(cols=c(height,diameter,
                      knn_height,knn_diameter,
                      costs,tickets,
                      knn_costs,knn_tickets),
               names_to="feature",values_to="feat_values")

#######################################################
#   FEATURES
#######################################################
imputed_df4 %>%
  ggplot(aes(x=feat_values,y=fct_reorder(feature,feat_values)))+
  geom_histogram(aes(fill=feature),stat = "identity")

#######################################################
#  POLAR
#######################################################
imputed_df3 %>%
ggplot(aes(costs,tickets,color=tot_wheels,size=height,alpha=height))+
  geom_smooth(method="lm")+
  geom_smooth(aes(knn_costs,knn_tickets),
              inherit.aes = F,color="yellow",method="lm")+
  geom_point()+
  geom_point(aes(knn_costs,knn_tickets,color=tot_wheels,size=knn_height),
             shape=21,stroke=0.5,
             inherit.aes = F)+
  geom_text(aes(label=country),vjust="top",nudge_y = 0.2)+
  scale_y_log10()+
  scale_x_log10()+
  coord_polar(theta = "x")+
  theme_void()+
  theme(plot.background = element_rect(fill="slategray",color="slategray"),
        panel.background = element_rect(fill="slategray",color="slategray"),
        legend.position = "none")

#######################################################
#    ALL WHEELS
#######################################################
cabin <- imputed_df2 %>% 
  group_by(name) %>% # summarise(number_of_cabins)
  summarise(cabin = seq_len(number_of_cabins),
    # Get x and y for the carts
    cabin_x = cos(cabin / number_of_cabins * 2 * pi),
    cabin_y = sin(cabin / number_of_cabins * 2 * pi),
    # Size them to be the right distance from the center
    cabin_x = cabin_x * (knn_diameter / 2),
    cabin_y = cabin_y * (knn_diameter / 2),
    # Make sure the carts are raised enough
    cabin_y = cabin_y + knn_height - knn_diameter / 2,
    # Lower the carts just a bit so it appears they are hanging
    cabin_y = cabin_y - 12.5,
    cabin_color = as.character(cabin %% 3),
    knn_diameter,
    .groups = "drop"
  )

cabin1 <- cabin %>%
  select(-name,-cabin,-cabin_color)%>%
  as.matrix()%>%
  scale(scale=T,center = F) %>%
  as_tibble() %>%
  cbind(name=cabin$name,
        cabin=cabin$cabin,
        cabin_color=cabin$cabin_color)


imputed_df2 %>%
ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = y, r = r),
                       color="red") +
  geom_point(data = cabin,
             mapping=aes(cabin_x, cabin_y, fill = cabin_color), 
            shape = 24) +
  theme_void()+
  theme(plot.background = element_rect(fill="slategray",color="slategray"),
        panel.background = element_rect(fill="slategray",color="slategray"),
        legend.position = "none")+
  facet_wrap(~fct_reorder(name,knn_diameter),scales = "free")

##################################################
#   SAVING
##################################################
ggsave("imputation9.png",
       dpi=320,
       width = 14,
       height = 15)

########################################################
# scale = 50 gives a different set of names
library(rnaturalearth)
world_map <- ne_countries(scale = 50, returnclass = 'sf')

#world_map%>%select(continent,name)%>%View

# ggplot(world_map)+
#   geom_sf(aes(fill=continent))+
#   coord_sf()

all_countries<-world_map$name  # 241
setdiff(my_countries,all_countries)
# all_countries[str_detect(all_countries,"Korea")]
########################################################
my_df2 <- my_df1%>% # 73
  mutate(country=case_when(country=="S Korea"~"Dem. Rep. Korea",
                           country=="UAE"~"United Arab Emirates",
                           country=="Dubai"~"United Arab Emirates",
                           country=="Phillippines"~"Philippines",
                           country=="Tailand"~"Thailand",
                           country=="UK"~"United Kingdom",
                           country=="USA"~"United States",
                           TRUE~country))



my_countries<-my_df2%>%count(country)%>%select(-n)%>%unlist()
setdiff(my_countries,all_countries)
########################################################

my_world <-world_map%>%  # 25 inclueded Dubai
    select(continent,name) %>%
    filter(name%in%my_countries)
  

my_df3 <- my_df2 %>%
  left_join(my_world,by=c("country"="name")) 
  
my_df3 %>%
  select(-height) %>%
  group_by(country) %>%
  summarize(av_cost=median(costs,na.rm = T),
            av_ticket=median(tickets,na.rm = T),
            av_dia=mean(diameter),
            pct=diameter/sum(diameter),
            error=abs(costs-av_cost),costs)%>%
           # adj_cost=ifelse(is.na(costs),exp(mean(log()))))%>%
  ungroup()%>%View()
  
  
  
median_val <- my_df3 %>%
  group_by(continent) %>%
  summarize(av_cost=median(costs,na.rm = T),
            av_ticket=median(tickets,na.rm = T))%>%
  ungroup()


my_df3 %>%
  left_join(median_val,by="continent")%>%
  mutate(costs_adj=ifelse(is.na(costs),av_cost,costs),
         tickets_adj=ifelse(is.na(tickets),av_ticket,tickets))%>%
  select(country,continent,costs,av_cost,costs_adj,tickets,av_ticket,tickets_adj)%>%
  group_by(country,continent) %>%
  summarise(med_c=median(costs_adj),med_t=median(tickets_adj))%>%
  ungroup()#%>%count(med_c,med_t)%>%map_dfr(median)






