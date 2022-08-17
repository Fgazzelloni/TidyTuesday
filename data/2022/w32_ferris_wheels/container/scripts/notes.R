
wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')
library(tidyverse)
my_df <- wheels%>% 
  filter(!str_detect(name,"Nippon"))%>% # excluding in construction
  #select(name,country,location,construction_cost,ticket_cost_to_ride,height,diameter)%>%
  mutate(temp=ifelse(name=="Big O",height,diameter),
         height=ifelse(name=="Big O",diameter,height),
         diameter=ifelse(name=="Big O",temp,diameter),
         costs=stringr::str_extract(construction_cost, "\\d+\\.*\\d*"),
         costs=as.numeric(costs),
         tickets=stringr::str_extract(ticket_cost_to_ride, "\\d+\\.*\\d*"),
         tickets=as.numeric(tickets),
         location=case_when(name=="The Dubai Eye"~"Bluewaters Island",
                            TRUE~location),
         med_bs_hg=median(height-diameter,na.rm = T),
         sintetic_diameter=ifelse(is.na(diameter),height-med_bs_hg,diameter),
         sintetic_height=sintetic_diameter+med_bs_hg,
         circumference=pi*sintetic_diameter,
         distance=circumference/number_of_cabins,
         teta=(distance*180)/(pi*sintetic_diameter/2),
         sintetic_n_cab=circumference/distance)%>%
        # sintetic_num_cabins=ifelse(is.na(number_of_cabins),
        #                            sintetic_diameter*(sintetic_diameter/()))) %>% #,diameter=ifelse(is.na(diameter),height,diameter))%>%
  select(-construction_cost,-ticket_cost_to_ride)%>%
  arrange(country)


 my_df%>%
   count(name,circumference,sintetic_diameter,
         number_of_cabins,sintetic_n_cab,distance,teta)%>%
   arrange(circumference)%>%
   View
 
 my_df%>%summary()

 my_df%>%
  #group_by(country)%>%
  mutate(sintetic_number_of_cabins=median(height-diameter,na.rm = T))%>%count(med_bs_hg)
#my_df%>%glimpse
# my_df%>%DataExplorer::profile_missing()%>%View
library(tidymodels)
knn <-recipe(costs~.,my_df)%>%
  step_zv(all_predictors()) %>% 
  step_impute_(costs,tickets) %>%
  prep()%>%juice()%>% # DataExplorer::profile_missing()%>%View
  select(name,country,location,sintetic_height,sintetic_diameter,costs,tickets,number_of_cabins)%>%
  arrange(country) %>%#DataExplorer::profile_missing()
  rename(knn_costs=costs,
         knn_tickets=tickets,
         knn_diameter=sintetic_diameter,
         knn_height=sintetic_height,
         knn_number_of_cabins=number_of_cabins)


knn1 <- knn %>%
  mutate(y=knn_height-knn_diameter/2,
         r = knn_diameter / 2,
         .after=location) %>%
  group_by(country) %>%
  mutate(country_id=1,
         name=paste0(name,"\n",country),
         tot_wheels=sum(country_id),.after=country)%>%#count(tot_wheels,sort=T)
  # filter(tot_wheels>6)%>%
  ungroup() %>%
  mutate(country=as.character(country))
  
# knn1%>%
#   select(-country,-location,-name,-country_id) %>%
#   as.matrix()%>%
#   scale(scale=T,center = F) %>%
#   as_tibble() %>%
#   cbind(name=knn$name,
#         country=knn$country) 

#knn1%>%names


 cabin <- knn1 %>% 
   group_by(name) %>% # summarise(number_of_cabins)
   summarise(cabin = seq_len(knn_number_of_cabins),
             # Get x and y for the carts
             cabin_x = cos(cabin / knn_number_of_cabins * 2 * pi),
             cabin_y = sin(cabin / knn_number_of_cabins * 2 * pi),
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


# cabin1<-cabin %>%
# select(-name,-cabin_color) %>%
#   as.matrix()%>%
#   scale(scale=T,center = F) %>%
#   as_tibble() %>%
#   cbind(name=cabin$name,
#         cabin_color=cabin$cabin_color) 


knn1 %>%
  ggplot() +
  geom_abline(slope = 0, intercept = 0, color = "darkgreen") +
  ylim(0, NA) +
  ggforce::geom_circle(aes(x0 = 0, y0 = y, r = r),
                       color="midnightblue") +
  # # Left leg
  geom_segment(aes(x = -(knn_height - knn_diameter / 2)/2, xend = 0,
                   yend = knn_height - knn_diameter / 2, y = 0)) +
  # right leg
  geom_segment(aes(x = (knn_height - knn_diameter / 2)/2, xend = 0,
                   yend = knn_height - knn_diameter / 2, y = 0)) +
  geom_point(data = cabin,
             mapping=aes(cabin_x, cabin_y, 
                         fill = cabin_color,
                         size=knn_diameter), 
             shape = 20) +
  coord_fixed() +
  theme_void()+
  theme(plot.background = element_rect(fill="#9FB3D4",color="#9FB3D4"),
        panel.background = element_rect(fill="#9FB3D4",color="#9FB3D4"),
        legend.position = "none")+
  facet_wrap(~fct_reorder(name,knn_diameter))


  # geom_point(aes(cart_x, cart_y, fill = cart_color), data = carts, shape = 24) +


##################################################
#   SAVING
##################################################
ggsave("imputation12.png",
       dpi=320,
       width = 14,
       height = 14)



##################################################
##################################################


wheels%>% #filter(country=="Russia")%>%
  select(name,country,height,diameter,number_of_cabins)%>%
  mutate(diff=height-diameter)%>%View(
    
  )
