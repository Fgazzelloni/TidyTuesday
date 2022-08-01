library(tidyverse)
library(ggdendro)
library(dendextend)
library(ggraph)
library(tidygraph)
library(purrr)
library(rlang)


# tuesdata <- tidytuesdayR::tt_load(2021, week = 36)
# bird_baths <- tuesdata$bird_baths


bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')
bird_baths <- bird_baths %>%
  drop_na() %>%
  filter(bird_count>0)  # %>% count(bird_type,sort=TRUE) %>% View()


survey_year_id <- bird_baths %>% count(survey_year) %>%
  mutate(survey_year_id = row_number()) %>% select(-n)
urban_rural_id <- bird_baths %>% count(urban_rural) %>%
  mutate(urb_rul_id = row_number()) %>% select(-n)
bioregions_id <- bird_baths %>% count(bioregions) %>% 
  mutate(bioregions_id = row_number()) %>% select(-n)
bird_type_id <- bird_baths %>% count(bird_type) %>% 
  mutate(bird_type_id = row_number()) %>% select(-n)


bird_baths_numeric <- bird_baths %>%
  inner_join(survey_year_id,
             by="survey_year") %>%
  inner_join(urban_rural_id,
             by="urban_rural") %>%
  inner_join(bioregions_id,
             by="bioregions") %>%
  inner_join(bird_type_id,
             by="bird_type") %>%
  count(survey_year_id,urb_rul_id,bioregions_id,bird_type_id) %>%
  arrange(survey_year_id,urb_rul_id,bioregions_id,bird_type_id) 


bird_baths_half_numeric <- bird_baths %>%
  inner_join(survey_year_id,
             by="survey_year") %>%
  inner_join(urban_rural_id,
             by="urban_rural") %>%
  inner_join(bioregions_id,
             by="bioregions") %>%
  inner_join(bird_type_id,
             by="bird_type") %>%
  count(survey_year,survey_year_id,
        urban_rural,urb_rul_id,
        bioregions,bioregions_id,
        bird_type,bird_type_id) %>%
  arrange(survey_year) 

bird_baths_numeric_short <- bird_baths %>%
  inner_join(survey_year_id,
             by="survey_year") %>%
  inner_join(urban_rural_id,
             by="urban_rural") %>%
  inner_join(bioregions_id,
             by="bioregions") %>%
  inner_join(bird_type_id,
             by="bird_type") %>%
  count(survey_year_id,urb_rul_id,bioregions_id,bird_type_id) %>% #View()
  filter(n<=20)


bird_baths_half_numeric_short <- bird_baths %>%
  inner_join(survey_year_id,
             by="survey_year") %>%
  inner_join(urban_rural_id,
             by="urban_rural") %>%
  inner_join(bioregions_id,
             by="bioregions") %>%
  inner_join(bird_type_id,
             by="bird_type") %>%
  count(survey_year,survey_year_id,
        urban_rural,urb_rul_id,
        bioregions,bioregions_id,
        bird_type,bird_type_id) %>% #View()
  filter(n<=20)

bb_piv_w <- bird_baths_half_numeric %>%
  count(survey_year,bird_type,urban_rural,bioregions) %>%
  pivot_wider(names_from=c(survey_year,urban_rural,bioregions),values_from=n,values_fill = 0)



#-------------

bioregions_labels<- data.frame(bird_baths_half_numeric) %>%
  mutate(bioregions=as.factor(bioregions)) 
bioregions_labels <- bioregions_labels[,5]
class(bioregions_labels)

library(colorspace) # get nice colors

bioregions_col <- rev(rainbow_hcl(10))[as.numeric(unique(bioregions_labels))]

#-------

library(tidymodels)
tidymodels_prefer()
set.seed(666)
split <- initial_split(bird_baths_numeric,
                       prop = 3/4,
                       strata = survey_year_id)

train <- training(split);dim(train)
test <- testing(split);dim(test)



to_lables <- bird_baths_half_numeric %>% 
  count(bird_type,bird_type_id)%>%
  select(-n)


# Make 2 dendrograms
d1 <- test %>%
  filter(survey_year_id==1) %>% 
  select(-survey_year_id) %>%
  scale() %>% #cbind(id_1) %>%
  dist(method="manhattan") %>% 
  hclust( method="ward.D2" ) %>% 
  as.dendrogram()


d2 <- test %>%
  filter(survey_year_id==2) %>% 
  select(-survey_year_id) %>%
  scale() %>% #cbind(id_2) %>%
  dist(method="manhattan") %>% 
  hclust( method="ward.D2" ) %>% 
  as.dendrogram()


# dendextend
dl <- dendlist(
  d1 %>% 
    set("labels") %>%
    set("labels_to_character") %>%
    set("labels_col", value = bioregions_col, k=10) %>%
    set("branches_lty", 1) %>%
    set("branches_k_color", value = bioregions_col, k = 10),
  d2 %>% 
    set("labels") %>%
    set("labels_to_character") %>%
    set("labels_col", value = bioregions_col, k=10) %>%
    set("branches_lty",1) %>%
    set("branches_k_color", value = bioregions_col, k = 10)
)



# Plot them together
tanglegram(dl, 
           common_subtrees_color_lines = TRUE, 
           highlight_distinct_edges  = TRUE, 
           highlight_branches_lwd=TRUE, 
           margin_inner=3,
           lwd=1.5,
           main = "Bird Baths Survey Year Tanglegram",
           main_left = "2014 Urban and Rural Bioregions",
           main_right = "2015 Urban and Rural Bioregions",
           sub = "Infographics Federica Gazzelloni",
           cex_main = 2,
           cex_main_left = 1.5,
           cex_main_right = 1.5,
           cex_sub = 1,
           hang = FALSE,
           lab.cex = 0.6
)


#----------
#------