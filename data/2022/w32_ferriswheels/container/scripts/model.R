# setwd("~/Documents/R/R_general_resources/TidyTuesday/data/2022/w32_ferrywheels")

library(tidyverse)
wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv')

my_df <- wheels%>% 
  mutate(costs=stringr::str_extract(construction_cost, "\\d+\\.*\\d*"),
         costs=as.numeric(costs),
         tickets=stringr::str_extract(ticket_cost_to_ride, "\\d+\\.*\\d*"),
         tickets=as.numeric(tickets),
         opened=as.Date(opened,"%Y-%m-%d"),
         open_yr=lubridate::year(opened))%>%# DataExplorer::profile_missing()%>%View
  rename(id=`...1`)%>%
  select(id,country,open_yr,
         costs,tickets,
         diameter,height,
         number_of_cabins,
         seating_capacity)%>%
  arrange(country)

my_df%>%filter(is.na(country))

################################################
# distributions are skewed we use trees to model and impute missing values
my_df%>%
  ggplot(aes(diameter))+
  geom_histogram()

my_df%>%
  ggplot(aes(height,diameter))+
  geom_point()
################################################
my_df%>%DataExplorer::profile_missing()

library(tidymodels)
tidymodels_prefer()


set.seed(1234)
split <- initial_split(my_df,strata = costs,prop = 0.9)
training <- training(split)
test <- testing(split)

set.seed(4567)
folds_costs <- vfold_cv(training, v=10, strata = costs)

################################################
rf_costs_spec <-rand_forest(trees = tune(),  
                            min_n = tune(),
                            mtry = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger", seed = 63233)
################################################

rec_costs <- recipe(costs~.,training) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_impute_bag(costs,tickets,
                  open_yr,diameter,
                  height,seating_capacity,
                  number_of_cabins,
                  trees = 100,
                  seed_val = 1111) %>% 
  step_normalize(all_numeric_predictors()) #%>% 
  #step_pca(all_predictors(),num_comp = 5,threshold = 0.75) %>%
  #prep()%>%juice()

# saveRDS(rec_costs,"rec_costs.rds")
################################################

wkf_costs <- workflow() %>%
  add_model(rf_costs_spec) %>%
  add_recipe(rec_costs)

################################################

doParallel::registerDoParallel()

res_costs <- tune_grid(
  object = wkf_costs,
  resamples = folds_costs,
  # tuning parameters can be manually specified with expand_grid()
  # or a grid number can be specified
  grid = 10,
  control = control_grid(save_pred = TRUE,
                         save_workflow = TRUE,
                         verbose = T,
                         parallel_over = "everything"))


# saveRDS(res_costs,here::here("data/2022/w32_ferriswheels/container/data/res_costs.rds"))
res_costs <- readRDS(here::here("data/2022/w32_ferriswheels/container/data/res_costs.rds"))
################################################
res_costs%>%select_best("rmse")
# mtry trees min_n .config              
# <int> <int> <int> <fct>                
#   1    19   969     5 Preprocessor1_Model04
################################################
my_df1 <- my_df%>%
  filter(!is.na(costs))
my_df2 <- my_df%>%
  filter(is.na(costs))


last_fit_costs <- wkf_costs %>%
  finalize_workflow(select_best(res_costs)) %>%
  last_fit(split)
  
 
last_fit_costs %>%
  collect_metrics()
# .metric .estimator .estimate .config             
# <chr>   <chr>          <dbl> <fct>               
#   1 rmse    standard    76.9     Preprocessor1_Model1
# 2 rsq     standard     0.00978 Preprocessor1_Model1

last_fit_costs %>%
  collect_predictions()%>%
  ggplot(aes(costs,.pred))+
  geom_point()+
  geom_smooth()


#my_df%>%filter(is.na(costs))%>%select(id,costs)%>%arrange(id)
########  ########  ########  ########  ########  ########
########  ########  ########  ########  ########  ########
 data_costs<-  recipe(costs~tickets+open_yr+diameter+height+seating_capacity+number_of_cabins,my_df) %>%
    step_impute_bag(costs,tickets,open_yr,diameter,
                    height,seating_capacity,
                    number_of_cabins,
                    trees = 100,
                    seed_val = 1111)%>%
    prep()%>%
    juice()

ggplot(data_costs,aes(costs,tickets))+
  geom_point()+
  geom_smooth()
  

  
  
  