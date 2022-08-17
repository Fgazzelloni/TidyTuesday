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
  prep()%>%juice()

saveRDS(rec_costs,"rec_costs.rds")
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
  control = control_grid(save_pred = TRUE)
)

saveRDS(res_costs,"res_costs.rds")
################################################
res_costs%>%select_best("rmse")


wkf_costs %>%
  finalize_workflow(select_best(res_costs)) %>%
  fit(data = training) %>%
  augment(new_data=test)




  ########  ########  ########  ########  ########  ########
  ########  ########  ########  ########  ########  ########
  