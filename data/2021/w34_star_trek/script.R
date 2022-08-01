library(tidyverse)
library(jsonlite)

url <- "http://www.speechinteraction.org/TNG/teaearlgreyhotdataset.json"

raw_json <- parse_json(url(url))

raw_json %>% 
  listviewer::jsonedit()

clean_df <- raw_json %>% 
  enframe() %>% 
  unnest_longer(value) %>% 
  unnest_wider(value) %>% 
  unnest_longer(type) %>% 
  unnest_longer(domain) %>% 
  unnest_longer(`sub-domain`) %>% 
  janitor::clean_names()

# clean_df %>% write_csv("2021/2021-08-17/computer.csv")