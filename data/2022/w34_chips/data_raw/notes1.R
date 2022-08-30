

# TidyTuesday 2022 week 34

library(tidyverse)

chips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-23/chips.csv')

chips%>%head
chips%>%DataExplorer::profile_missing()

chips_1 <- chips %>%
  drop_na() 
chips_1 %>% 
  pivot_longer(cols=2:4,names_to="profile",values_to="values") %>%
  ggplot(aes(x=year)) +
  geom_point(aes(y=values,color=profile))


chips_1 %>% 
  ggplot(aes(x=year)) +
  geom_col(aes(y=process_size_nm),color="yellow") +
  #geom_point(aes(y=transistors_million),color="red") +
  geomtextpath::geom_textline(aes(y=tdp_w),label="Thermal Design",color="blue")
  geomtextpath::geom_textline(aes(y=die_size_mm_2),label="Thermal Design",color="blue")
  geom_point(aes(y=tdp_w),color="blue") +
  geom_point(aes(y=die_size_mm_2)) 


