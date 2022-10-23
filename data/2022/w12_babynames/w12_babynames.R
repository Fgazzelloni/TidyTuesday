
# TidyTuesday week12 BabyNames
# source: https://r-graph-gallery.com/wordcloud

library(tidyverse)

babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

df <- babynames %>%
  arrange(-n) %>%
  select(name,n,prop) %>%
  group_by(name) %>%
  summarise(n=sum(n),prop=sum(prop))%>%
  ungroup() %>%
  arrange(-n) %>%
  slice(1:200)

library(wordcloud)
set.seed(123)

par(bg="black") 
wordcloud(df2$word ,size= df2$freq, 
          col=terrain.colors(length(df2$word),alpha=0.9), 
          rot.per=0.3)



