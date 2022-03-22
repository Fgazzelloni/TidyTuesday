library(wordcloud2) 

# install webshot
library(webshot)
webshot::install_phantomjs()


df2 <- df %>%
  rename(word=name,freq=n)



# Make the graph
my_graph <- wordcloud2(df2, size=1.5,
                       backgroundColor = "black")


# save it in html
library("htmlwidgets")
saveWidget(my_graph,"data/2022/w12_babynames/tmp.html",selfcontained = F)

# and in png or pdf
webshot("data/2022/w12_babynames/tmp.html",
        "data/2022/w12_babynames/fig_1.png", 
        delay =5, vwidth = 980, vheight=950)



