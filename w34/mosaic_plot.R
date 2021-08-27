
# source: https://www.r-bloggers.com/2020/05/4-great-alternatives-to-standard-graphs-using-ggplot/
  
library("Stat2Data")
data("GrinnellHouses")
library("ggmosaic")

# preparing the data - making broader categories

  
GrinnellHouses <- GrinnellHouses %>% #count(Bedrooms,Baths)
    mutate_at(
  .vars = c("Bedrooms", "Baths"),
  .funs = function(x) factor(ifelse(x>=4, round(x), 4), 
           levels = c(0:4), 
           labels = c(0:3, "4 or more"), 
           ordered = T)
)


table(Bathrooms=GrinnellHouses$Baths, 
      Bedrooms=GrinnellHouses$Bedrooms, exclude = NULL) 

GrinnellHouses%>%
  drop_na()




ggplot(GrinnellHouses) +
  geom_mosaic(aes(x = product(Baths, Bedrooms), fill = Baths)) +
  theme(
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.title.y = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.background = element_blank(),
  legend.position = "left"
)
