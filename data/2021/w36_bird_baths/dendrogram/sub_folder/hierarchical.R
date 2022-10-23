# Load library
# install.packages("collapsibleTree")
library(collapsibleTree)




Bird.baths <- bird_baths%>%
  drop_na()%>%
  filter(bird_count>0) %>% #count(bird_type)
  count(survey_year,urban_rural,bioregions,bird_type,sort=TRUE)


p1 <- collapsibleTree(Bird.baths, c("survey_year",
                                    "urban_rural",
                                    "bioregions",
                                    "bird_type",
                                    "n"))
p1
# save the widget
library(htmlwidgets)
saveWidget(p1, file=paste0("w36/dendrogram_interactive.html"))



#---------------------

dist <- dist(Bird.baths)

skimr::everything(dist)
sum(is.na(dist))

hc <- hclust(dist)
plot(hc)


dend<- as.dendrogram(hc)
plot(dend)

specific_leaf <- dend[[1]][[1]][[1]]


c(`Survey Year`="survey_year",
  "Urban or Rural"="urban_rural",
  "Bioregions"="bioregions",
  "Bird Type"="bird_type",
  "N.","n"))


# Finally I just have to apply this to my dendrogram
dL <- dendrapply(dend, colLab)

# And the plot
plot(dL , main="structure of the population")
legend("topright",
       legend = c("High Nitrogen" , "Low Nitrogen" , "Durum" , "Dicoccoides" , "Dicoccum"),
       col = c("red", "blue" , "blue" , "red" , "Darkgreen"),
       pch = c(20,20,4,4,4), bty = "n",  pt.cex = 1.5, cex = 0.8 ,
       text.col = "black", horiz = FALSE, inset = c(0, 0.1))













