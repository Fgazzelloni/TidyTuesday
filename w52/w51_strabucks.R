library(tidyverse)

starbucks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-21/starbucks.csv')


head(starbucks)


my_coffee<-starbucks%>%count(product_name,sort=T)%>%slice(1:10)%>%select(-n)%>%unlist()



library(extrafont)
library(showtext)
showtext::showtext_auto()
showtext::showtext_opts(dpi=110)
library(sysfonts)
font_add_google(name ="Black Han Sans" ,family = "my_font")
font_add_google(name ="Odibee Sans" ,family = "my_font1")

family = "my_font"
family1 = "my_font1"


image <- "w52/cup.png"

data<- starbucks%>%
  mutate(trans_fat_g=as.numeric(trans_fat_g),fiber_g=as.numeric(fiber_g))%>%
  select(-size,-milk,-serv_size_m_l)%>%
  filter(product_name%in%my_coffee)%>%
  arrange(product_name)%>%
  mutate(product_name =fct_reorder(product_name,-calories)) %>%
  group_by(product_name)%>%
  summarise_all(.funs = mean)%>%
  ungroup()%>%
  tibble::column_to_rownames("product_name")%>%
  as.matrix()%>%
  scale()%>%
  as.data.frame()%>% 
  rownames_to_column()%>%
  rename(product=rowname)%>%
  mutate(product=as.factor(product))%>%
  pivot_longer(cols=-product,names_to="names",values_to="values")%>%
  mutate(names=gsub("_g|_mg","",names),
         names=gsub("_"," ",names),
         names=gsub("total ","",names))%>%
  mutate(names=ifelse(names=="saturated fat",yes = "satur fat",no = names))%>%
  mutate(names=tools::toTitleCase(names),
         values=round(values,5))%>%
  mutate(img = image)%>%
  group_by(product)%>%
  mutate(values2 =ifelse(values==max(values),values,NA),
         values3 =ifelse(values==min(values),values,NA))%>%
  ungroup()%>%
  mutate(imagemax=ifelse(!is.na(values2),img,NA),
         imagemin=ifelse(!is.na(values3),img,NA))

library(RColorBrewer)
  my_hmap<- data%>%
    ggplot(mapping=aes(x=fct_reorder(names,values),y=fct_reorder(product,values),
           fill=factor(values)))+
  geom_tile(show.legend = F) +
  ggimage::geom_image(aes(image=imagemax,scale=values),
                      asp = 1.5, size = 0.1, by = "width") +
  ggimage::geom_image(aes(image=imagemin,scale=values),
                      asp = 1.5, size = 0.1, by = "width") +
  labs(title=" ")+
  scale_fill_manual(values=colorRampPalette(brewer.pal(5, "BrBG"))(98))+
  scale_color_manual(values=colorRampPalette(brewer.pal(5, "BrBG"))(98))+
  theme_void()+
  theme(text = element_text(family=family1,color="#F5F5F5"),
        axis.title = element_blank(),
        axis.text.y = element_text(vjust=0,hjust=0.95,size=16),
        axis.text.x = element_text(size=11,vjust=0.5),
        plot.background = element_blank(),
        panel.background = element_blank())
  


# build a nice legend
library(circlize)
col_fun = circlize::colorRamp2(c(0, 0.5, 1), c("#A6611A", "#F5F5F5", "#018571"))
lgd = ComplexHeatmap::Legend(at = c(0, 0.5, 1),
                             labels = c("  Low", "", "  High"),
                             col_fun = col_fun, 
                             grid_width = unit(3.4, "cm"),
                             labels_gp = gpar(col = "#F5F5F5", 
                                              fontsize=11,
                                              fontface="bold"))
 
ComplexHeatmap::draw(lgd)
dev.off()

library(cowplot)
library(grid)
leg<-grid.grabExpr(ComplexHeatmap::draw(lgd))
legend<-plot_grid(leg)
class(legend)

# remove bg from pictures
# https://www.remove.bg/upload

 # assemble all the pieces
 library(cowplot)
final<- ggdraw()+
  draw_image(image="w52/bg.jpg",scale=1.5,x=0)+
  draw_image("w52/title.png",x=-0.05,y=0.35,scale=0.9)+
  draw_plot(my_hmap,scale=0.83,x=0.05,y=-0.05)+
  draw_label("STARBUCKS 10' COFFEE",x=0.48,y=0.9,size=55,color="black",
             fontfamily = family,fontface = "bold")+
  draw_image("w52/cup.png",scale=0.5,x=-0.435,y=-0.25)+
  draw_plot(legend,x=-0.422,y=-0.28)+
  draw_label("STARBUCKS",x=0.067,y=0.17,
             size=9,color="black",fontfamily = family)+
  draw_label("DataSource: Starbucks Coffee Company | DataViz: Federica Gazzelloni",
             x=0.3,y=0.03,size=11,color="#F5F5F5",
             fontfamily = family1)+
  draw_image("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png",
             x=0.45,y=0.45,scale=0.09)

####### SAVING ######################################
ragg::agg_png(here::here("w52/starbucks.png"),
              #res = 320, 
              width = 1200, 
              height = 675, 
              units = "px",
              #pointsize = 12,
              background = "white",
              scaling = 1)
final

dev.off()



