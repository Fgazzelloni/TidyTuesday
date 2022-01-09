# TidyTuesday Week37 - FORMULA1

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2021, week = 37)

results <- tuesdata$results
qualifying <- tuesdata$qualifying
constructors <- tuesdata$constructors
drivers <- tuesdata$drivers


my_df <- qualifying%>%
  left_join(results%>%select(raceId,driverId,constructorId,points), by=c("raceId","driverId","constructorId")) %>%
  left_join(constructors,by="constructorId") %>%
  left_join(drivers%>%select(driverId,forename,surname),by="driverId") %>%
  count(qualifyId,raceId,driverId,forename,surname,constructorId,name,number,position,points,sort=T) %>%
  unite("driver_name",forename:surname,sep=" ",remove=TRUE) %>%
  count(driverId,driver_name,constructorId,name,number,position,points)%>%
  arrange(position,-number,-points) %>%
  filter(position<=10)


rank_df<- my_df%>%
  group_by(name,position)%>%
  summarize(total=sum(points),.groups="drop")%>%
  ungroup()%>%
  arrange(position) %>%
  pivot_wider(values_from = total,names_from=position)

rank_df[is.na(rank_df)]<-0


# rank_df <- column_to_rownames(rank_df,var = "driver_name")

rank_df

#library(ggbump)
library(ggrepel)
library(extrafont)
#fonts()

rank_df_long <- rank_df %>%arrange(-`1`)%>% slice(1:10)%>%
  pivot_longer(cols = 2:11,names_to="position",values_to="points")%>%
  mutate(position=as.numeric(position),
         position=factor(position))

summary_first_position <- rank_df_long%>%filter(name==c("Mercedes","Red Bull","Ferrari"))%>%filter(position==1)%>%summary()

library(hrbrthemes)
library(GGally)
library(viridis)

colors <- c("#0EED4D", "#008B00", "#17B6EB",
            "#C00000" , "#FF8700", "#00D2BE",
            "#0600EF", "#FFF500", "#E68C17", "#0082FA")

colors2 <- c("grey70", "grey70", "grey70",
            "#C00000" , "#FF8700", "#00D2BE",
            "#0600EF", "grey70", "grey70", "grey70")


plot <- rank_df_long%>%
  ggplot(aes(x=fct_reorder(position,-position),y=points,group=name,color=factor(name)))+
  geom_point(aes(size=points),shape = 21, fill = "white",  stroke = 5)+
  geom_line(size=2)+
  scale_color_manual(values = colors2)+
  labs(y="Points",x="Positions",
       color="Top 10 Constructors",size="Points")+
  theme_gray()+
  theme(text = element_text(family="Impact",color="grey90"),
        plot.margin = margin(1,1,1,1,unit = "pt"),
        panel.grid.major.y = element_line(size=2),
        panel.background = element_rect(color="grey20",fill="grey20"),
        plot.background = element_rect(color="grey20",fill="grey20"),
        legend.key = element_blank(),
        legend.background = element_blank(),
        legend.text = element_text(color="grey90",size=14),
        legend.title = element_text(color="grey90",size=14),
        axis.text = element_text(color="grey90"),
        axis.title = element_text(size=14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.y = element_blank()
  )


library(ggpubr)
graphics <- ggarrange(plot)+
  theme(plot.background = element_rect(fill="#228B22", color = NA))

final_plot <- annotate_figure(graphics,
                              top = text_grob("FORMULA 1 WINNER CONSTRUCTORS",
                                              color = c("red"), face = "bold", size = 45,
                                              family = "Impact"),
                              bottom = text_grob("Infographics Federica Gazzelloni DataSource: Ergast API, TidyTuesday week37",
                                                 color = "black",family = "Impact",
                                                 hjust = 0.5, x = 0.5, face = "bold.italic", size = 20),
                              left = text_grob("FORMULA 1", color = c("#778899"), rot = 90,size = 30),
                              right = text_grob(bquote("FORMULA1"), color = c("#778899"),rot = 90,size = 30),
                              fig.lab = "TidyTuesday week37", fig.lab.face = "bold.italic",fig.lab.size = 12,
                              fig.lab.pos = "bottom.right"
)



final_plot <-
  final_plot +

  annotate(geom = "text", label = "Lewis Hamilton\nNico Rosberg\n Valtteri Bottas \n- Mercedes",
           x = 0.11, y = 0.85,colour = "#00D2BE",size = 4,family = "Impact") +
  annotate(geom = "curve", x = 0.07, xend = 0.09, y = 0.85, yend = 0.76, colour = "#00D2BE", curvature = .3, arrow = arrow(length = unit(2, "mm")),family = "Impact",size=1.5) +

  annotate(geom = "text", label = "Sebastian Vettel\nMark Webber\nMax Verstappen - Red Bull",
           x = 0.23, y = 0.49,colour = "#0600EF",size = 4.5,family = "Impact") +
  annotate(geom = "curve", x = 0.25, xend = 0.28, y = 0.53, yend = 0.69, colour = "#0600EF", curvature = -.3, arrow = arrow(length = unit(2, "mm")),family = "Impact",size=1.5) +

  annotate(geom = "text", label = "Charles Leclerc\nFernando Alonso\n - Ferrari",
           x = 0.1, y = 0.63,colour = "#C00000",size = 4,family = "Impact") +
  annotate(geom = "curve", x = 0.09, xend = 0.10, y = 0.67, yend = 0.70, colour = "#C00000", curvature = -.3, arrow = arrow(length = unit(2, "mm")),family = "Impact",size=1.5) +

  annotate(geom = "text", label = "F1",x = 0.85, y = 0.88, colour = "red", size = 5,family = "Impact") +

  annotate(geom = "text", label = "MERCEDES, RED BULL and FERRARI\n reached the higest level of points\n at being number one\nwith an average of 418.8 points",x = 0.7, y = 0.81, colour = "white", size = 7,family = "Impact")+


  annotate(geom = "text", label = "Lewis Hamilton - McLaren",x = 0.7, y = 0.53, colour = "#FF8700", size = 5,family = "Impact") +
  annotate(geom = "curve", x = 0.68, xend = 0.64, y = 0.52, yend = 0.41, colour = "#FF7F00", curvature = -.3, arrow = arrow(length = unit(2, "mm")),family = "Impact",size=1.5)

library(ggimage)
library(magick)
library(cowplot)


logo_f1_img <- image_read(here::here("w37/F1.svg.png"))


final <- ggdraw() +
  draw_plot(final_plot) +
  draw_image(logo_f1_img, x = 0.1, y = 0.47,width = 0.12)



## Save final plot ----

ragg::agg_png(here::here("w37/formula1_second_version.png"),
              res = 320, width = 16, height = 8, units = "in")
final

dev.off()
