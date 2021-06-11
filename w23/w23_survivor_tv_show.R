# TidyTuesdat Week 23 ##############################

# Survivor TV Show 
# survivoR Package
# https://github.com/doehm/survivoR

# http://gradientdescending.com/survivor-data-from-the-tv-series-in-r/


# install.packages("survivoR")


# source of inspiration: 
# https://github.com/TIvanDijk/TidyTuesday/blob/main/Week%2023/survivors.R
# https://cran.r-project.org/web/packages/ggtext/vignettes/plotting_text.html

library(survivoR)

library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load(2021, week = 23)

challenges <- tuesdata$challenges
head(challenges);dim(challenges)
viewers <- tuesdata$viewers
head(viewers);dim(viewers)
jury_votes <- tuesdata$jury_votes
castaways <- tuesdata$castaways
summary <- tuesdata$summary
head(summary);dim(summary)

#########################


head(castaways);dim(castaways)

library(tidyverse)
library(ggthemes)
library(extrafont)
library(patchwork)
library(ggtext)
library(forcats)
library(ggrepel)
library(showtext)
library(cowplot)
library(magick)
# fonts()

font_add_google("Roboto", "roboto")
font_add_google("Roboto Condensed", "roboto condensed")


##############################################

labels <-
  tibble(
    labels = c(
      "**The 4 Cognitive Functions dealing with '*How Processing Information*'**",
      "Extroverted Sensing = ESTP, ESFP, ISTP, and ISFP                   ",
      "Introverted Sensing = ISTJ, ISFJ, ESTJ, and ESFJ                   ",
      "Extroverted Intuition = ENTP, ENFP, INTP, and INFP                 ",
      "Introverted Intuition = INTJ, INFJ, ENTJ, and ENFJ                 "
      ),
    x = rep(3.5, 5),
    y = c(0.8,0.7,0.6,0.5,0.4)#rep(1, 5)
    )

personality_codes <-ggplot(labels, aes(x, y)) +
  annotate("text", x = 3, y = 1.45,
           label = "Survivor personality code explained",
           size = 14,color="#A91727",
           fontface = "bold",
           family =  "Roboto Condensed" ) +
  geom_text(aes(x=2.8,y=1,family= "Roboto Condensed" ),
            label="The Myers-Briggs Personality Type Indicator is a personality classification system that breaks personalities into 16 different categories. \nEach personality type has specific preferences in how they 1.) perceive the world and how they 2.) make decisions",
            color = 'black', size = 4,face="bold")+
  geom_richtext(aes(label = labels,family= "Roboto Condensed"),
                label.color = NA,
                vjust = 0.8,hjust=0.2,color="#A91727",size=4,fill=NA) +
  annotate(geom = "curve", x = 1.8, y = 0.6, xend = 2.5, yend = 0.75, curvature = -.2, arrow = arrow(length = unit(2, "mm")))+
  annotate(geom = "rect",xmin=0.2,ymin=0.35,xmax =1.8 ,ymax =0.81, color="#FEED01",fill="#FEED01")+
  annotate(geom = "text",x=1,y=0.6,
           label="Each personality type has four cognitive functions:\n1. dominant\n2. auxiliary or secondary\n3. tertiary\n4. inferior or least developed",
           size = 3,color="black",
           family =  "Roboto Condensed")+
  annotate("text",x=1,y=0.25,
           label="Each of the 16 personality types will have a unique combination of 4 out of 8 cognitive functions",
           size = 3,color="black",
           family =  "Roboto Condensed")+
  scale_x_continuous(limits = c(0, 6.1)) +
  scale_y_continuous(limits =  c(0.2, 1.5)) +
  labs(title="Survivor (American TV series)")+
  theme_void()+
  theme(plot.title = element_text(hjust=1,vjust=-0.5,face="bold",family = "Roboto Condensed"),
        plot.background = element_rect(fill = "#EFCC24"),
        panel.border = element_blank(),
        plot.margin = unit(c(1,1,0,1), "cm"))


personality_plot <- castaways%>%
  group_by(personality_type)%>%
  summarize(avg_age=mean(age),n=n())%>%
  ungroup()%>%
  arrange(desc(n))%>%
  drop_na()%>%
  ggplot(aes(x=reorder(round(avg_age),avg_age),y=n,
             color=personality_type,fill=personality_type))+
  geom_col()+
  geom_label(aes(y=n,label=paste(personality_type),fill=personality_type),
            #direction = "y",
            color="white",
            position= position_stack(vjust=0.5))+
  scale_fill_survivor(12)+
  scale_colour_survivor(12)+#8
  labs(title="Personality type by Age group",
       x="Survivor's Age",y="Group(n)",
       fill="Personality Type",color="",
       caption = "Viz @fgazzelloni Source: TidyTuesday week 23 & Survivor TV Show, more info @Daniel's Oehm's Website")+
  guides(color=FALSE)+
 theme_base()+
  theme(
    legend.position = "none",
    plot.background = element_rect(fill="#EFCC24"),
    plot.title = element_text(color="#784937",family="Roboto Condensed"),
    plot.caption = element_text(color="#784937",family="Roboto Condensed",size=9,face="bold"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(),
    axis.title = element_text(family="Roboto Condensed"))



p<-personality_codes / personality_plot


img <- image_read("hex-torch.png")

# Set the canvas where you are going to draw the plot and the image
final <-ggdraw() +
  # Draw the plot in the canvas setting the x and y positions, which go from 0,0
  # (lower left corner) to 1,1 (upper right corner) and set the width and height of
  # the plot. It's advisable that x + width = 1 and y + height = 1, to avoid clipping 
  # the plot
  draw_plot(p,x = 0, y = 0, width = 1, height = 1) +
  # Draw image in the canvas using the same concept as for the plot. Might need to 
  # play with the x, y, width and height values to obtain the desired result
  draw_image(img,x = -0.85, y = 0.8, width = 1.85, height = 0.2)




###################### SAVING ############################


ragg::agg_png(here::here("w23","w23_survivor.png"),
              res = 320, width = 14, height = 8, units = "in")
final

dev.off()



#### ATTACHING LOGO ############################
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


final_plot <- image_read("w23_survivor.png")

attached_logo <- image_composite(final_plot, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "w23_survivor.png",
            format = "png") # save final plot



##############################################################



