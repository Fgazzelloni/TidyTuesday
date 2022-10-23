# Ask a Manager Survey
rm(list=ls())

library(tidyverse)
library(DataExplorer)
options(scipen=999)



tuesdata <- tidytuesdayR::tt_load(2021, week = 21)
survey <- tuesdata$survey


head(survey)
profile_missing(survey)

my_df<-survey%>%select(2,4,6,8,14,15)


arrange(plyr::count(my_df$currency),freq)

my_currency<-c("EUR","GBP","USD")

country_df<-data.frame(country_name=c("Europe","UK","USA"),
                       currency=c("EUR","GBP","USD"))

my_df_curr<-my_df%>%
  filter(currency%in%my_currency,annual_salary<3000000)%>%
  inner_join(country_df,by="currency")

str(my_df_curr)
names(my_df)


my_df_curr<-my_df_curr%>%
  mutate(how_old_are_you=case_when(
    how_old_are_you=="65 or over"~"65+",
    how_old_are_you=="under 18"~"<18",
    TRUE~how_old_are_you),
    years_of_experience_in_field=case_when(
      years_of_experience_in_field=="1 year or less"~"0-1",
      years_of_experience_in_field=="41 years or more"~"41+",
      TRUE~sub(" years$","",years_of_experience_in_field)),
    overall_years_of_professional_experience=case_when(
      overall_years_of_professional_experience=="1 year or less"~"0-1",
      overall_years_of_professional_experience=="41 years or more"~"41+",
      TRUE~sub(" years$","",overall_years_of_professional_experience)),
    overall_years_of_professional_experience = as.factor(overall_years_of_professional_experience),
    how_old_are_you = as.factor(how_old_are_you),
    years_of_experience_in_field = as.factor(years_of_experience_in_field))%>%
  arrange(desc(how_old_are_you))


my_df_curr<- my_df_curr%>%
  mutate(years_of_experience_in_field=case_when(
    TRUE~gsub(" ","",years_of_experience_in_field)),
    overall_years_of_professional_experience=case_when(
      TRUE~gsub(" ","",overall_years_of_professional_experience))
  )


names(my_df_curr)<-c("age","job_title","annual_salary","currency" ,"yr_prof_exp","yr_exp","country_name")

######### PLOTTING ####################


library(viridis)
library(hrbrthemes)
library(ggthemes)

scr_df<-data.frame(country_name=c("Europe","UK","USA"),
                   scr=c(1/1.22,1/1.41,1))

my_df_curr<-my_df_curr%>%inner_join(scr_df,by="country_name")%>%
  mutate(salaryUSD=annual_salary*scr)

library(extrafont)
fonts()
leg_lab<-c("0-1","2-4","5-7","8-10","11-20","21-30","31-40","40+")
newcolors<-c("#004586","#ffd320","#314004","#aecf00","#ff420e",
             "#579d1c","#7e0021","#83caff")


survey_plot<-ggplot(my_df_curr, aes(x=age, y=salaryUSD)) +
  geom_line(aes(group=age))+
  geom_point(aes(group=yr_exp,col=yr_exp),
            alpha=0.5) +
  geom_text(aes(label=job_title,size=salaryUSD),hjust = 0, nudge_x = 0.05,
            check_overlap = TRUE,family="World of Water") +
  scale_y_continuous(labels = dollar_format(prefix="$"), limits = c(0,NA))+
  scale_color_manual(labels = leg_lab,values = newcolors)+
  labs(title="Who is the Richest?",
       subtitle="Industry job title pyramid annual Salary(USD) by age - Ask a Manager Salary Survey ",
       caption="Viz. @fgazzelloni | DataSource: Ask a Manager Salary Survey | TidyTuesday Week21",
       x="Age", y="Salary(USD)",
       col="Years of experience in the field",
       size="")+
  scale_size(guide=FALSE)+
  theme_calc() +
  theme(plot.margin=unit(c(c(1, 1, 0.5, 0.5)), units="line"),
        legend.position = c(0.83,0.7),
        #legend.justification = c("right", "top"),
        #legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6),
        legend.direction = "horizontal",
        legend.title = element_text(family="World of Water"),
        legend.text = element_text(family="World of Water"),
        legend.background = element_rect(color = "#4b1f6f"),
        plot.title = element_text(family="World of Water",size=rel(4)),
        plot.subtitle = element_text(family="World of Water",size=20),
        plot.caption = element_text(family="World of Water",size=10,hjust = 0.5),
        axis.title = element_text(family="World of Water"),
        axis.text = element_text(family="World of Water"),
        axis.line = element_line(size = 3, colour = "grey80"),
        axis.ticks = element_line(size=2,color="#4b1f6f"))



################################################################################


####### SAVING ######################################
ragg::agg_png(here::here("tidytuesday_Ask_a_manager_survey.png"),
              res = 320, width = 14, height = 8, units = "in")
survey_plot

dev.off()



#### ATTACHING LOGO ############################
library(ggimage)
library(magick)


tidy_logo<-image_read("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/static/plot_logo.png") %>%
  image_resize("300x300")


final_plot <- image_read("tidytuesday_Ask_a_manager_survey.png")

attached_logo <- image_composite(final_plot, tidy_logo,
                                 operator="atop",
                                 gravity="northeast") # tell R where to put the logo


image_write(attached_logo, path = "tidytuesday_Ask_a_manager_survey.png",
            format = "png") # save final plot



##############################################################













