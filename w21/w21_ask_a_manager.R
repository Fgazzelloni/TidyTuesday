# Ask a Manager Survey



library(tidyverse)
library(DataExplorer)
options(scipen=999)
library(ggthemes)


tuesdata <- tidytuesdayR::tt_load(2021, week = 21)
survey <- tuesdata$survey


head(survey)
profile_missing(survey)

my_df<-survey%>%select(2,4,6,8,11,14,15)

plyr::count(my_df$currency)

my_currency<-c("EUR","GBP","USD")

my_df_curr<-my_df%>%filter(currency%in%my_currency,annual_salary<3000000)

str(my_df_curr)
names(my_df)
plyr::count(s$years_of_experience_in_field)

s<-my_df%>%
  mutate(how_old_are_you=case_when(
    how_old_are_you=="65 or over"~"65+",
    how_old_are_you=="under 18"~"<18",
    TRUE~how_old_are_you),
    years_of_experience_in_field=case_when(
        years_of_experience_in_field=="1 year or less"~"<=1",  
    TRUE~sub(" years$","",years_of_experience_in_field)))
    
    
   s<- s%>%
      mutate(years_of_experience_in_field=case_when(
        TRUE~gsub(" ","",years_of_experience_in_field)
    ))
  


# fit<-lm(annual_salary~job_title+ how_old_are_you+ country+overall_years_of_professional_+years_of_experience_in)


ggplot(data=my_df_curr,
       aes(x=currency,y=annual_salary,group=currency,fill=currency)) + 
  geom_boxplot() + 
  theme(legend.position = "none")


ggplot(data=my_df_curr,
       aes(annual_salary,group=currency,fill=currency)) + 
  geom_histogram(bins = 40,  stat = "bin",position = "stack")+
  #geom_smooth(aes(annual_salary~currency+how_old_are_you)) + 
  theme_calc()+
  theme(legend.position = "none")


ggplot(data=my_df_curr,
       aes(annual_salary,group=currency,fill=currency)) + 
  geom_density()+
  #geom_smooth(aes(annual_salary~currency+how_old_are_you)) + 
  theme_calc()+
  theme(legend.position = "none")

library(ggridges)
library(viridis)
library(hrbrthemes)

plyr::count(my_df_curr$job_title)

# Plot
ggplot(my_df_curr, 
       aes(x = annual_salary, y = currency, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Salary') + xlim(0,280000)+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )


plyr::count(my_df_curr$overall_years_of_professional_experience)

ggplot(my_df_curr, 
       aes(x = annual_salary, y = overall_years_of_professional_experience, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Salary') + xlim(0,280000)+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )


plyr::count(my_df_curr$how_old_are_you)
ggplot(my_df_curr, 
       aes(x = annual_salary, y = how_old_are_you, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Salary') + xlim(0,400000)+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )

library(lubridate)

my_df_curr%>%
 

  ggplot(s,aes(x = annual_salary, y = , fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Salary') + xlim(0,400000)+
  theme_ipsum() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  )




library(forcats)


# Plot
my_df_curr %>%
  mutate(text = fct_reorder(text, value)) %>%
  ggplot( aes(y=text, x=value,  fill=text)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)")

