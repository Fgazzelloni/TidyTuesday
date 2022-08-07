
## This script is to make the ROC plot for Many Models.


library(tidyverse)
library(yardstick)

grid_results <- readRDS(here::here("data/2022/w31_frogs/container/models/grid_results.rds"))

roc <- grid_results %>%
  unnest(result) %>%
  unnest(.predictions) %>%
  select(wflow_id,.pred_0,.pred_1,.pred_class,female) %>%
  group_by(wflow_id) %>%
  roc_curve(female, .pred_0) 

roc_curves <- roc %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity,group=wflow_id )) +
  geom_line(size = 0.5,color="gray") +
  geom_line(data=roc %>% filter(str_detect(wflow_id,"rf")),
            aes(color = wflow_id),
            inherit.aes = T,
            size = 0.5) +
  geom_abline(lty = 2, alpha = 0.5,
              color = "gray50",
              size = 0.8) +
  labs(title="Is female or male?",
       subtitle="Among many models Random Forest with interaction terms or with splines are the best\nidentifying the next Rana Pretiosa by gender.",
       color="Models",
       caption = "Models id identify the type of model with used recipe.\nRandom Forest models are colored, other models used are KNN and Logistic Regression.")+
  ggthemes::theme_fivethirtyeight()+
  theme(text=element_text(family="Roboto Condensed"),
        plot.background = element_rect(color="white",fill="white"),
        panel.background = element_rect(color="white",fill="white"),
        legend.background = element_rect(color="grey95",fill="grey95"),
        legend.box.background = element_blank(),
        legend.position = "none")


roc_curves

# ggsave(here::here("data/2022/w31_frogs/container/images/roc_plot.png"),dpi=320)

