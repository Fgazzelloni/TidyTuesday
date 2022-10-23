# install.packages("ggfx")
library(ggplot2)
library(ggfx)
ggplot() + 
  as_reference(
    geom_polygon(aes(c(0, 1, 1), c(0, 0, 1)), 
                 colour = NA, fill = 'darkred'), 
    id = "pattern"
  ) + 
  with_displacement(
    geom_text(aes(0.5, 0.5, label = 'Yarn-Yarn'), 
              size = 25, fontface = 'bold'), 
    x_map = ch_red("pattern"), 
    y_map = ch_blue("pattern"),
    x_scale = unit(0.025, 'npc'),
    id = "text"
  ) +
  with_blend(
    geom_density_2d_filled(aes(rnorm(1e4, 0.5, 0.2), 
                               rnorm(1e4, 0.5, 0.2)), 
                           show.legend = FALSE),
    bg_layer = "text",
    blend_type = "in",
    id = "blended"
  ) + 
  with_shadow("blended", sigma = 3) + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = 'off') + 
  labs(x = NULL, y = NULL)+
  theme_void()
