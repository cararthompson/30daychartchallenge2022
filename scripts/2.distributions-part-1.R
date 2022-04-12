# Distributions
## Physical
## Mountain
## Circular

library(tidyverse)
library(extrafont)
library(elevatr)
library(ggtext)
library(geomtextpath)

elev_points <- get_elev_point(
  #long = x, lat = y
  data.frame(x = c(-179:179), 
             y = rep(0, 359)), src = "aws",
  prj = "EPSG:4326")



elev_df <- tibble(long = c(-179:179), lat = 0, elev = elev_points$elevation) %>%
  mutate(sea_land = case_when(elev < 0 ~ "sea",
                              TRUE ~"land"))

ggplot(elev_df) +
  geom_rect(aes(xmin = -179, xmax = 179, ymin = 0, ymax = 6000),
            fill = "#c5e3ed", alpha = 0.4) +
  geom_rect(aes(xmin = -179, xmax = 179, ymin = 0, ymax = -64000),
            fill = "#768ca3", alpha = 0.4) +
  geom_rect(aes(xmin = -179, xmax = 179, ymin = -29000, ymax = -51000),
            fill = "#e4792b", alpha = 0.4) +
  geom_rect(aes(xmin = -179, xmax = 179, ymin = -51000, ymax = -64000),
            fill = "#e1bb7b", alpha = 0.4) +
  geom_area(aes(x = long, y = elev), alpha = 0.6,
            fill = "#2a435e", size = 0.1) +
  geom_text(aes(x = -79, y = 6300), label = "*", size = 5, colour = "#e4792b",
            family = "Sintony") +
  geom_line(aes(x = long, y = elev, colour = elev),
            alpha = 0.7, size = 0.3, show.legend = F) +
  geom_segment(aes(x = 89.5, xend = 89.5, y = -64000, yend = -6000), 
               colour = "#c5e3ed", arrow = arrow(angle = 90, length = unit(0.1, "in"))) +
  geom_textbox(aes(x = 89.5, y = -35000), label = "1:100", colour = "#c5e3ed", 
               fill = NA, box.colour = NA, family = "Sintony", size = 3.5,
               hjust = 0.5, halign = 0.5, vjust = 0, valign = 0) +
  geom_textline(aes(x = -19, y = 6000), label = "Atlantic", hjust = 0.5,
                family = "Arvo", colour = "#2a435e", size = 5, vjust = 0.1) +
  geom_textline(aes(x = 71.5, y = 6000), label = "Indian", hjust = 0.5,
                family = "Arvo", colour = "#2a435e", size = 5, vjust = 0.1) +
  geom_textline(aes(x = -150, y = 6000), label = "Pacific", hjust = 0.5,
                family = "Arvo", colour = "#2a435e", size = 5, vjust = 0.1) +
  scale_colour_gradient2(low = "#0c5196", high = "#e1bb7b", midpoint = 3000) +
  ylim(c(-64000, 10000)) +
  coord_polar("x", direction = -1) +
  labs(title = "  
Elevation around the equator  
  ",
       caption = "#30DayChartChallenge | Graphic: @cararthompson | Source: aws via {elevatr}  
  ",
       subtitle = "  
         
If you were to walk, climb and swim in a straight line around  
the equator, you would spend most of your time swimming.
  
  
You would also climb the mountain whose summit is the  
furthest away from the Earth's core: Mount Chimborazo<span style=\"color:#e4792b\">*</span>.") +
  theme_void() +
  theme(plot.caption = element_text(hjust = 0.5, family = "Sintony", colour = "#677787", size = 10),
        plot.title = element_text(hjust = 0.5, family = "Arvo", colour = "#2C3D4F", size = 28),
        plot.subtitle = element_markdown(hjust = 0.5, family = "Sintony", colour = "#677787", size = 14))

# Export
ggsave(filename = "plots/2.distributions-part-1.png", 
       height = 10, width = 8, bg = "#c5e3ed", dpi = 400)

