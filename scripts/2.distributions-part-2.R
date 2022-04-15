# Distributions - Statistics

## Load libraries ----
library(tidyverse)
library(extrafont)
library(datasauRus)
library(gganimate)
library(ggthemes)
library(ggtext)
library(ggimage)


### Set up the data and the colours ----
eye_data <- tibble(
  dataset = unique(datasaurus_dozen$dataset),
  x = c(50, sample(20:80, length(unique(datasaurus_dozen$dataset)) - 1)),
  y = c(81, sample(25:95, length(unique(datasaurus_dozen$dataset)) - 1))
) %>%
  mutate(point_size = 3)

set.seed(112)
df <- rbind(
  datasaurus_dozen %>%
    mutate(point_size = 1),
  eye_data) %>%
  mutate(point_colour = case_when(dataset == "dino" & point_size == 3 ~ "turquoise",
                                  TRUE ~ sample(c("red", "blue"), 1859, replace = T)),
         dataset = factor(dataset, levels = unique(dataset)))

filter(df, point_size == 3) %>% pull(point_colour) %>% table()
# The eye is blue 6 times and red 6 times

pal <- c("#be0a3e", # red
         "#1f5c99", # blue
         "#3dbbd1", # turquoise
         "#e8e8e8") # beige

monkey_text <- "Designed by vectorstock (Image #21339882 at VectorStock.com)"
monkey <- here::here("assets/monkey.png")
monkey_data <- tibble(dataset = unique(df$dataset),
                      x = seq(-25, 119, 12),
                      y = rep(50, 13))         

## Plot it ----
p <- ggplot(df, aes(x = x, y = y)) +
  geom_image(data = monkey_data, 
             aes(x = x, y = y, image = monkey), size = 0.4) +
  geom_point(aes(colour = point_colour, size = point_size),
             show.legend = F) +
  theme_minimal() +
  scale_colour_manual(labels = c("red", "blue", "turquoise"),
                      values = pal[1:3]) +
  coord_cartesian(xlim = c(0, 100)) +
 transition_states(dataset, transition_length = 2, state_length = 4, wrap = T) +
 ease_aes("quadratic-in-out") +
 enter_grow() +
 exit_shrink() +
  labs(x = "",
       y = "", 
       title = "<span style='color:#e2110a;font-size:90px'>_</span><br>The Invisible Dinosaur",
       subtitle = "Follow the dinosaur's <span style='color:#3dbbd1'>eye</span>. How many times 
       is it <span style='color:#be0a3e'>red</span> and <span style='color:#1f5c99'>blue</span> within<br>each cycle?<br>",
       caption = "
Excluding the eye, the points in all the plots result in the same summary 
statistics - a good lesson in the importance of looking at roooaaar data! 

\n#30DayChartChallenge | Graphic: @cararthompson | Source: datasauRus
Monkey: adapted from vectorstock (Image #21339882 at VectorStock.com)") +
  theme_minimal() %+replace%
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(colour = pal[4], size = 1.2),
        text = element_text(family = "Lato", colour = "#050709"),
        plot.title = element_markdown(hjust = 0, size = 16, face = "bold", 
                                      lineheight = 2, padding = unit(c(0, 0, 0.5, 0), "line")),
        plot.subtitle = element_markdown(hjust = 0, size = 16, lineheight = 1.3),
        plot.caption = element_text(hjust = 1, size = 10, colour = "#aeb2b7"))

## Export plot ----
anim_save(animate(p, width = 8, height = 9, units = "in", res = 300, start_pause = 30), 
          filename = here::here("plots/2.distributions-part-2.gif"), rewind = F)
