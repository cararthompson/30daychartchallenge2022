# Relationships
## Correlation (education / $$)
## Multivariate (gender)
## OECD (data source)

## Load libraries ----
library(tidyverse)
library(extrafont)
library(ggtext)


### Set up the data and the colours ----
education_spending <- read.csv("https://stats.oecd.org/sdmx-json/data/DP_LIVE/.EDUEXPTRY.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en")

public_spending <- read.csv("https://stats.oecd.org/sdmx-json/data/DP_LIVE/.EDUPUBEXP.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en") %>%
  janitor::clean_names() %>%
  filter(time ==  max(education_spending$TIME), 
         subject == "TRY")

ed_prop_df <- education_spending %>%
  janitor::clean_names() %>%
  filter(time == max(education_spending$TIME),
         subject %in% c("PRIV", "PUB")) %>%
  rename(prop_spending = value) %>%
  left_join(public_spending %>%
              select(x_location, value), by = "x_location") %>%
  rename(gdp_prop = value) 

tertiary_ed_by_gender <- read.csv("https://stats.oecd.org/sdmx-json/data/DP_LIVE/.EDUADULT.../OECD?contentType=csv&detail=code&separator=comma&csv-lang=en")

gender_df <- tertiary_ed_by_gender %>%
  janitor::clean_names() %>%
  

ggplot(ed_prop_df) +
  geom_point(aes(y = gdp_prop, x = prop_spending, colour = subject)) +
  geom_hline(aes(yintercept = gdp_prop))

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
