# Comparisons - Part 1:
## Part-to-whole
## Historical
## Slope 

library(tidyverse)
library(scattermore)
library(extrafont)
library(cowplot)
library(patchwork)
library(ggtext)

# Colours 
blend_col <- "#2C3D4F"
dark_text <- monochromeR::generate_palette("#2C3D4F", 
                                           modification = "go_darker", 
                                           n_colours = 2)[2]
light_text <- monochromeR::generate_palette(dark_text, 
                                            modification = "go_lighter", 
                                            n_colours = 4)[2]

blend <- function(colour) {
  monochromeR::generate_palette(colour, 
                                blend_colour = "#2C3D4F", 
                                n_colours = 10)[2]
}

# Theme
theme_w1p1_min <- function() {
  theme_minimal() %+replace%
    theme(plot.title = element_text(hjust = 0.5, size = 24, colour = dark_text, 
                                        family = "Just Another Hand", 
                                        lineheight = 1.8),
          plot.subtitle = element_markdown(family = "How Lovely", size = 12, hjust = 0.5, lineheight = 1.5),
          legend.text = element_text(family = "How Lovely", colour = dark_text),
          axis.text = element_text(family = "Just Another Hand", colour = light_text, size = 8),
          axis.title.y = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
          axis.title = element_text(family = "Just Another Hand", colour = light_text, size = 10),
          legend.title = element_blank())
}

theme_w1p1_void <- function() {
  theme_void() %+replace%
    theme(plot.title = element_text(hjust = 0.5, size = 24, colour = dark_text, 
                                        family = "Just Another Hand",
                                        lineheight = 1.8),
          plot.subtitle = element_markdown(family = "How Lovely", size = 12, hjust = 0.5, lineheight = 1.5),
          legend.text = element_text(family = "How Lovely", colour = dark_text),
          legend.title = element_blank(),
          legend.position = "bottom")
}

# Text only
title <- ggdraw() +
  draw_label("\nRelationships",
             fontfamily = "Christmas Love",
             colour = dark_text,
             hjust = 0.5,
             size = 46) +
  theme_w1p1_void()

subtitle <- ggdraw() +
  draw_label("Tips from musicians through the decades",
             fontfamily = "How Lovely",
             colour = dark_text,
             hjust = 0.5,
             size = 16) +
  theme_w1p1_void()

title/subtitle

caption <- ggdraw() +
  draw_label("#30DayChartChallenge | @cararthompson",
             fontfamily = "Just Another Hand",
             colour = dark_text,
             hjust = 0.5,
             size = 10) +
  theme_w1p1_void()


# Individual plots
bruce_springsteen_1980 <- tibble(heart_cond = c('Hungry', 'Not hungry'),
                                 amount = c(100, 0)) %>%
  ggplot(aes(x = "", y = amount, fill = heart_cond)) +
  geom_bar(stat = "identity", position = "stack", width = 1, 
           size = 1) +
  coord_polar("x", start = 1.57) +
  ylim(c(-120, 100)) +
  scale_fill_manual(values = c(blend("#53b7ce"), blend("#24211c"))) +
  labs(title = "#1 - Acknowledge the situation",
       subtitle = "Universal heart condition survey") +
  theme_w1p1_void() 


meatloaf_1993 <- tibble(things = c('Anything else', 'That'), 
                        amount = c(100, 0)) %>%
  ggplot(aes(x = "", y = amount, fill = things)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  theme_void() +
  scale_fill_manual(values = c(blend("#2a537f"), blend("#571602"))) +
  labs(title = "#2 - Establish healthy boundaries",
       subtitle = "Things he said he would do for love.") +
  theme_w1p1_void()

set.seed(123)
sc7_data <- tibble(x = rep(c(1:2000), 500), y = sort(rep(c(1:500), 2000)), 
                   us_others = sample(c(rep("Us", 2), 
                                        rep("Everyone else", 999998))))

s_club_7_1999 <- ggplot() +
  geom_scattermore(data = filter(sc7_data, us_others == "Everyone else"),
                   aes(x = x, y = y, color = us_others), shape = "*",
                   show.legend = F) +
  geom_scattermore(data = filter(sc7_data, us_others == "Us"),
                   aes(x = x, y = y, color = us_others),  shape = "*",
                   show.legend = F) +
  scale_colour_manual(values = c(blend("#582166"), blend("#f9ebe2"))) +
  labs(title = "\n#3 - Cherish the unique nature of each relationship",
       subtitle = "Zoom in, and spare a thought for the <span style='font-family:\"Just Another Hand\"'>6.033</span> billion who didn't get a mention.",
       x = "2000",
       y = "500") +
  coord_equal() +
  theme_w1p1_min() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank())

taylor_swift_2004 <- tibble(You = c(1:200),
                            Me = c(1:200),
                            up_down = c(rep("Up", 100), rep("Down", 100))) %>%
  ggplot(aes(x = Me, y = You, color = up_down)) +
  geom_jitter(width = 5, height = 5, 
              size = sample(c(1:5), 200, replace = T), 
              alpha = 0.3,
              show.legend = F) +
  labs(title = "#4 - Friendships are important",
       subtitle = "Mood correlation study") +
  scale_x_continuous(breaks = c(0, 200), labels = c("Down", "Up")) +
  scale_y_continuous(breaks = c(0, 200), labels = c("Not up", "Not down")) +
  theme_w1p1_min() +
  scale_colour_manual(values = c(blend("#09a8d2"), blend("#164c2b"))) +
  theme(panel.grid = element_blank())

celine_dion_1997 <- 
  rvest::read_html("http://www.azlyricdb.com/lyrics/Celine-Dion-My-Heart-Will-Go-On-57923") %>%
  rvest::html_elements("li") %>%
  rvest::html_text() %>%
  enframe() %>%
  mutate(clean_text = tolower(stringr::str_squish(gsub(",", "", 
                                                       value, fixed = T))),
         line = ceiling(name/2)) %>%
  mutate(formatted_text = gsub("my heart", paste0("<span style='color:", blend("#476c87"),
                                                  "'>my heart</span>"), clean_text)) %>%
  mutate(formatted_text = gsub("go on", paste0("<span style='color:",
                                               blend("#be6a31"), "'>go on</span>"), formatted_text)) %>%
  separate(formatted_text, c("plain", "format_open", "formatted", "format_close", 
                             "plain2", "format_open2", "formatted2", "format_close2",
                             "plain3"), 
           sep = c("<|>"),
           fill = "right") %>%
  mutate(format_open = paste0("<", format_open, ">"),
         format_open2 = paste0("<", format_open2, ">"),
         format_close = paste0("<", format_close, ">"),
         format_close2 = paste0("<", format_close2, ">"),
         plain = gsub("[a-z]", "■", plain),
         plain2 = gsub("[a-z]", "■", plain2),
         plain3 = gsub("[a-z]", "■", plain3),
         formatted = gsub("[a-z]", "■", formatted),
         formatted2 = gsub("[a-z]", "■", formatted2)) %>%
  unite(c("plain","format_open", "formatted", "format_close",
          "plain2","format_open2", "formatted2", "format_close2",
          "plain3"), sep = " ", col = "plot_text") %>%
  # Doing weird things to NAs, needed extra steps to remove them
  mutate(plot_text = gsub("NA", "", plot_text)) %>%
  mutate(plot_text = gsub("_<>_|_|<>", "", plot_text)) %>%
  mutate(plot_text = gsub(" ", "  ", plot_text)) %>%
  ggplot() +
  geom_textbox(aes(x = 0, y = -name, label = plot_text),
               vjust = 0.5, valign = 0.5,
               hjust = 0.5, halign = 0.5,
               maxwidth = unit(25, "lines"),
               width = unit(25, "lines"),
               size = 4,
               colour = blend("#adc2dc"),
               fill = NA,
               box.color = NA) +
  labs(title = "#5 - So is commitment",
       subtitle = paste0("A lyricist's commitment to <span style='color:",
                         blend("#476c87"), "'>my heart</span> and <span style='color:",
         blend("#be6a31"), "'>go on</span>.")) +
  theme_w1p1_void()


# Whole image
p <- title / 
  subtitle /
  (bruce_springsteen_1980 | meatloaf_1993) /
  s_club_7_1999 /
  (taylor_swift_2004 | celine_dion_1997) /
  caption +
  plot_layout(heights = unit(c(2, 1.5, 7, 3, 8, 0.5), c('cm', 'null')))


# Save
ggsave(plot = p, filename = "plots/comparisons-part-1.png", 
       dpi = 400, height = 14, width = 8, bg = "#ffffff")
