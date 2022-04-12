# Comparisons
## Pictogram
## Flora
## OWID

library(tidyverse)
library(extrafont)
library(ggfx)
library(ggimage)

forest_data <- read.csv("data/forest-area-as-share-of-land-area.csv",
                        na.strings = "") %>%
  janitor::clean_names() %>%
  filter(year == max(year),
         !is.na(code)) %>%
  filter(forest_cover %in% quantile(forest_cover, 
                                    probs = seq(0, 1, 0.1), type = 1),
         forest_cover != 0) %>%
  arrange(forest_cover) %>%
  mutate(entity = factor(entity, levels = unique(entity)))

icon_layout <- data.frame(x_trees = sort(rep(seq(5, 95, 10), 10)),
                          y_trees = forest_data$entity)

# Plot components
# Must be a transparent png for this to work!
icon <- "assets/tree.png"

bg <- "#eef8fe"
ground <- "#887673"
trees <- "#2d553f"
dark_text <- "#122610"
light_text <- monochromeR::generate_palette("#122610",
                                            n_colours = 3, 
                                            modification = "go_lighter")[2]

ggplot() +
  as_reference(
    geom_image(data = icon_layout, aes(x = x_trees, y = y_trees, image = icon), 
               size = 0.08),
    id = "text"
  ) +
  geom_segment(aes(y = c(0.55:9.55), yend = c(0.55:9.55), x = rep(0, 10), xend = rep(100, 10)),
               lineend = "round", size = 2, colour = ground, alpha = 0.8) +
  with_blend(
    geom_col(data = forest_data, aes(x = forest_cover, y = entity),
             fill = trees,
             position = position_stack(reverse = T), show.legend = F),
    bg_layer = "text",
    blend_type = "in"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        text = element_text(colour = light_text, family = "Segoe UI"),
        plot.title = element_text(colour = dark_text, family = "Arvo", size = 24),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(hjust = 0.35),
        axis.text = element_text(colour = light_text, family = "Segoe UI", size = 10)) +
  coord_fixed(ratio = 10) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100),
                     labels = function(x) paste0(x, "%"),
                     limits = c(0, 120)) +
  labs(title = paste0("Proportion of land covered in forest"),
       subtitle = "  
The geographical entities included in this plot fell on the 10th, 20th, 30th to 100th.  
percentiles of forest cover in the most recent year in the dataset (2020). Each tree  
represents 10% of land covered in forest.  
  
The large jump between Congo and Suriname indicates that relatively few entities 
have more than 65% forest cover. Of the 224 entities with 2020 data, only Palau, 
the Solomon Islands, Gabon, Guyana and Suriname had forest over more than  
90% of the land.  
  ",
       caption = "  
  
#30DayChartChallenge | Graphic: @cararthompson | Source: Our World In Data",
       x = "",
       y = "")

# Export
ggsave(filename = "plots/1.comparisons-part-2.png", 
       height = 10, width = 9, bg = "#f5f5f5", dpi = 400)

