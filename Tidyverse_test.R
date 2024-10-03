# Short script to test RStudio and tidyverse installation
# by Luc Bussière
# last modified Oct 31, 2022


# clear workspace
rm(list = ls())

# Before you run the next line, you must have installed the tidyverse. Do this according to the instructions in the tutorial pdf.

library(tidyverse)

# If you have correctly installed the tidyverse, you will see information on the packages being attached, and a list of functions being replaced (conflicts) in the console after running the previous line of code. This is all good! If you get an error, read it carefully -- the most common problem is that the tidyverse has not been installed. Revisit the PDF for instructions on installing the tidyverse. 

# load data from starwars dataset
data(starwars)


# Make a plot illustrating some nerd things (modified from original here: https://b-rodrigues.github.io/modern_R/graphs.html)

starwars %>%
  rowwise() %>%
  mutate(n_films = length(films)) %>%
  mutate(more_1 = case_when(n_films == 1 ~ "Exactly one movie",
                            n_films != 1 ~ "More than 1 movie")) %>%
  mutate(human = case_when(species == "Human" ~ "Human",
                           species != "Human" ~ "Non-human")) %>%
  filter(gender %in% c("feminine", "masculine"), !is.na(human)) %>%
  ggplot(aes(height, fill = gender)) +
  facet_grid(human ~ more_1) +
  ggtitle("Data-viz for nerds") +
  xlab("height (cm)") +
  geom_density(alpha = 0.4) +
  theme(plot.title = element_text(hjust = 0.5))

# This last code should produce a figure like the one in the handout. If not, see Luc during the appointed time for resolving tech issues before the session on using R.
