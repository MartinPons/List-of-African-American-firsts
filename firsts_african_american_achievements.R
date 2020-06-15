# 01-EDA.R


# ITINTIAL SETTING --------------------------------------------------------

# libraries
library(tidyverse)
library(skimr)
library(extrafont)
library(viridis)
library(tidytext)
library(lubridate)
library(ggrepel)


# LOAD DATA ----------------------------------------------------------

firsts <- read_csv(here::here("2020", "2020-06-09", "firsts.csv")) %>% 
  mutate(across(.cols = 4:5, .fns = as.factor))


# WRANGLING ---------------------------------------------------------------

# create decade column
firsts <- firsts %>% 
  mutate(decade = year - year %% 10)



# VISUALIZATION -----------------------------------------------------------

# Note: you'll have to uncomment the two lines in the next paragraph if you use extrafont library 
# for the first time in order for the code to detect the Georgia Font. please note that fonts are 
# loaded from Windows operating system

# font_import()
# loadfonts(device = "win")

firsts %>% 
  
  # remove blank category
  filter(!is.na(category)) %>% 
  
  # decada as factor
  mutate(decade = as.factor(decade)) %>% 
  
  # accomplishments count
  group_by(decade, category) %>% 
  summarise(n = sum(!is.na(accomplishment))) %>% 
  
   ## visualization ##
  ggplot(aes(x = n, y = category)) + 

  # columns
  geom_col(aes(fill = category)) + 
  
  # vertical white lines to make the "squares"
  geom_vline(xintercept = seq(0, 25, by = 1), color = "white") + 
  
  # black band
  geom_rect(xmin = 16, xmax = 24, ymin = 3, ymax = 9, color = "#783838", fill = "black", size = 1.2) + 
  
  # decade tag
  geom_text(aes(label = decade), y = 7, x = 20, size = 6.5, 
            alpha = 1, color = "white", family = "Georgia") +
  facet_wrap(~decade, scales = "fixed", drop = F) + 
  theme_bw() + 
  
  # plot theme
  theme(
    text = element_text(family = "Georgia", color = "grey45"),
    legend.position = "top", 
    panel.border = element_rect(color = "#783838", size = 1, linetype = 7),
    axis.line = element_line(),
    axis.title.x = element_text(size = 13),
    axis.title.y = element_blank(),
    strip.text = element_blank(), 
    strip.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, color = "#783838", size = 18),
    panel.background = element_rect(fill = "white", size = 1.2),
    legend.title = element_blank()) + 
    scale_fill_brewer(type = "qual", palette = "RdBu") + 
  scale_x_continuous(limits = c(0, 25)) + 
  labs(title = "First achievements by African Americans decade-by-decade", 
       x = "Number of achievements",
       caption = "Data comes from Wikipedia, cleaned by Thomas Mock | Visualization by @MartinponsM")
   

  

