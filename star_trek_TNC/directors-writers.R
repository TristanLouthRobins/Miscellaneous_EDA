# Data EDA for Star Trek: The Next Conversation podcast ------------------------
# Hosted by Matt Mira and Andy Secunda -----------------------------------------
# This EDA script is for their episodes reviewing Star Trek: TNG ---------------
# Directors and writers --------------------------------------------------------
# Latest update: v1.0 (14 July 2023) ------------------------------------------
library(tidyverse)
library(showtext)
library(patchwork)

data <- read_csv("data/tnc.csv") 
  
data %>% 
  filter(Series == "TNG") %>% 
  select(`Director 1`) %>% 
  group_by(`Director 1`) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  arrange(desc(n))

data %>% 
  filter(Series == "TNG") %>% 
  select(`Writer 1`) %>% 
  group_by(`Writer 1`) %>% 
  summarise(n = n(), .groups = "drop") %>% 
  arrange(desc(n))

