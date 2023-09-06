library(tidyverse)
library(ggplot2)
data <- read_csv("data/tnc.csv")

data %>% 
  filter(Season %in% c(3:6)) %>% 
  ggplot() +
  geom_point(aes(x=ep_num, y=`Joint TNC Rating`, colour=Season)) +
  geom_smooth(aes(x=ep_num, y=`Joint TNC Rating`), se=F)
