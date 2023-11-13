# https://www.digitalocean.com/community/tutorials/google-sheets-in-r

library(tidyverse)
library(googlesheets4)

data <- read_sheet('https://docs.google.com/spreadsheets/d/13u3rkmdJbufFHSMYz2W21cjqGLhDt4TaLwM82hGVN4k/edit#gid=0')

data$Ep <- unlist(data$Ep)

data <- 
  data %>% 
  rename("Episode_name" = "Ep",
         "Episode_number" = "n", 
         "Andy_rating" = "Rating - Andy",
         "Matt_rating" = "Rating - Matt",
         "Andy_MVC" = "MVC - Andy",
         "Matt_MVC" = "MVC - Matt",
         "Andy_Watch" = "Watch - Andy",
         "Matt_Watch" = "Watch - Matt",
         "TNC" = "Joint TNC Rating",
         "IMDB" = "IMDB (at time of listen)") %>% 
  mutate(Episode_name = factor(Episode_name), 
         Season = factor(Season),
         Andy_MVC = factor(Andy_MVC),
         Matt_MVC = factor(Matt_MVC),
         Andy_Watch = factor(Andy_Watch),
         Matt_Watch = factor(Matt_Watch),
         TNC = as.double(TNC))

data
