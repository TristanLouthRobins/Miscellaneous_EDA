library(tidyverse)

# import dataset ---------------------------------------------------------------
data <- read_csv("data/tnc.csv") %>% 
  filter(Series == "VOY",
         Season %in% c(1,2,3,4))

# tidy data --------------------------------------------------------------------
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
         Matt_Watch = factor(Matt_Watch))

# episodes watched: to ensure that unwatched episodes are not included. this is based on whether
# 'Andy_rating' field is populated.
eps_watched <- data[!is.na(data$Andy_rating),] %>% 
  nrow()

# generate summary statistics for the ratings data (Andy, Matt, joint score, IMDb) --
(summary_stats <- 
    data %>% 
    slice(0:eps_watched) %>% 
    pivot_longer(cols = c("Andy_rating", "Matt_rating", "TNC", "IMDB"), names_to = "Rating", values_to = "Value") %>% 
    group_by(Rating) %>% 
    summarise(min = min(Value),
              max = max(Value),
              mean = mean(Value),
              sd = sd(Value),
              median = median(Value)) %>% 
    arrange(desc(mean)))

# creating the MVC leaderboard -------------------------------------------------
MVC_leaderboard <- data %>% 
  slice(0:eps_watched) %>% 
  pivot_longer(cols = c("Andy_MVC", "Matt_MVC"), names_to = "Host") %>% 
  rename("Character" = "value") %>% 
  select(Host, Character) %>% 
  group_by(Character) %>% 
  summarise(count = n()) %>%  
  mutate(ep_appearances = eps_watched) %>% 
  arrange(desc(count)) %>% 
  na.omit()

# create ep count for Seven
MVC_leaderboard$ep_appearances[9] <- (eps_watched - 66)
# create ep count for Kes
MVC_leaderboard$ep_appearances[5] <- 69

MVC_leaderboard %>% 
  mutate(vote_freq = round(ep_appearances / count, 1))

t <- 
data %>% 
  select(Season, Episode_name, ep_num, Andy_MVC, Matt_MVC) %>% 
  slice(0:eps_watched) %>% 
  pivot_longer(cols = c("Andy_MVC", "Matt_MVC"), names_to = "Host") %>% 
  rename("Character" = "value") %>% 
  group_by(Episode_name, Character, ep_num) %>% 
  summarise(count = n()) %>% 
  arrange(ep_num) %>% 
  pivot_wider(names_from = Character, values_from = count, values_fill = 0) %>% 
  pivot_longer(cols = 3:15, names_to = "Character", values_to = "count") %>% 
  group_by(Character) %>% 
  mutate(total = cumsum(count),
         Character = toupper(factor(Character)),
         Episode_number = as.integer(ep_num)) %>% 
  mutate(Character = toupper(Character))

model_data <- 
t %>% 
  select(-Episode_number)

seven_data <- 
  model_data %>% 
  filter(ep_num >= 67, Character == "SEVEN")

model <- lm(total ~ ep_num, data = seven_data)

# Predict the number of episodes needed to reach 31 votes for KIM
predicted_episodes <- predict(model, newdata = data.frame(ep_num = 1:1000)) # Adjust the range as needed

(prediction <- min(which(predicted_episodes >= 31))) 

library(ggplot2)

model_data %>% 
  ggplot() +
  geom_line(aes(x=ep_num, y=total, colour=Character))

######----------------------------######

# Predict the scores at Episode 93
character_totals <- model_data %>%
  group_by(Character) %>%
  filter(ep_num <= 83) %>%
  summarise(total_at_episode_178 = tail(total, 1))

models <- list()
for (char in unique(model_data$Character)) {
  char_data <- filter(model_data, Character == char)
  models[[char]] <- lm(total ~ ep_num, data = char_data)
}

predictions <- list()
for (char in unique(model_data$Character)) {
  char_data <- filter(model_data, Character == char)
  predicted_votes <- predict(models[[char]], newdata = data.frame(ep_num = 178))
  predictions[[char]] <- predicted_votes[1]
}

predictions

######---------------------------#######

model_data %>% 
  filter(Character == "JANEWAY")

seven_data <- 
  model_data %>% 
  filter(ep_num >= 67)

predict_total_votes <- function(data, character_name, ep) {
  # Filter the data for the specified character
  char_data <- filter(data, Character == character_name)
  
  # Fit a linear regression model for the character
  model <- lm(total ~ ep_num, data = char_data)
  
  # Predict the total votes for the character at episode 178
  predicted_votes <- predict(model, newdata = data.frame(ep_num = ep))
  
  # Extract and return the predicted total votes
  return(predicted_votes[1])
}

(prediction <- predict_total_votes(model_data, "JANEWAY", 100))

##

your_data <- model_data

your_data$JaneVotes <- ifelse(your_data$Character == 'JANEWAY' & your_data$count == 0, 1, 0)

(your_data <- your_data %>%
  mutate(JaneCumSum = cumsum(JaneVotes)) %>%
  group_by(JaneCumSum) %>%
  filter(Character == "JANEWAY") %>% 
  mutate(Interval = c(0, diff(row_number()))))


view(your_data)

