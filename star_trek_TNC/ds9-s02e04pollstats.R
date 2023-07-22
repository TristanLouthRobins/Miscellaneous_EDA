# Data EDA for Star Trek: The Next Conversation podcast ------------------------
# Hosted by Matt Mira and Andy Secunda -----------------------------------------
# This EDA script is for Patreon votes -----------------------------------------
# Podcast episode time line ----------------------------------------------------
# Latest update: v1.0 (19 July 2023) -------------------------------------------
library(tidyverse)
library(ggplot2)

# Populate with latest stats ---

# Series, Ep title, Ep num, Season, scores[0-10], total votes

percentages <- c("SNW", "Among The Lotus Eaters",4,2,1,0,3,1,5,14,24,38,12,1,274)

patreon.df <- read_csv("patreon_stats.csv")
(row <- nrow(patreon.df) + 1)

populate <- function(vec){
  for(i in 1:2) {
      patreon.df[row,i] <- vec[i]
  }
  for(i in 3:15){
    patreon.df[row,i] <- as.double(vec[i])
  }
  return(patreon.df)
}

patreon.df.updated <- populate(percentages)

write_csv(patreon.df.updated, "patreon_stats.csv")

# Read in updated version of df ---

patreon.df <- read_csv("patreon_stats.csv")
view(patreon.df)
(row <- nrow(patreon.df))

# Select row to calculate pop ---
calculate_population <- function(df){
  for(i in 1:row){
    for(j in 5:14){
      df[i,j] <- round((df[row,15] * df[row,j])/100, 0) 
    }
  }
  return(df)
}
  

#    d[rows, i] <- round((d[rows,15] * d[rows, i])/100, 0)

calculate_population(patreon.df)

testdf <- 
  calculate_population(patreon.df) %>% 
  mutate(tot = rowSums(across(5:14)))

view(testdf)
pop <- testdf %>% select(5:14)
popDf <- tibble(score = c(1:10), n = pop)

freq <- c()
averages <- tibble(avg = numeric())

calc_averages <- function(df) {
  for(i in 1:row){
    for(j in 5:14) {
      line <- unlist(df[i,])
      print(line[,j])
#      freq <- c(freq, rep(df[i,j], df[i,j]))
#      avg <- mean(freq)
    }
  }
}

calc_averages(testdf)



########################

quantile(scores, probs = c(0, 0.25, 0.5, 0.75, 1))
quantile(scores, probs)