library(tidyverse)
library(ggplot2)

percentages <- c(0,0,0,5,13,18,45,14,1,2)
total <- 272

calc_population <- function(total_votes, percentages) {
  array <- c()
  for (i in percentages) {
    pop <- round((total_votes * i)/100, 0)
    array <- c(array, pop)
  }
  return(array)
}

pop <- calc_population(total, percentages)
pop

popDf <- tibble(score = c(1:10), n = pop)

ggplot(popDf) +
  geom_histogram(aes(x = score, weight = n), bins = 10)

scores <- function(df){
  freq <- c()
  for (i in 1:10){
    freq <- c(freq, rep(i, df[i,2]))
  }
  return(freq)
}

scores <- scores(popDf)

mean(scores)
median(scores)

quantile(scores, probs = c(0, 0.25, 0.5, 0.75, 1))

quantile(scores, probs)