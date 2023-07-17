library(tidyverse)
library(ggplot2)

percentages <- c(1,0,3,2,6,14,24,38,12,1)
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

poll <- tibble(score = c(1:10), n = pop)

ggplot(poll) +
  geom_histogram(aes(x = score, weight = n), bins = 10)

scores_e4 <- c(rep(1,3),
            rep(2,0),
            rep(3,8),
            rep(4,5),
            rep(5,16),
            rep(6,38),
            rep(7,65),
            rep(8,103),
            rep(9,33),
            rep(10,3))

mean(scores_e4)
median(scores_e4)

quantile(scores, probs = c(0, 0.25, 0.5, 0.75, 1))

quantile(scores, probs)