library(tidyverse)
library(ggplot2)

percentages <- c(2,
                 0,
                 0,
                 2,
                 6,
                 6,
                 21,
                 31,
                 23,
                 6,
                 4)
total <- 52

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

popDf <- tibble(score = c(0:10), n = pop)

#ggplot(popDf) +
#  geom_histogram(aes(x = score, weight = n), bins = 10)

scores <- function(df){
  freq <- c()
  for (i in 1:11){
    freq <- c(freq, rep(i, df[i,2]))
  }
  freq <- freq - 1
  return(freq)
}

scores <- scores(popDf)

mean(scores)
median(scores)
