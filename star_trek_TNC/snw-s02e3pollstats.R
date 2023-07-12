library(tidyverse)
library(ggplot2)

poll <- tibble(score = c(1:10), n = c(3,14,25,25,35,68,100,64,17,7))

ggplot(poll) +
  geom_histogram(aes(x = score, weight = n), bins = 10)

scores <- c(rep(1,3),
            rep(2,14),
            rep(3,25),
            rep(4,25),
            rep(5,35),
            rep(6,68),
            rep(7,100),
            rep(8,64),
            rep(9,17),
            rep(10,7))

quantile(scores, probs = c(0, 0.25, 0.5, 0.75, 1))

quantile(scores, probs)