data <- read_csv("star_trek_TNC/data/tnc.csv") %>% 
  filter(Series == "DS9") %>% 
  pivot_longer(cols = c("Rating - Andy", "Rating - Matt", "IMDB (at time of listen)", "Joint TNC Rating"), values_to = "val", names_to = "rating")

data %>% 
  ggplot() +
  geom_line(aes(x=as.numeric(row.names(data)), y=val, colour = as.factor(Season))) +
  geom_smooth(aes(x=as.numeric(row.names(data)), y=val), se = FALSE) +
  xlim(0,220) +
  ylim(0,10) +
  facet_wrap(~rating, nrow = 2)
