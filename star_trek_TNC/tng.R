# Data EDA for Star Trek: The Next Conversation podcast ---
# Hosted by Matt Mira and Andy Secunda --------------------

# This EDA script is for their episodes reviewing TNG -----

# Latest update: v1.0.0 (9th March 2023) -------------------

# Variable key:
# Andy_rating/Matt_rating: rating out of 10 'Andys' for each episode.
# Andy_MVC/Matt_MVC: Most Valuable Character vote for each episode.
# Andy_Watch/Matt_Watch: Whether the host recommends viewings watch a given episode.
# TNC: The averaged rating - i.e: (Andy + Matt) / 2
# IMDB: IMDb rating as at 15th Feb 2023

# load Tidyverse for data tidying and import current dataset ------------
library(tidyverse)
library(ggplot2)
library(cropcircles) # Credit to: https://github.com/doehm for his excellent pkg!
library(ggimage)
library(ggrepel)
library(ggbump)
library(glue)
library(showtext)

data <- read_csv("star_trek_TNC/data/tnc_stats.csv") %>% 
  filter(Series == "TNG") 

view(data)

# tidy data -------------------------------------------------------------
# first season data tidy ------------------------------------------------

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


# summarise the hosts rating data -------------------

host_rating_data <- 
  data %>% 
  pivot_longer(cols = c("Andy_rating", "Matt_rating"), names_to = "Host", values_to = "Rating") %>% 
  select(Episode_number, Season, Episode_name, Host, Rating)

# summarise the joint-host and IMDB data ------------

joint_rating_data <- 
  data %>% 
  pivot_longer(cols = c("TNC", "IMDB"), names_to = "Joint", values_to = "Rating") %>% 
  select(Episode_number, Season, Episode_name, Joint, Rating)

# create MVC leaderboard ------------------------------------
eps_watched <- data[!is.na(data$Andy_rating),] %>% 
  nrow()

pivot_MVC <- 
  data %>% 
  slice(0:eps_watched) %>% 
  pivot_longer(cols = c("Andy_MVC", "Matt_MVC"), names_to = "Host") %>% 
  rename("Character" = "value") %>% 
  select(Host, Character)

(MVC_leaderboard <- 
    pivot_MVC %>% 
    group_by(Character) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count)))

# convert Andy_Watch and Matt_Watch categories into joint int value and new category ----
data <- 
  data %>% 
  mutate(A.Watch_score = ifelse(Andy_Watch == "YES", 2, 
                                ifelse(Andy_Watch == "NEUTRAL", 1, 
                                       0))) %>% 
  mutate(M.Watch_score = ifelse(Matt_Watch == "YES", 2, 
                                ifelse(Matt_Watch == "NEUTRAL", 1, 
                                       0))) %>% 
  mutate(J.Watch_score = A.Watch_score + M.Watch_score) %>% 
  mutate(Watchability = factor(ifelse(J.Watch_score == 4, "WATCH IT!",
                                      ifelse(J.Watch_score == 3, "NOT ESSENTIAL",
                                             ifelse(J.Watch_score == 2, "MEH",
                                                    ifelse(J.Watch_score == 1, "SKIP",
                                                           "AVOID")))))) %>% 
  select(-A.Watch_score, -M.Watch_score)


# generate summary statistics ---------------------------------
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

# top 3, bottom 3 eps within given season
top3 <- function(season, who){
  data %>% 
    slice(0:eps_watched) %>% 
    filter(Season == season) %>% 
    arrange(desc({{who}})) %>% 
    head(n=3)
}

bot3 <- function(season, who){
  data %>% 
    slice(0:eps_watched) %>% 
    filter(Season == season) %>% 
    arrange({{who}}) %>% 
    head(n=3)
}

(top_TNC <- top3(1,TNC))
(top_IMDB <- top3(1,IMDB))

(bot_TNC <- bot3(1,TNC))
(bot_IMDB <- bot3(1,IMDB))

# visualisations ------------------------------------------------  

library(ggplot2)
library(cropcircles) # Credit to: https://github.com/doehm for his excellent pkg!
library(ggimage)
library(lcars)
library(glue)

font_add("StarTrekTNGTitle", "/Users/tristanlouth-robins/Library/Fonts/Star Trek TNG-Title Regular.ttf")
font_add("Antonio", "/Users/tristanlouth-robins/Library/Fonts/Antonio-VariableFont_wght.ttf")

# define TNG viz themes --------------------------------------------
theme_trek <- function(){
  theme(
    text = element_text(family = "Antonio", colour = "#FFFFFF"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#000000", colour = "#99CCFF"),
    plot.title = element_text(family = "Star Trek TNG-Title", size = 20, colour = "#F7B05A", vjust = 0),
    plot.subtitle = element_text(family = "Antonio", face = "bold", size = 14, colour = "#99CCFF"),
    axis.title.x = element_text(family = "Antonio", face = "bold", size = 12),
    axis.title.y = element_text(family = "Antonio", face = "bold", size = 12),
    axis.text.x = element_text(family = "Antonio", face = "bold", size = 10, colour = "#646DCC"),
    axis.text.y = element_text(family = "Antonio", face = "bold", size = 10, colour = "#646DCC"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "#000000"),
    legend.title = element_text(family = "Antonio", size = 14, colour = "#E7FFFF"),
    legend.text = element_text(family = "Antonio", size = 12, colour = "#E7FFFF"),
    legend.background = element_rect(fill = "#000000"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank() 
  )
}

# this theme is for the 'header' once all the other plots are patched together --

theme_trek_header <- function(){
  theme(
    text = element_text(family = "Antonio", colour = "#FFFFFF"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#000000", colour = "000000"),
    plot.title = element_text(family = "Star Trek TNG-Title", size = 24, colour = "#F7B05A", vjust = 0),
    plot.subtitle = element_text(family = "Antonio", face = "bold", size = 18, colour = "#99CCFF"),
    axis.title.x = element_text(family = "Antonio", face = "bold", size = 12),
    axis.title.y = element_text(family = "Antonio", face = "bold", size = 12),
    axis.text.x = element_text(family = "Antonio", face = "bold", size = 14, colour = "#646DCC"),
    axis.text.y = element_text(family = "Antonio", face = "bold", size = 14, colour = "#646DCC"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "#000000"),
    legend.title = element_text(family = "Antonio", size = 14, colour = "#E7FFFF"),
    legend.text = element_text(family = "Antonio", size = 12, colour = "#E7FFFF"),
    legend.background = element_rect(fill = "#000000"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank() 
  )
}

# viz for individual host rating data -----------------------------------

host_rating_plt <- 
  host_rating_data %>% 
  filter(Season == 1) %>% 
  mutate(Episode_name = toupper(Episode_name)) %>% 
  ggplot(aes(x=Rating, y=fct_reorder(Episode_name, Episode_number), fill = Host)) +
  geom_bar(stat = "identity", position = "dodge") +
  #  geom_vline(xintercept = summary_stats$mean[2], colour = "#FFFF33", linetype = "dashed", size = 1, alpha = 0.7) +
  #  geom_vline(xintercept = summary_stats$mean[4], colour = "#ED884C", linetype = "dashed", size = 1, alpha = 0.7) +
  labs(
    subtitle = "SEASON ONE: EPISODE RATINGS - INDIVIDUAL",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0,2,4,6,8,10))  +
  scale_fill_manual(values = c("#FFFF33", "#ED884C"),
                    labels = c("ANDY", "MATT")) +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust =1),
        legend.title = element_blank(),
        legend.position = "right") +
  coord_flip()

# viz for joint rating vs IMDb rating -----------------------------------

joint_vs_imdb_rating_plt <- 
  joint_rating_data %>% 
  filter(Season == 1) %>% 
  mutate(Episode_name = toupper(Episode_name)) %>% 
  ggplot(aes(x=Rating, y=fct_reorder(Episode_name, Episode_number), fill = Joint)) +
  geom_bar(stat = "identity", position = "dodge") +
  #  geom_vline(xintercept = summary_stats$mean[1], colour = "#97567B", linetype = "dashed", size = 1, alpha = 0.7) +
  #  geom_vline(xintercept = summary_stats$mean[3], colour = "#72E2E4", linetype = "dashed", size = 1, alpha = 0.7) +
  labs(
    subtitle = "SEASON ONE: EPISODE RATINGS - TNC VS IMDb",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0,2,4,6,8,10)) +
  scale_fill_manual(values = c("#97567B", "#72E2E4"),
                    labels = c("IMDb", "TNC")) +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust =1),
        legend.title = element_blank(),
        legend.position = "right") +
  coord_flip()

# -------------------------------------------------
# -------------------------------------------------

# Cluster analysis of episode rankings

k_data <- data %>% 
  filter(Season == 2,
         TNC != 0) %>%  
  select("Episode_name", "Matt_rating", "Andy_rating", "TNC", "IMDB")

episodes <- k_data[,1]
tnc_score <- k_data[,4] # Here, we're grabbing the TNC score as an un-scaled variable
imdb_score <- k_data[,5] # As above, but with IMDB
joint_avg <- (tnc_score + imdb_score) / 2 
k_data <- k_data[2:5]
scaled_k_data <- scale(k_data)
model <- kmeans(scaled_k_data, centers = 4)
cluster <- model$cluster

cluster <- factor(cluster, levels = c(3,4,2,1))

scaled_k_data <- cbind(scaled_k_data, cluster)
complete_k_data <- cbind(episodes, scaled_k_data, joint_avg) %>% 
  rename(joint = 7)

cluster_txtcol <- "#3786FF" 
stellar_pal <- c("#CD6363", "#99FF66", "#FF9C00", "#9C9CFF")

theme_trek_clust <- function(){
  theme(
    text = element_text(family = "Antonio", colour = "#FFFFFF"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#000000", colour = "#99CCFF"),
    plot.title = element_text(family = "StarTrekTNGTitle", size = 28, colour = "#3399FF", vjust = 0),
    plot.subtitle = element_text(family = "Antonio", face = "bold", size = 14, colour = "#99CCFF"),
    axis.title.x = element_text(family = "Antonio", face = "bold", size = 12),
    axis.title.y = element_text(family = "Antonio", face = "bold", size = 12),
    axis.text.x = element_text(family = "Antonio", face = "bold", size = 10, colour = "#646DCC"),
    axis.text.y = element_text(family = "Antonio", face = "bold", size = 10, colour = "#646DCC"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.background = element_rect(fill = "#000000"),
    legend.title = element_text(family = "Antonio", size = 14, colour = "#E7FFFF"),
    legend.text = element_text(family = "Antonio", size = 12, colour = "#E7FFFF"),
    legend.background = element_rect(fill = "#000000"),
    panel.grid.major = element_line(colour = "#4C4D47"), 
    panel.grid.minor = element_blank()
  )
}

complete_k_data %>% 
  ggplot() +
  stat_density_2d(aes(x=TNC, y=IMDB), colour = "#46616E") +
  annotate("text", x=1, y=1, size = 8, colour = cluster_txtcol, label = "DABO!", fontface = 2, family = "Antonio") +
  annotate("text", x=0, y=0, size = 8, colour = cluster_txtcol, label = "NEUTRAL ZONE", fontface = 2, family = "Antonio") +
  annotate("text", x=-1, y=-1, size = 8, colour = cluster_txtcol, label = "BADLANDS", fontface = 2, family = "Antonio") +
  annotate("text", x=-2, y=-2, size = 8, colour = cluster_txtcol, label = "HELL", fontface = 2, family = "Antonio") +
  geom_point(aes(x=TNC, y=IMDB, colour = factor(cluster), size = joint), alpha = 0.7) +
  geom_label_repel(aes(x=TNC, y=IMDB, label=toupper(Episode_name)),
                   family = "Antonio",
                   colour = "#FFFF33",
                   fill = "black",
                   max.overlaps = 7,
                   nudge_x = 0,
                   nudge_y = 0.25,
                   segment.linetype = 1,
                   segment.curvature = -0.1,
                   segment.ncp = 3,
                   segment.angle = 20,
                   alpha = 0.7) +
  scale_color_manual(values = stellar_pal) +
  labs(title = "THE NEXT GENERATION",
       subtitle = "EPISODE GUIDE: SEASON 2 QUADRANT (K-MEANS CLUSTER MODEL)",
       caption = "BROUGHT TO YOU BY TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins",
       x = "", y = "") +
  theme_trek_clust() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.key = element_rect(fill = "#000000", color = NA)) 


ggsave("exports/tng_s2_cluster.png", width = 36, height = 24, units = "cm") 
