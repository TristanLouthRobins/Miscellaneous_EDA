# Data EDA for Star Trek: The Next Conversation podcast ---
# Hosted by Matt Mira and Andy Secunda --------------------

# This EDA script is for their episodes reviewing DS9 -----

# Latest update: v1.2.2 (15th March 2023) -------------------

# Variable key:
# Andy_rating/Matt_rating: rating out of 10 'Andys' for each episode.
# Andy_MVC/Matt_MVC: Most Valuable Character vote for each episode.
# Andy_Watch/Matt_Watch: Whether the host recommends viewings watch a given episode.
# TNC: The averaged rating - i.e: (Andy + Matt) / 2
# IMDB: IMDb rating as at 15th Feb 2023

# load Tidyverse for data tidying and import current dataset ------------
library(tidyverse)

data <- read_csv("star_trek_TNC/data/tnc_stats.csv") %>% 
  filter(Series == "DS9") %>%
  slice(1:20)

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

# Add new variables for character's Division (Command, Ops, Sciences, Civilian, Other) 

var.cmd <- c("Sisko", "Worf", "Kira")
var.ops <- c("O'Brien", "Odo", "Rom")
var.sci <- c("Dax", "Bashir")
var.civ <- c("Jake", "Nog", "Keiko", "Quark", "Garak", "Leeta", "Kassidy")
var.oth <- c("Dukat", "Other")

# Add new variables for character's race (Human, Bajoran, Cardassian, Klingon, Ferengi, Other)

var.hum <- c("Sisko", "O'Brien", "Bashir", "Jake", "Keiko", "Kassidy")
var.baj <- c("Kira", "Leeta")
var.car <- c("Garak", "Dukat")
var.kli <- c("Worf")
var.fer <- c("Quark", "Rom", "Nog")
var.tri <- c("Dax")
var.oth <- c("Changling")

data <- 
  data %>% 
  mutate(A.Div = ifelse(Andy_MVC %in% var.cmd, "Command",
                        ifelse(Andy_MVC %in% var.ops, "Ops",
                               ifelse(Andy_MVC %in% var.sci, "Sciences",
                                      ifelse(Andy_MVC %in% var.civ, "Civilian",
                                             "Other"))))) %>% 
  mutate(M.Div = ifelse(Matt_MVC %in% var.cmd, "Command",
                        ifelse(Matt_MVC %in% var.ops, "Ops",
                               ifelse(Matt_MVC %in% var.sci, "Sciences",
                                      ifelse(Matt_MVC %in% var.civ, "Civilian",
                                             "Other"))))) %>% 
  mutate(A.race = ifelse(Andy_MVC %in% var.hum, "Human",
                         ifelse(Andy_MVC %in% var.baj, "Bajoran",
                                ifelse(Andy_MVC %in% var.car, "Cardassian",
                                       ifelse(Andy_MVC %in% var.kli, "Klingon",
                                              ifelse(Andy_MVC %in% var.fer, "Ferengi",
                                                     ifelse(Andy_MVC %in% var.tri, "Trill",
                                                            "Other"))))))) %>% 
  mutate(M.race = ifelse(Matt_MVC %in% var.hum, "Human",
                         ifelse(Matt_MVC %in% var.baj, "Bajoran",
                                ifelse(Matt_MVC %in% var.car, "Cardassian",
                                       ifelse(Matt_MVC %in% var.kli, "Klingon",
                                              ifelse(Matt_MVC %in% var.fer, "Ferengi",
                                                     ifelse(Matt_MVC %in% var.tri, "Trill",
                                                            "Other")))))))




# summarise the hosts rating data -------------------

host_rating_data <- 
  data %>% 
  pivot_longer(cols = c("Andy_rating", "Matt_rating"), names_to = "Host", values_to = "Rating") %>% 
  select(Episode_number, Episode_name, Host, Rating)

# summarise the joint-host and IMDB data ------------

joint_rating_data <- 
  data %>% 
  pivot_longer(cols = c("TNC", "IMDB"), names_to = "Joint", values_to = "Rating") %>% 
  select(Episode_number, Episode_name, Joint, Rating)

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

# Summarise MVC by joint count of character division -------------------

pivot_MVC_by_div <- 
  data %>% 
  slice(0:eps_watched) %>% 
  pivot_longer(cols = c("A.Div", "M.Div"), names_to = "Host") %>% 
  rename("Division" = "value") %>% 
  select(Host, Division)

(MVC_leaderboardby_div <- 
    pivot_MVC_by_div %>% 
    group_by(Division) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count))) 

# Summarise MVC by joint count of character race -------------------

pivot_MVC_by_race <- 
  data %>% 
  slice(0:eps_watched) %>% 
  pivot_longer(cols = c("A.race", "M.race"), names_to = "Host") %>% 
  rename("Race" = "value") %>% 
  select(Host, Race)

(MVC_leaderboard_by_race <- 
    pivot_MVC_by_race %>% 
    group_by(Race) %>% 
    summarise(count = n()) %>% 
    arrange(desc(count)))

# visualisations ------------------------------------------------  

library(ggplot2)
library(cropcircles) # Credit to: https://github.com/doehm for his excellent pkg!
library(ggimage)
library(lcars)
library(glue)

# create a vector of image 'headshots' of DS9 characters --------

images <- c(
  "star_trek_TNC/images/headshots/s1/bashir.jpg",
  "star_trek_TNC/images/headshots/s1/dax.jpg",
  "star_trek_TNC/images/headshots/s1/jake.jpg",
  "star_trek_TNC/images/headshots/s1/kira.jpg",
  "star_trek_TNC/images/headshots/s1/obrien.jpg",
  "star_trek_TNC/images/headshots/s1/odo.jpg",
  "star_trek_TNC/images/headshots/s1/quark.jpg",
  "star_trek_TNC/images/headshots/s1/sisko.jpg",
  "star_trek_TNC/images/headshots/s1/nog.jpg",
  "star_trek_TNC/images/headshots/s1/garak.jpg",
  "star_trek_TNC/images/headshots/s1/other.jpg"
)

# apply cropcircles to create circular images representing MVCs --

image_df <- tibble(y = 1:11, images = images) %>% 
  mutate(images_cropped = circle_crop(images))

images <- image_df$images_cropped

# merge cropped image df with corresponding MVC in leaderboard -----------------
MVC_leaderboard <- 
  MVC_leaderboard %>% 
  mutate(Character = toupper(Character)) %>% 
  mutate(img = ifelse(Character == "BASHIR", images[1], 
                      ifelse(Character == "DAX", images[2], 
                             ifelse(Character == "JAKE", images[3],
                                    ifelse(Character == "KIRA", images[4],
                                           ifelse(Character == "O'BRIEN", images[5], 
                                                  ifelse(Character == "ODO", images[6], 
                                                         ifelse(Character == "QUARK", images[7], 
                                                                ifelse(Character == "SISKO", images[8], 
                                                                       ifelse(Character == "NOG", images[9], 
                                                                              ifelse(Character == "GARAK", images[10],
                                                                                     images[11])))))))))))


# define DS9 viz themes --------------------------------------------
theme_trek <- function(){
  theme(
    text = element_text(family = "Antonio", colour = "#FFFFFF"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#000000", colour = "#99CCFF"),
    plot.title = element_text(family = "Federation", size = 20, colour = "#F7B05A", vjust = 0),
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
    plot.title = element_text(family = "Federation", size = 24, colour = "#F7B05A", vjust = 0),
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


# viz for bar plot of the DS9 season 1 MVC leaderboard ------------------

sub_title <- glue("SEASON ONE: MVC VOTE LEADERBOARD ({eps_watched}/19 EPISODES)")
current_ep <- data$Episode_name[eps_watched]
current_ep_num <- data$Episode_number[eps_watched]

MVC_plt <- 
  MVC_leaderboard %>% 
  ggplot(aes(x=count, y=fct_reorder(Character, count))) +
  geom_bar(stat = "identity", fill = "#9C9CFF") +
  geom_image(aes(x=count, y=fct_reorder(Character, count), image = img), size = 0.1) +
  geom_text(aes(x=8.5, y=8, label = toupper(glue("+ 2 VOTES ({current_ep})")), family = "Antonio"), color = "#ED884C", size = 4) +
  labs(
    subtitle = sub_title,
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme_trek()

# viz for individual host rating data -----------------------------------

host_rating_plt <- 
  host_rating_data %>% 
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

# viz for TNC joint ratings in descending order of ranking -----------------------------------

joint_rating_plt_desc <- 
  data %>% 
  mutate(Episode_name = toupper(Episode_name)) %>% 
  ggplot(aes(x=TNC, y=fct_reorder(Episode_name, TNC), fill = as.factor(TNC))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    subtitle = "SEASON ONE: EPISODE RATINGS - JOINT",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  scale_fill_brewer(palette = "Spectral") +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust =1),
        legend.title = element_blank(),
        legend.position = "right") +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() +
  scale_y_discrete(limits = rev)

# viz for joint rating vs IMDb rating -----------------------------------

joint_vs_imdb_rating_plt <- 
  joint_rating_data %>% 
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

# viz for TNC joint ratings in order of episode sequence -----------------------------------

joint_rating_plt_seq <- 
  data %>% 
  mutate(Episode_name = toupper(Episode_name)) %>% 
  ggplot(aes(x=TNC, y=fct_reorder(Episode_name, Episode_number), fill = as.factor(TNC))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    subtitle = "SEASON ONE: EPISODE RATINGS - JOINT",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
  scale_fill_brewer(palette = "Spectral") +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust =1),
        legend.title = element_blank(),
        legend.position = "right") +
  guides(fill = guide_legend(reverse = TRUE)) +
  coord_flip() 

# view each of the plots -----------------------------------------------------

MVC_plt
host_rating_plt
joint_rating_plt_seq
joint_vs_imdb_rating_plt
joint_rating_plt_desc

library(patchwork) 

# patchwork everything together and add a subtitle ---------------------------

patch_plt <- 
  (host_rating_plt / joint_vs_imdb_rating_plt) |
  (MVC_plt)

patch_plt + plot_annotation(title = 'DEEP SPACE NINE', 
                            subtitle = "STAR TREK: THE NEXT CONVERSATION",
                            caption = "BROUGHT TO YOU BY TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins",theme=theme_trek_header())  

ggsave("exports/ds9_s1_3panel.png",width = 36, height = 24, units = "cm") 

# generate plots of the MVC ranking trend ------------------------------------

library(ggbump)

tally <- 
  data %>% 
  slice(0:eps_watched) %>% 
  pivot_longer(cols = c("Andy_MVC", "Matt_MVC"), names_to = "Host") %>% 
  rename("Character" = "value") %>% 
  select(Episode_name, Episode_number, Host, Character) %>% 
  group_by(Episode_name, Episode_number, Character) %>% 
  summarise(count = n()) %>% 
  arrange(Episode_number) %>% 
  pivot_wider(names_from = Character, values_from = count, values_fill = 0) %>% 
  pivot_longer(cols = 3:11, names_to = "Character", values_to = "count") %>% 
  group_by(Character) %>% 
  mutate(total = cumsum(count),
         Character = toupper(factor(Character)),
         Episode_number = as.integer(Episode_number)) %>% 
  mutate(img = ifelse(Character == "BASHIR", images[1], 
                      ifelse(Character == "DAX", images[2], 
                             ifelse(Character == "JAKE", images[3],
                                    ifelse(Character == "KIRA", images[4],
                                           ifelse(Character == "O'BRIEN", images[5], 
                                                  ifelse(Character == "ODO", images[6], 
                                                         ifelse(Character == "QUARK", images[7], 
                                                                ifelse(Character == "SISKO", images[8], 
                                                                       ifelse(Character == "NOG", images[9], 
                                                                              ifelse(Character == "GARAK", images[10],
                                                                                     images[11])))))))))))


ranks <- tally %>% 
  group_by(Episode_number) %>% 
  mutate(rank = rank(total, ties.method = "first")) %>% 
  select(-count, -total)

sel_chr <- c("BASHIR", "DAX", "O'BRIEN", 
             "KIRA", "SISKO", "JAKE", 
             "NOG", "ODO", "OTHER")

current_rank <- 
  ranks %>% 
  mutate(Character = as_factor(Character)) %>% 
  filter(Episode_number == current_ep_num) %>% 
  arrange(desc(rank)) 

current_rank <- current_rank$Character

pos <- ranks %>% 
  filter(Episode_number == current_ep_num, Character %in% sel_chr)

lcars_pal1 <- c("#FF9C00", "#FFFF33", "#CC99CC", 
                "#DDFFFF", "#3399FF", "#99FF66", 
                "#FFCC33", "#31C924", "#4D6184")

sub_title <- glue("SEASON ONE: MVC VOTE RANKING TREND ({eps_watched}/19 EPISODES)")

# Highlight particular rank
hlt_rank_plt <- function(selection){
  deselect_ranks <- ranks %>% 
    filter(Character %in% sel_chr) %>% 
    filter(Character != selection)
  
  select_ranks <- ranks %>% 
    filter(Character %in% sel_chr) %>% 
    filter(Character == selection)
  
  ggplot() +
    geom_bump(data = deselect_ranks, aes(x=Episode_number, y=rank, colour = fct_reorder(Character, rank)), size = 0.5, alpha = 0.25) +
    geom_bump(data = select_ranks, aes(x=Episode_number, y=rank, colour = fct_reorder(Character, rank)), size = 1, alpha = 0.5) +
    scale_color_manual(values = lcars_pal1) +
    geom_image(data=pos, aes(x=Episode_number + 0.5, y=rank, image = img), size = 0.05) +
    theme_trek() +
    labs(
      subtitle = sub_title,
      x = element_blank(),
      y = element_blank()) +
    theme(axis.text.y = element_blank(),
          legend.title = element_blank(),
          legend.key = element_rect(fill = "#000000", color = NA),
          legend.position = "right") +
    scale_x_continuous(limits = c(0, 20), breaks = c(1:19)) 
} 

# MVC_plt
MVC_ranks_plt <- hlt_rank_plt("ODO")

MVC_votes_rank  <- MVC_plt | MVC_ranks_plt

MVC_votes_rank + plot_annotation(title = 'DEEP SPACE NINE', 
                                 subtitle = "STAR TREK: THE NEXT CONVERSATION",
                                 caption = "BROUGHT TO YOU BY TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins",theme=theme_trek_header())  


ggsave("exports/ds9_s1_ranks_e19.png", width = 48, height = 24, units = "cm") 

# -------------------------------------------------
# -------------------------------------------------

# Cluster analysis of episode rankings
library(ggrepel)

k_data <- data %>% slice(0:(current_ep_num-1)) %>%  select("Episode_name", "Matt_rating", "Andy_rating", "TNC", "IMDB")
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

stellar_palette <- c("#66CCFF", "#99FF66", "#FF9C00", "#9C9CFF")

theme_trek_clust <- function(){
  theme(
    text = element_text(family = "Antonio", colour = "#FFFFFF"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#000000", colour = "#99CCFF"),
    plot.title = element_text(family = "Federation", size = 28, colour = "#F7B05A", vjust = 0),
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
  annotate("text", x=1, y=1, size = 8, colour = "#FFCC33", label = "DABO!", fontface = 2) +
  annotate("text", x=0, y=0, size = 8, colour = "#FFCC33", label = "NEUTRAL ZONE", fontface = 2) +
  annotate("text", x=-1, y=-1, size = 8, colour = "#FFCC33", label = "BADLANDS", fontface = 2) +
  annotate("text", x=-2, y=-2, size = 8, colour = "#FFCC33", label = "HELL", fontface = 2) +
  geom_point(aes(x=TNC, y=IMDB, colour = factor(cluster), size = joint)) +
  geom_label_repel(aes(x=TNC, y=IMDB, label=Episode_name),
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
  scale_color_manual(values = stellar_palette) +
  labs(title = "DEEP SPACE NINE",
       subtitle = "EPISODE GUIDE: SEASON 1 QUADRANT (K-MEANS CLUSTER MODEL)",
       caption = "BROUGHT TO YOU BY TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins",
       x = "", y = "") +
  theme_trek_clust() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.key = element_rect(fill = "#000000", color = NA)) 


ggsave("exports/ds9_s1_ep_cluster.png", width = 36, height = 24, units = "cm") 
