# Data EDA for Star Trek: The Next Conversation podcast ---
# Hosted by Matt Mira and Andy Secunda --------------------

# This EDA script is for their episodes reviewing DS9 -----

# Latest update: v1.1.1 (18th Feb 2023) -------------------

# Variable key:
# Andy_rating/Matt_rating: rating out of 10 'Andys' for each episode.
# Andy_MVC/Matt_MVC: Most Valuable Character vote for each episode.
# Andy_Watch/Matt_Watch: Whether the host recommends viewings watch a given episode.
# TNC: The averaged rating - i.e: (Andy + Matt) / 2
# IMDB: IMDb rating as at 15th Feb 2023

# load Tidyverse for data tidying and import current dataset ------------
library(tidyverse)

data <- read_csv("data/tnc_ds9_stats-230215.csv") %>% 
  slice(1:19)

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

MVC_leaderboard <- 
pivot_MVC %>% 
  group_by(Character) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

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


# visualisations ------------------------------------------------  

library(ggplot)
library(cropcircles) # Credit to: https://github.com/doehm for his excellent pkg!
library(ggimage)
library(lcars)
library(glue)

# create a vector of image 'headshots' of DS9 characters --------

images <- c(
  "images/headshots/s1/bashir.jpg",
  "images/headshots/s1/dax.jpg",
  "images/headshots/s1/jake.jpg",
  "images/headshots/s1/kira.jpg",
  "images/headshots/s1/obrien.jpg",
  "images/headshots/s1/odo.jpg",
  "images/headshots/s1/quark.jpg",
  "images/headshots/s1/sisko.jpg",
  "images/headshots/s1/nog.jpg",
  "images/headshots/s1/garak.jpg",
  "images/headshots/s1/other.jpg"
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
    plot.background = element_rect(fill = "#000000"),
    plot.title = element_text(family = "Federation", size = 24, colour = "#F7B05A", vjust = 0),
    plot.subtitle = element_text(family = "Antonio", face = "bold", size = 18, colour = "#99CCFF"),
    axis.title.x = element_text(family = "Antonio", face = "bold", size = 14),
    axis.title.y = element_text(family = "Antonio", face = "bold", size = 14),
    axis.text.x = element_text(family = "Antonio", face = "bold", size = 14, colour = "#646DCC"),
    axis.text.y = element_text(family = "Antonio", face = "bold", size = 18, colour = "#646DCC"),
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
    plot.background = element_rect(fill = "#000000"),
    plot.title = element_text(family = "Federation", size = 36, colour = "#F7B05A", vjust = 0),
    plot.subtitle = element_text(family = "Antonio", face = "bold", size = 18, colour = "#99CCFF"),
    axis.title.x = element_text(family = "Antonio", face = "bold", size = 14),
    axis.title.y = element_text(family = "Antonio", face = "bold", size = 14),
    axis.text.x = element_text(family = "Antonio", face = "bold", size = 14, colour = "#646DCC"),
    axis.text.y = element_text(family = "Antonio", face = "bold", size = 18, colour = "#646DCC"),
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

MVC_plt <- 
MVC_leaderboard %>% 
  ggplot(aes(x=count, y=fct_reorder(Character, count))) +
  geom_bar(stat = "identity", fill = "#9C9CFF") +
  geom_image(aes(x=count, y=fct_reorder(Character, count), image = img), size = 0.1) +
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

# define a blank plot template with the DS9 typeface and subtitle ------------ 

template <- 
ggplot() +
  geom_point() +
  labs(title = "DEEP SPACE NINE") +
  theme_trek()

# patchwork everything together and add a subtitle ---------------------------

patch_plt <- 
(host_rating_plt / joint_vs_imdb_rating_plt) |
  (MVC_plt)

patch_plt + plot_annotation(title = 'DEEP SPACE NINE', 
                            subtitle = "STAR TREK: THE NEXT CONVERSATION",
                            caption = "BROUGHT TO YOU BY ODO'S SMOOTH BROW. GITHUB: https://github.com/TristanLouthRobins",theme=theme_trek_header())  
