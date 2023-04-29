# Data EDA for Star Trek: The Next Conversation podcast ------------------------
# Hosted by Matt Mira and Andy Secunda -----------------------------------------
# This EDA script is for their episodes reviewing DS9 --------------------------
# Season 1 (19 episodes in total) ----------------------------------------------
# Latest update: v1.2.22 (22 April 2023) ---------------------------------------

library(tidyverse)

# import dataset ---------------------------------------------------------------
data <- read_csv("star_trek_TNC/data/tnc_stats.csv") %>% 
  filter(Series == "DS9") %>%
  slice(1:19)

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

# add new variables for character's Division (Command, Ops, Sciences, Civilian, Other) --
var.cmd <- c("Sisko", "Worf", "Kira")
var.ops <- c("O'Brien", "Odo", "Rom")
var.sci <- c("Dax", "Bashir")
var.civ <- c("Jake", "Nog", "Keiko", "Quark", "Garak", "Leeta", "Kassidy")
var.oth <- c("Dukat", "Other")

# Add new variables for character's race (Human, Bajoran, Cardassian, Klingon, Ferengi, Other) --
var.hum <- c("Sisko", "O'Brien", "Bashir", "Jake", "Keiko", "Kassidy")
var.baj <- c("Kira", "Leeta")
var.car <- c("Garak", "Dukat")
var.kli <- c("Worf")
var.fer <- c("Quark", "Rom", "Nog")
var.tri <- c("Dax")
var.oth <- c("Changling")

data <- 
  data %>% 
  # assign divisions to characters --
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
  # assign races to characters --
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
  arrange(desc(count))

# summarise MVC by joint count of character division ---------------------------
(MVC_leaderboardby_div <- data %>% 
  slice(0:eps_watched) %>% 
  pivot_longer(cols = c("A.Div", "M.Div"), names_to = "Host") %>% 
  rename("Division" = "value") %>% 
  select(Host, Division) %>% 
  group_by(Division) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))) 

# summarise MVC by joint count of character race -------------------------------
(MVC_leaderboard_race <- 
  data %>% 
  slice(0:eps_watched) %>% 
  pivot_longer(cols = c("A.race", "M.race"), names_to = "Host") %>% 
  rename("Race" = "value") %>% 
  select(Host, Race) %>% 
  group_by(Race) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)))

# visualisations --------------------------------------------------------------- 

library(ggplot2)
library(cropcircles) # Credit to: https://github.com/doehm for his excellent pkg!
library(ggimage)
library(ggrepel)
library(ggbump)
library(ggtext)
library(glue)
library(showtext)
library(patchwork) 

font_add("Federation", "/Users/tristanlouth-robins/Library/Fonts/DS9_Title.ttf")
font_add("Antonio", "/Users/tristanlouth-robins/Library/Fonts/Antonio-VariableFont_wght.ttf")

# create a vector of image 'headshots' of DS9 characters -----------------------
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

# apply cropcircles to create circular images representing MVCs ----------------
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


# define DS9 viz themes --------------------------------------------------------
lcars_pal1 <- c("#FF9C00", "#FFFF33", "#CC99CC", 
                "#DDFFFF", "#3399FF", "#99FF66", 
                "#FFCC33", "#31C924", "#4D6184")

# the general LCARS theme for visualisations -----------------------------------
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

# the base layer theme for patchworked visualisations --------------------------
theme_trek_header <- function(){
  theme(
    text = element_text(family = "Antonio", colour = "#FFFFFF"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#000000", colour = "000000"),
    plot.title = element_text(family = "Federation", size = 24, colour = "#F7B05A", vjust = 0),
    plot.subtitle = element_text(family = "Antonio", face = "bold", size = 18, colour = "#99CCFF"),
    panel.background = element_rect(fill = "#000000"),
  )
}

# cluster analysis plot aesthetics and theme -----------------------------------
cluster_txtcol <- "#3786FF" 
stellar_pal <- c("#CD6363", "#99FF66", "#FF9C00", "#9C9CFF")

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
    panel.grid.major = element_line(colour = "#191a18"), 
    panel.grid.minor = element_blank()
  )
}

# 1. plotting the episode scores -----------------------------------------------
# bar plot for individual host rating data -------------------------------------
consensus <- data %>% 
  select(Andy_rating, Matt_rating) %>% 
  mutate(diff = Andy_rating - Matt_rating) %>% 
  mutate(diff.cat = ifelse(diff >= -0.5 & diff <= 0.5, "Agree", 
                           ifelse(diff > 0.5, "Disagree +", "Disagree -"))) %>%
  mutate(who.scored.higher = ifelse(diff.cat == "Disagree +", "Andy",
                                    ifelse(diff.cat == "Disagree -", "Matt", "Consensus"))) %>% 
  arrange(desc(diff)) %>% 
  group_by(who.scored.higher) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(prop = round(count/sum(count) * 100, 0))

host_rating_caption <- str_wrap(glue("The hosts Season {data$Season[1]} scores matched or were very close on {consensus$count[2]} occassions, {consensus$prop[2]}% of the time and Andy consistently scored higher than 
                                     Matt {consensus$count[1]} times ({consensus$prop[1]}%) compared to only {consensus$count[3]} instances when Matt scored higher than Andy. Their average scores
                                     for this season were: {round(summary_stats$mean[2],1)} (Andy) and {round(summary_stats$mean[4],1)} (Matt)"), 100)

host_rating_plt <- 
  data %>% 
  # pivot the data
  pivot_longer(cols = c("Andy_rating", "Matt_rating"), names_to = "Host", values_to = "Rating") %>% 
  select(Episode_number, Episode_name, Host, Rating) %>% 
  mutate(Episode_name = toupper(Episode_name)) %>% 
  # visualise
  ggplot(aes(x=Rating, y=fct_reorder(Episode_name, Episode_number), fill = Host)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = summary_stats$mean[2], colour = "#FFCC33", size = 1, alpha = 0.7) +
  geom_vline(xintercept = summary_stats$mean[4], colour = "#3399FF", size = 1, alpha = 0.7) +
  annotate(geom = "text", x=10, y=data$Episode_number[2], label=host_rating_caption, size=4, colour="#E7FFFF", family = "Antonio", hjust=0, vjust=1, lineheight = 1.3) +
  labs(
    subtitle = "EPISODE RATINGS - THE HOSTS",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0:10))  +
  scale_fill_manual(values = c("#FFCC33", "#3399FF"),
                    labels = c("ANDY", "MATT")) +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust =1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  coord_flip()

# bar plot for joint TNC rating vs IMDb ----------------------------------------
consensus.2 <- data %>% 
  select(TNC, IMDB) %>% 
  mutate(diff = TNC - IMDB) %>% 
  mutate(diff.cat = ifelse(diff >= -0.5 & diff <= 0.5, "Agree", 
                           ifelse(diff > 0.5, "Disagree +", "Disagree -"))) %>%
  mutate(who.scored.higher = ifelse(diff.cat == "Disagree +", "TNC",
                                    ifelse(diff.cat == "Disagree -", "IMDB", "Consensus"))) %>% 
  arrange(desc(diff)) %>% 
  group_by(who.scored.higher) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(prop = round(count/sum(count) * 100, 0))

TNCIMDB_rating_caption <- str_wrap(glue("The TNC/IMDb Season {data$Season[1]} scores matched or were very close on only {consensus.2$count[1]} occassions, {consensus.2$prop[1]}% of the time and the IMDb scores were consistently higher than 
                                     the joint TNC score {consensus.2$count[2]} times ({consensus.2$prop[2]}%) compared to only {consensus.2$count[3]} instance when the joint TNC score was higher than that of IMDb ('Emissary').
                                     The average scores for this season were: {round(summary_stats$mean[3],1)} (TNC) and {round(summary_stats$mean[1],1)} (IMDb)"), 100)

joint_vs_imdb_rating_plt <- 
  data %>% 
  pivot_longer(cols = c("TNC", "IMDB"), names_to = "Joint", values_to = "Rating") %>% 
  select(Episode_number, Episode_name, Joint, Rating) %>% 
  mutate(Episode_name = toupper(Episode_name),
         Joint = as.factor(Joint)) %>% 
  ggplot(aes(x=Rating, y=fct_reorder(Episode_name, Episode_number), fill = Joint)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_vline(xintercept = summary_stats$mean[1], colour = "#FFFF33", size = 1, alpha = 0.7) +
  geom_vline(xintercept = summary_stats$mean[3], colour = "#72E2E4", size = 1, alpha = 0.7) +
  annotate(geom = "text", x=10, y=data$Episode_number[2], label=TNCIMDB_rating_caption, size=4, colour="#E7FFFF", family = "Antonio", hjust=0, vjust=1, lineheight = 1.3) +
  labs(
    subtitle = "EPISODE RATINGS - JOINT TNC SCORE VS IMDb",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0,2,4,6,8,10)) +
  scale_fill_manual(values = c("#FFFF33", "#72E2E4"),
                    labels = c("IMDb", "TNC")) +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust =1),
        legend.title = element_blank(),
        legend.position = "bottom") +
  coord_flip()

host_rating_plt
joint_vs_imdb_rating_plt

# patch these two plots together -----------------------------------------------

ep_ratings_plt <- (host_rating_plt | joint_vs_imdb_rating_plt) + plot_annotation(title = 'DEEP SPACE NINE: SEASON 1', 
                            subtitle = "STAR TREK: THE NEXT CONVERSATION",
                            caption = "BROUGHT TO YOU BY TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins",theme=theme_trek_header())  

ggsave("exports-final/ds9-final_s1_scores.png",width = 48, height = 24, units = "cm") 

# 2. plotting the MVC (Most Valuable Character/Crewmember) ---------------------
# bar plot of the DS9 MVC leaderboard ------------------------------------------
sub_title <- glue("MVC VOTE LEADERBOARD ({eps_watched}/19 EPISODES)")
current_ep <- data$Episode_name[eps_watched]
current_ep_num <- data$Episode_number[eps_watched]

total_votes <- sum(MVC_leaderboard$count)

MVClb_caption <- str_wrap(glue("With {total_votes} available votes for this season, Kira was the MVC for Season {data$Season[1]} with a total of {MVC_leaderboard$count[1]} votes. 
Although she maintained a solid lead into the latter half of the season, O'Brien nearly pulled off a surprise win in the final episodes, picking up a total of {MVC_leaderboard$count[2]} votes. 
                               Odo also made a late charge as well to pick up up a total of {MVC_leaderboard$count[3]} votes."), 45)

# update MVC leaderboard dt with MVC division colours --------------------------
MVC_leaderboard <- MVC_leaderboard %>% 
  mutate(div_col = ifelse(Character %in% c("KIRA", "SISKO"), '#CD6363', # Command
                          ifelse(Character %in% c("O'BRIEN", "ODO"), '#B5A424', # Ops / Security
                                                  ifelse(Character %in% c("BASHIR", "DAX"), '#3399FF', '#4C4D47')))) # Sciences, if not: Civilian/Other

MVC_plt <- 
  MVC_leaderboard %>% 
  ggplot(aes(x=count, y=fct_reorder(Character, count))) +
  geom_bar(aes(fill = div_col), stat = "identity") +
  scale_fill_identity() +
  geom_image(aes(x=count, y=fct_reorder(Character, count), image = img), size = 0.1) +
  # MVC 1
# geom_text(aes(x=2, y=9, label = toupper(glue("+ 1 VOTE ({current_ep})")), family = "Antonio"), color = "#FFFF9C", size = 4) + 
  # MVC 2
# geom_text(aes(x=2, y=7, label = toupper(glue("+ 1 VOTE ({current_ep})")), family = "Antonio"), color = "#FFFF9C", size = 4) +
  annotate(geom = "text", x=5, y=5 + 0.2 ,label=MVClb_caption, size=5, colour="#E7FFFF", family = "Antonio", hjust=0, vjust=1, lineheight = 1.3) +
  labs(
    subtitle = "",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme_trek() +
  theme(axis.text.y = element_text(family = "Antonio", face = "bold", size = 14, colour = "#646DCC"))

# in order to generate a plot of MVC rankings (i.e. the order of rankings in episode sequence), 
# I'll need to perform some more data manipulation. ----------------------------------------------
MVC.tally <- 
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


ranks <- MVC.tally %>% 
  group_by(Episode_number) %>% 
  mutate(rank = rank(total, ties.method = "first")) 

sel_chr <- c("BASHIR", "DAX", "O'BRIEN", "KIRA", "SISKO", "JAKE", "NOG", "ODO", "OTHER")

current_rank <- 
  ranks %>% 
  mutate(Character = as_factor(Character)) %>% 
  filter(Episode_number == current_ep_num) %>% 
  arrange(desc(rank)) 

current_rank <- current_rank$Character

pos <- ranks %>% 
  filter(Episode_number == current_ep_num, Character %in% sel_chr)

sub_title <- glue("MVC VOTE RANKING TREND ({eps_watched}/19 EPISODES)")

# Highlight particular rank
hlt_rank_plt <- function(selection){
  deselect_ranks <- ranks %>% 
    filter(Character %in% sel_chr) %>% 
    filter(Character != selection)
  
  select_ranks <- ranks %>% 
    filter(Character %in% sel_chr) %>% 
    filter(Character %in% selection)
  
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

# MVC_plt - highlight the top 3 MVCs for this season -----
MVC_ranks_plt <- hlt_rank_plt(c("O'BRIEN", "ODO", "KIRA"))

# patch these plots together ---------------------------------------------------

MVC_plt <- (MVC_plt | MVC_ranks_plt) + plot_annotation(title = 'DEEP SPACE NINE: SEASON 1', 
                                                                                 subtitle = "STAR TREK: THE NEXT CONVERSATION - MVC LEADERBOARD",
                                                                                 caption = "BROUGHT TO YOU BY TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins",theme=theme_trek_header())  

ggsave("exports-final/ds9-final_s1_MVC.png",width = 48, height = 24, units = "cm") 

# animate the MVC leaderboard --------------------------------------------------
library(gganimate)
ranks <- ranks %>% 
  mutate(div_col = ifelse(Character %in% c("KIRA", "SISKO"), '#CD6363', # Command
                          ifelse(Character %in% c("O'BRIEN", "ODO"), '#B5A424', # Ops / Security
                                 ifelse(Character %in% c("BASHIR", "DAX"), '#3399FF', '#4C4D47')))) # Sciences, if not: Civilian/Other

anim <- 
ranks %>% 
  ggplot(aes(rank, group = Character)) +
  geom_tile(aes(y = total/2, 
                height = total,
                width = 0.9, fill = div_col), alpha = 0.9) +
  scale_fill_identity() +
  geom_text(aes(y = 0, label = Character), colour = "#FFFFFF", family = "Antonio", size = 4, hjust = -0.1) +
  geom_image(aes(y=total + 0.6, image = img), size = 0.09) +
  coord_cartesian(clip = "off", expand = FALSE) +
  scale_y_continuous(limits = c(0, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  coord_flip() +
  theme_trek() +
  theme(axis.text.y = element_blank()) +
  labs(title = "DEEP SPACE NINE", 
       subtitle = "EPISODE NUMBER: {closest_state}/20", x = "", y= "") +
  transition_states(Episode_number, 
                    transition_length = 10,
                    state_length = 5) +
  ease_aes('cubic-in-out')

animate(anim, fps = 25,
        duration = 20,
        width = 400,
        height = 400,
        end_pause = 60)

# cluster analysis of episode rankings -----------------------------------------

k_data <- data %>% slice(0:(current_ep_num-1)) %>%  select("Episode_name", "Matt_rating", "Andy_rating", "TNC", "IMDB")
episodes <- k_data[,1]
tnc_score <- k_data[,4] # Here, we're grabbing the TNC score as an un-scaled variable
imdb_score <- k_data[,5] # As above, but with IMDB
joint_avg <- (tnc_score + imdb_score) / 2 
k_data <- k_data[2:5]
scaled_k_data <- scale(k_data)
model <- kmeans(scaled_k_data, centers = 4)
cluster <- model$cluster
scaled_k_data <- cbind(scaled_k_data, cluster)
complete_k_data <- cbind(episodes, scaled_k_data, joint_avg) %>% 
  rename(joint = 7)

# annotation for the below plot ------------------------------------------------
cluster_caption <- str_wrap(glue("All the rating data (Andy, Matt, Joint TNC and IMDb) is used for the model, which essentially standardises the scores 
                                 and clusters episodes together by colour and their proximity to their k-Centroid. We can see that the episode 'Duet' is the clear
                                 outlier followed by more regular clusters from top-right to the bottom-left, where we find poor old 
                                 'Move Along Home'. Along with the coloured clustering, anything within the space of OO-MOX through to DABO is essential-to-worthwhile viewing. 
                                 As we drift into the NEUTRAL ZONE and towards the BADLANDS (and eventually HELL), the watchability of episodes diminishes substatially."), 120)

complete_k_data %>% 
  ggplot() +
  stat_density_2d(aes(x=TNC, y=IMDB), colour = "#46616E") +
  annotate(geom = "text", x=-2.2, y=3.2 ,label=cluster_caption, size=5, colour="#E7FFFF", family = "Antonio", hjust=0, vjust=1, lineheight = 1.3) +
  annotate("text", x=2, y=2, size = 8, colour = cluster_txtcol, label = "OO-MOX", fontface = 2, family = "Antonio") +
  annotate("text", x=1, y=1, size = 8, colour = cluster_txtcol, label = "DABO", fontface = 2, family = "Antonio") +
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
  labs(title = "DEEP SPACE NINE: SEASON 1",
       subtitle = "EPISODE GUIDE: SEASON QUADRANT (K-MEANS CLUSTER MODEL)",
       caption = "BROUGHT TO YOU BY TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins",
       x = "", y = "") +
  theme_trek_clust() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.key = element_rect(fill = "#000000", color = NA)) 

ggsave("exports-final/ds9-final_s1_cluster.png.png", width = 36, height = 24, units = "cm") 

# summary statistics in one big infographic ------------------------------------

# Combined scores (Andy, Matt, TNC, IMDB)
# 1. plotting the episode scores -----------------------------------------------
# bar plot for averaged scores--------------------------------------------------
all_rating_plt <- 
  data %>% 
  # pivot the data
  pivot_longer(cols = c("Andy_rating", "Matt_rating", "TNC", "IMDB"), names_to = "Scores", values_to = "Rating") %>% 
  select(Episode_number, Episode_name, Scores, Rating) %>% 
  mutate(Episode_name = toupper(Episode_name)) %>% 
  group_by(Episode_name, Episode_number) %>% 
  summarise(Weighted = mean(Rating)) %>% 
  # visualise
  ggplot(aes(fill = Weighted)) +
  geom_bar(aes(x=Weighted, y=fct_reorder(Episode_name, Episode_number)), stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "#CD6363", high = "#99FF66") + 
  geom_text(aes(x=Weighted + 0.5, y=fct_reorder(Episode_name, Episode_number), label = round(Weighted,1)), family="Antonio", colour = "#FFFF33", size = 8) +
#  geom_vline(xintercept = summary_stats$mean[2], colour = "#FFCC33", size = 1, alpha = 0.7) +
  labs(
    subtitle = "EPISODE RATINGS - MATT, ANDY, JOINT TNC AND IMDB AVERAGED TOGETHER",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0:10))  +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust =1, family = "Antonio", face = "bold", size = 18, colour = "#646DCC"),
        axis.text.y = element_text(family = "Antonio", face = "bold", size = 12, colour = "#646DCC"),
        legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()

all_rating_plt

ggsave("exports-final/final_s1_all_scores.png",width = 48, height = 24, units = "cm") 

# Top 3 episodes / Bottom 3 episodes

# Most valuable crewmember (w/runner up)

