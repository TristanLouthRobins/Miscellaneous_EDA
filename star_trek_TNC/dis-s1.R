# Data EDA for Star Trek: The Next Conversation podcast ------------------------
# Hosted by Matt Mira and Andy Secunda -----------------------------------------
# This EDA script is for their episodes reviewing Star Trek: DIS ---------------
# Season 1 (10 episodes in total) ----------------------------------------------
# Latest update: v1.10 (6 June 2023) ------------------------------------------
library(tidyverse)

getwd()

# import dataset ---------------------------------------------------------------
data <- read_csv("star_trek_TNC/data/tnc.csv") %>% 
  filter(Series == "DIS",
         Season == 1)

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
var.cmd <- c("Pike", "Una", "Ortegas", "Kirk")
var.ops <- c("Hemmer", "Uhura", "La'an")
var.sci <- c("Chapel", "M'Benga", "Spock")
var.civ <- c("Other")
var.oth <- c("Other")

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
                                             "Other")))))

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
  arrange(desc(count)) %>% 
  na.omit()

# summarise MVC by joint count of character division ---------------------------
(MVC_leaderboardby_div <- data %>% 
   slice(0:eps_watched) %>% 
   pivot_longer(cols = c("A.Div", "M.Div"), names_to = "Host") %>% 
   rename("Division" = "value") %>% 
   select(Host, Division) %>% 
   group_by(Division) %>% 
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

font_add("Star Trek TNG-Title", "/Users/tristanlouth-robins/Library/Fonts/Star Trek TNG-Title Regular.ttf")
font_add("Antonio", "/Users/tristanlouth-robins/Library/Fonts/Antonio-VariableFont_wght.ttf")
font_add("Jefferies Extended", "/Users/tristanlouth-robins/Library/Fonts/JEFFE___.TTF")

# create a vector of image 'headshots' of DS9 characters -----------------------
images_snw <- c(
  "star_trek_TNC/images/headshots/snw/chapel.jpg",
  "star_trek_TNC/images/headshots/snw/hemmer.jpg",
  "star_trek_TNC/images/headshots/snw/kirk.jpg",
  "star_trek_TNC/images/headshots/snw/laan.jpg",
  "star_trek_TNC/images/headshots/snw/mbenga.jpg",
  "star_trek_TNC/images/headshots/snw/ortegas.jpg",
  "star_trek_TNC/images/headshots/snw/pike.jpg",
  "star_trek_TNC/images/headshots/snw/spock.jpg",
  "star_trek_TNC/images/headshots/snw/uhura.jpg",
  "star_trek_TNC/images/headshots/snw/una.jpg")

# apply cropcircles to create circular images representing MVCs ----------------
image_df <- tibble(y = 1:10, images = images_snw) %>% 
  mutate(images_cropped = circle_crop(images))

images <- image_df$images_cropped

# check sequence of image files for mapping below ------------------------------
image_df %>% 
  ggplot() +
  geom_image(aes(1.5, y, image = images), size = 0.15) +
  geom_image(aes(3.5, y, image = images_cropped), size = 0.15) +
  xlim(0, 11) +
  ylim(0, 11) +
  coord_fixed()

# merge cropped image df with corresponding MVC in leaderboard -----------------
MVC_leaderboard <- 
  MVC_leaderboard %>% 
  mutate(Character = toupper(Character)) %>% 
  mutate(img = ifelse(Character == "CHAPEL", images[1], 
                      ifelse(Character == "HEMMER", images[2], 
                             ifelse(Character == "KIRK", images[3],
                                    ifelse(Character == "LA'AN", images[4],
                                           ifelse(Character == "M'BENGA", images[5], 
                                                  ifelse(Character == "ORTEGAS", images[6], 
                                                         ifelse(Character == "PIKE", images[7], 
                                                                ifelse(Character == "SPOCK", images[8], 
                                                                       ifelse(Character == "UHURA", images[9], 
                                                                              ifelse(Character == "UNA", images[10],
                                                                                     images[10])))))))))))





# define viz themes --------------------------------------------------------
lcars_pal1 <- c("#FF9C00", "#FFFF33", "#CC99CC", 
                "#DDFFFF", "#3399FF", "#99FF66", 
                "#FFCC33", "#31C924", "#4D6184")

# the general LCARS theme for visualisations -----------------------------------
theme_trek <- function(){
  theme(
    text = element_text(family = "Antonio", colour = "#FFFFFF"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#000000", colour = "#99CCFF"),
    plot.title = element_text(family = "Jefferies", size = 20, colour = "#FFFF33", vjust = 0),
    plot.subtitle = element_text(family = "Antonio", face = "bold", size = 18, colour = "#99CCFF"),
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
    plot.title = element_text(family = "Star Trek TNG-Title", size = 24, colour = "#3399FF", vjust = 0),
    plot.subtitle = element_text(family = "Antonio", face = "bold", size = 24, colour = "#99CCFF"),
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
    plot.title = element_text(family = "Star Trek TNG-Title", size = 28, colour = "#3786FF", vjust = 0),
    plot.subtitle = element_text(family = "Antonio", face = "bold", size = 18, colour = "#99CCFF"),
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
compare.AndyMatt <- data %>% 
  select(Andy_rating, Matt_rating) %>% 
  mutate(diff = Andy_rating - Matt_rating) %>% 
  mutate(diff.type = ifelse(diff >= -0.5 & diff <= 0.5, "Close",
                            ifelse(diff > 0.5, "Andy:Disagree +", "Matt:Disagree -"))) %>% 
  arrange(desc(diff)) %>% 
  group_by(diff.type) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(prop = round(count/sum(count) * 100, 0))

scored.AndyMatt <- data %>% 
  select(Andy_rating, Matt_rating) %>% 
  mutate(diff = Andy_rating - Matt_rating) %>% 
  mutate(whos.higher = ifelse(diff > 0, "Andy",
                              ifelse(diff < 0, "Matt",
                                     ifelse(diff == 0, "MATCH!", "???")))) %>% 
  arrange(desc(diff)) %>% 
  group_by(whos.higher) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(prop = round(count/sum(count) * 100, 0))

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
  #  annotate(geom = "text", x=10, y=data$Episode_number[1], label=host_rating_caption, size=4, colour="#E7FFFF", family = "Antonio", hjust=0, vjust=1, lineheight = 1.3) +
  labs(
    subtitle = "THE HOSTS: ANDREW SECUNDA & MATT MIRA",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0:10))  +
  scale_fill_manual(values = c("#FFCC33", "#3399FF"),
                    labels = c("ANDY", "MATT")) +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1, colour = "#FFFF33"),
        legend.title = element_blank(),
        legend.position = "bottom") +
  coord_flip()

# bar plot for joint TNC rating vs IMDb ----------------------------------------
compare.TNCIMDB <- data %>% 
  select(TNC, IMDB) %>% 
  mutate(diff = TNC - IMDB) %>% 
  mutate(diff.type = ifelse(diff >= -0.5 & diff <= 0.5, "Close",
                            ifelse(diff > 0.5, "Disagree +", "Disagree -"))) %>% 
  arrange(desc(diff)) %>% 
  group_by(diff.type) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(prop = round(count/sum(count) * 100, 0))

scored.TNCIMDB <- data %>% 
  select(TNC, IMDB) %>% 
  mutate(diff = TNC - IMDB) %>% 
  mutate(whos.higher = ifelse(diff > 0, "TNC",
                              ifelse(diff < 0, "IMDB",
                                     ifelse(diff == 0, "MATCH!", "???")))) %>% 
  arrange(desc(diff)) %>% 
  group_by(whos.higher) %>% 
  summarise(count = n()) %>% 
  na.omit() %>% 
  ungroup() %>% 
  mutate(prop = round(count/sum(count) * 100, 0))

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
  #  annotate(geom = "text", x=10, y=data$Episode_number[1], label=TNCIMDB_rating_caption, size=4, colour="#E7FFFF", family = "Antonio", hjust=0, vjust=1, lineheight = 1.3) +
  labs(
    subtitle = "JOINT TNC SCORE VS IMDb",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0:10)) +
  scale_fill_manual(values = c("#FFFF33", "#72E2E4"),
                    labels = c("IMDb", "TNC")) +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1 , colour = "#FFFF33"),
        legend.title = element_blank(),
        legend.position = "bottom") +
  coord_flip()

# patch these two plots together -----------------------------------------------

ep_ratings_plt <- (host_rating_plt | joint_vs_imdb_rating_plt) + plot_annotation(title = '', 
                                                                                 subtitle = "",
                                                                                 caption = "",theme=theme_trek_header())  


ggsave("star_trek_TNC/exports-snw-s1/snw-final_s1_scores.png",width = 48, height = 24, units = "cm", dpi = 100) 

# 2. plotting the MVC (Most Valuable Character/Crewmember) ---------------------
# bar plot of the DS9 MVC leaderboard ------------------------------------------
sub_title <- glue("MVC VOTE LEADERBOARD ({eps_watched}/10 EPISODES)")
current_ep <- data$Episode_name[eps_watched]
current_ep_num <- data$Episode_number[eps_watched]

total_votes <- sum(MVC_leaderboard$count)

MVClb_caption <- str_wrap(glue(""), 45)

# update MVC leaderboard dt with MVC division colours --------------------------
MVC_leaderboard <- MVC_leaderboard %>% 
  mutate(div_col = ifelse(Character %in% c("PIKE", "UNA", "KIRK"), "#B5A424", # Command
                          ifelse(Character %in% c("HEMMER", "LA'AN", "UHURA"), "#CD6363", # Ops / Security
                                 ifelse(Character %in% c("", "CHAPEL", "SPOCK", "M'BENGA"), '#3399FF', '#4C4D47')))) # Sciences, if not: Civilian/Other

(MVC_plt <- 
    MVC_leaderboard %>% 
    ggplot(aes(x=count, y=fct_reorder(Character, count))) +
    geom_bar(aes(fill = div_col), stat = "identity") +
    scale_fill_identity() +
    geom_image(aes(x=count, y=fct_reorder(Character, count), image = img), size = 0.145) +
    geom_point(aes(x=count+1.5, y=fct_reorder(Character, count)), size = 9, colour ="#E7FFFF") +
    geom_text(aes(x=count+1.5, y=fct_reorder(Character, count), label=count, family = "Antonio"), size = 6, colour ="#000000") +
    # MVC 1
    # geom_text(aes(x=2, y=9, label = toupper(glue("+ 1 VOTE ({current_ep})")), family = "Antonio"), color = "#FFFF9C", size = 4) + 
    # MVC 2
    # geom_text(aes(x=2, y=7, label = toupper(glue("+ 1 VOTE ({current_ep})")), family = "Antonio"), color = "#FFFF9C", size = 4) +
    annotate(geom = "text", x=5, y=5 + 0.2 ,label=MVClb_caption, size=5, colour="#E7FFFF", family = "Antonio", hjust=0, vjust=1, lineheight = 1.3) +
    labs(
      subtitle = "MVC LEADERBOARD",
      x = element_blank(),
      y = element_blank()) +
    scale_x_continuous(limits = c(0, 11), breaks = c(0:10)) +
    theme_trek() +
    theme(axis.text.y = element_text(family = "Antonio", face = "bold", size = 14, colour = "#646DCC")))

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
  pivot_longer(cols = 3:8, names_to = "Character", values_to = "count") %>% 
  group_by(Character) %>% 
  mutate(total = cumsum(count),
         Character = toupper(factor(Character)),
         Episode_number = as.integer(Episode_number)) %>% 
  mutate(Character = toupper(Character)) %>% 
  mutate(img = ifelse(Character == "CHAPEL", images[1], 
                      ifelse(Character == "HEMMER", images[2], 
                             ifelse(Character == "KIRK", images[3],
                                    ifelse(Character == "LA'AN", images[4],
                                           ifelse(Character == "M'BENGA", images[5], 
                                                  ifelse(Character == "ORTEGAS", images[6], 
                                                         ifelse(Character == "PIKE", images[7], 
                                                                ifelse(Character == "SPOCK", images[8], 
                                                                       ifelse(Character == "UHURA", images[9], 
                                                                              ifelse(Character == "UNA", images[10],
                                                                                     images[10])))))))))))




ranks <- MVC.tally %>% 
  group_by(Episode_number) %>% 
  mutate(rank = rank(total, ties.method = "first")) 

sel_chr <- c("PIKE", "UHURA", "HEMMER", "UNA", "LA'AN", "KIRK")

current_rank <- 
  ranks %>% 
  mutate(Character = as_factor(Character)) %>% 
  filter(Episode_number == current_ep_num) %>% 
  arrange(desc(rank)) 

current_rank <- current_rank$Character

pos <- ranks %>% 
  filter(Episode_number == current_ep_num, Character %in% sel_chr)

sub_title <- glue("MVC VOTE RANKING TREND ({eps_watched}/10 EPISODES)")

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
    geom_image(data=pos, aes(x=Episode_number + 0.5, y=rank, image = img), size = 0.1) +
    theme_trek() +
    labs(
      subtitle = sub_title,
      x = element_blank(),
      y = element_blank()) +
    theme(axis.text.y = element_blank(),
          legend.title = element_blank(),
          legend.key = element_rect(fill = "#000000", color = NA),
          legend.position = "right") +
    scale_x_continuous(limits = c(0, 11), breaks = c(1:10)) 
} 

# MVC_plt - highlight the top 3 MVCs for this season -----
MVC_ranks_plt <- hlt_rank_plt(c("PIKE", "UHURA", "HEMMER", "UNA", "LA'AN", "KIRK"))

# patch these plots together ---------------------------------------------------

MVC_plt <- (MVC_plt | MVC_ranks_plt) + plot_annotation(title = '', 
                                                       subtitle = "",
                                                       caption = "",theme=theme_trek_header())  

ggsave("star_trek_TNC/exports-snw-s1/tng_s1_MVC-wotitle.png",width = 48, height = 24, units = "cm",  dpi = 100) 

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
cluster_caption <- str_wrap(glue(""), 120)

cluster_plot <- 
  complete_k_data %>% 
  ggplot() +
  stat_density_2d(aes(x=TNC, y=IMDB), colour = "#46616E") +
  annotate(geom = "text", x=-2.2, y=3.2 ,label=cluster_caption, size=5, colour="#E7FFFF", family = "Antonio", hjust=0, vjust=1, lineheight = 1.3) +
  annotate("text", x=2, y=2, size = 8, colour = cluster_txtcol, label = "ACHINGLY\nBEAUTIFUL", fontface = 2, family = "Antonio") +
  annotate("text", x=1, y=1, size = 8, colour = cluster_txtcol, label = "PIKE'S PEAK", fontface = 2, family = "Antonio") +
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
  labs(title = "",
       subtitle = "EPISODE GUIDE: SEASON QUADRANT (K-MEANS CLUSTER MODEL)",
       caption = "",
       x = "", y = "") +
  theme_trek_clust() +
  theme(legend.title = element_blank(),
        legend.position = "none",
        legend.key = element_rect(fill = "#000000", color = NA)) 

cluster_plot

ggsave("star_trek_TNC/exports-snw-s1/snw-final_s1_cluster.png", width = 36, height = 24, units = "cm",  dpi = 100) 
ggsave("star_trek_TNC/exports-snw-s1/snw-final_s1_cluster-square.png", width = 36, height = 36, units = "cm",  dpi = 100) 

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
  geom_vline(aes(xintercept = summary_stats$mean[2], colour=Weighted, alpha = 0.5), size = 1) +
  scale_colour_gradient(low = "#CD6363", high = "#99FF66") + 
  geom_text(aes(x=Weighted + 0.5, y=fct_reorder(Episode_name, Episode_number), label = round(Weighted,1)), family="Antonio", colour = "#FFFF33", size = 8) +
  labs(
    subtitle = "EPISODE RATINGS - MATT, ANDY, JOINT TNC AND IMDB AVERAGED TOGETHER",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0:10))  +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1, family = "Antonio", face = "bold", size = 18, colour = "#646DCC"),
        axis.text.y = element_text(family = "Antonio", face = "bold", size = 12, colour = "#646DCC"),
        legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()

all_rating_plt

ggsave("star_trek_TNC/exports-snw-s1/final_s1_all_scores.png",width = 24, height = 24, units = "cm",  dpi = 100) 

# order the same plot by scores ------------------------------------------------ 
all_rating_plt_by_scores <- 
  data %>% 
  # pivot the data
  pivot_longer(cols = c("Andy_rating", "Matt_rating", "TNC", "IMDB"), names_to = "Scores", values_to = "Rating") %>% 
  select(Episode_number, Episode_name, Scores, Rating) %>% 
  mutate(Episode_name = toupper(Episode_name)) %>% 
  group_by(Episode_name, Episode_number) %>% 
  summarise(Weighted = mean(Rating)) %>% 
  # visualise
  ggplot(aes(fill = Weighted)) +
  geom_bar(aes(x=Weighted, y=fct_reorder(Episode_name, -Weighted)), stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "#CD6363", high = "#99FF66") + 
  geom_vline(aes(xintercept = summary_stats$mean[2], colour=Weighted, alpha = 0.5), size = 1) +
  scale_colour_gradient(low = "#CD6363", high = "#99FF66") + 
  geom_text(aes(x=Weighted + 0.5, y=fct_reorder(Episode_name, Episode_number), label = round(Weighted,1)), family="Antonio", colour = "#FFFF33", size = 8) +
  labs(
    subtitle = "EPISODE RATINGS - MATT, ANDY, JOINT TNC AND IMDB AVERAGED TOGETHER",
    x = element_blank(),
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(0:10))  +
  theme_trek() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust =1, family = "Antonio", face = "bold", size = 18, colour = "#646DCC"),
        axis.text.y = element_text(family = "Antonio", face = "bold", size = 12, colour = "#646DCC"),
        legend.title = element_blank(),
        legend.position = "none") +
  coord_flip()

all_rating_plt_by_scores

ggsave("star_trek_TNC/exports-snw-s1/final_s1_all_order_by_scores.png",width = 24, height = 24, units = "cm",  dpi = 100) 

# top 3 episodes  --------------------------------------------------------------
# create a vector of image 'headshots' of TNC/IMDb indicative images -----------
images_tnc <- c(
  "star_trek_TNC/images/tnc_headshots/secunda.jpg",
  "star_trek_TNC/images/tnc_headshots/mira.jpg",
  "star_trek_TNC/images/tnc_headshots/imdb.jpg",
  "star_trek_TNC/images/tnc_headshots/tnc.jpg")

# apply cropcircles to create circular images representing MVCs --
image_tnc_df <- tibble(y = 1:4, images = images_tnc) %>% 
  mutate(images_cropped = circle_crop(images, border_size = 10))

images_tnc <- image_tnc_df$images_cropped

# now create a datatable of top 3 episodes for each --
top3 <- function(season, who, who_str){
  data %>% 
    slice(0:eps_watched) %>% 
    filter(Season == season) %>% 
    arrange(desc({{who}})) %>% 
    head(n=3) %>% 
    select(Episode_name, {{who}}) %>% 
    mutate(rank = 1:3) %>% 
    pivot_longer(cols = who_str, names_to = "rating")
}

# generate the datatables of top 3 episodes for each -- 
top_TNC <- top3(1,TNC,"TNC")
top_IMDB <- top3(1,IMDB,"IMDB")
top_Andy <- top3(1,Andy_rating,"Andy_rating")
top_Matt <- top3(1,Matt_rating,"Matt_rating") 

# single datatable for visualisation --
top_all <- bind_rows(top_TNC, top_IMDB, top_Andy, top_Matt) %>% 
  mutate(rating = as.factor(rating))

top_all$rating <- recode_factor(top_all$rating, Andy_rating = "Andy", Matt_rating = "Matt", TNC = "TNC (Joint Score)")
top_all$rating <- factor(top_all$rating, c("Andy", "Matt", "TNC (Joint Score)", "IMDB"))

top_all

# include the TNC headshot images --
top_all <- 
  top_all %>% 
  mutate(rating = toupper(rating)) %>% 
  mutate(img = ifelse(rating == "ANDY", images_tnc[1], 
                      ifelse(rating == "MATT", images_tnc[2], 
                             ifelse(rating == "TNC (JOINT SCORE)", images_tnc[4],
                                    images_tnc[3])))) %>% 
  mutate(Episode_name = toupper(Episode_name))

# visualise top 3 episodes ---------
top_all <- top_all %>% 
  mutate(tnc_col = ifelse(rating == "ANDY", '#CD6363', 
                          ifelse(rating == "MATT", "#72E2E4", 
                                 ifelse(rating == "IMDB", '#e6d609', '#3399FF')))) # Sciences, if not: Civilian/Other

library(tidytext) # <-- for reorder_within function
# This allows the reordering across factors (rating) to work properly. It was not working with fct_reorder.
# https://juliasilge.github.io/tidytext/reference/reorder_within.html

top3_plt_facet <- 
  top_all %>% 
  ggplot(aes(x=value, y=reorder_within(Episode_name, value, rating))) +
  geom_bar(aes(fill = tnc_col), stat = "identity") +
  scale_fill_identity() +
  geom_text(aes(x=0.1, y=reorder_within(Episode_name, value, rating), label = (glue("{Episode_name}")), family = "Antonio"), color = "#000000", size = 5, hjust = 0) +
  geom_point(aes(x=value - 1, y=reorder_within(Episode_name, value, rating)), colour = "#000000", size = 11) +
  geom_text(aes(x=value - 1, y=reorder_within(Episode_name, value, rating), label = round(value,1)), family="Antonio", colour = "#FFFF33", size = 5) +
  labs(
    subtitle = "TOP 3 EPISODES",
    x = element_blank(), 
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme_trek() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_reordered() +
  facet_wrap(~factor(rating, levels = c("ANDY", "MATT", "TNC (JOINT SCORE)", "IMDB")), scales = "free", nrow = 1)

top3_plt_facet

ggsave("star_trek_TNC/exports-snw-s1/top_3_scores_facet.png",width = 24, height = 8, units = "cm",  dpi = 100) 

top_episode <- 
  top_all %>% 
  filter(rank == 1) %>% 
  ggplot(aes(x=value, y=fct_reorder(rating, value))) +
  geom_bar(aes(fill = tnc_col), stat = "identity") +
  scale_fill_identity() +
  geom_image(aes(x=1, y=fct_reorder(rating, value), image = img), size = 0.15) +
  geom_text(aes(x=2, y=fct_reorder(rating, value), label = (glue("{Episode_name}")), family = "Antonio"), color = "#000000", size = 5, hjust = 0) +
  # extra text annotation for Andy's joint favourite, "Duet" which he also scored 9.5
  geom_point(aes(x=value -1, y=fct_reorder(rating, value)), colour = "#000000", size = 11) +
  geom_text(aes(x=value -1, y=fct_reorder(rating, value), label = round(value,1)), family="Antonio", colour = "#FFFF33", size = 5) +
  labs(
    subtitle = "TOP EPISODES",
    x = element_blank(), 
    y = element_blank()) +
  scale_x_continuous(limits = c(0, 10), breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  theme_trek() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank()) 

top_episode

ggsave("star_trek_TNC/exports-snw-s1/top_episode.png",width = 12, height = 12, units = "cm",  dpi = 100) 

# patch EVERYTHING together ----------------------------------------------------
font_add("Star Trek TNG-Title", "/Users/tristanlouth-robins/Library/Fonts/Star Trek TNG-Title Regular.ttf")
showtext::showtext_auto()

summary <- str_wrap(glue("The hosts Season {data$Season[1]} scores matched or were very close on {compare.AndyMatt$count[2]} occassions, {compare.AndyMatt$prop[2]}% 
                                      of the time. Andy scored higher than Matt {scored.AndyMatt$count[1]} times ({scored.AndyMatt$prop[1]}%) compared to  
                                      {scored.AndyMatt$count[3]} instances when Matt scored higher than Andy. Their average scores were very close to each other 
                                      with: {round(summary_stats$mean[2],1)} (Andy) and {round(summary_stats$mean[4],1)} (Matt.) The TNC/IMDb Season {data$Season[1]} 
                                      scores matched or were very close on {compare.TNCIMDB$count[1]} occassions, {compare.TNCIMDB$prop[1]}% of the time and the IMDb scores 
                                      were significantly higher than the joint TNC score the majority of the time ({scored.TNCIMDB$count[1]}/10, {scored.TNCIMDB$prop[1]}%) compared 
                                      to only {scored.TNCIMDB$count[2]} instances when the joint TNC score was marginally higher than that of IMDb ('Strange New Worlds' and 'Spock Amok').
                                      The average scores for this season were: {round(summary_stats$mean[3],1)} (TNC) and {round(summary_stats$mean[1],1)} (IMDb)"), 200)


# set up a base layer --
title <- "STAR TREK: THE NEXT CONVERSATION - STRANGE NEW WORLDS: SEASON 1"
caption <- "BROUGHT TO YOU BY TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins"
plot.bg <- "#000000"

base <- ggplot() +
  labs(title = title,
       subtitle = summary,
       caption = caption) +
  theme_trek() +
  theme(plot.title = element_text(family = "Jefferies Extended", size = 36, colour = "#3399FF", margin=margin(0,0,10,0)),
        plot.subtitle = element_text(family = "Antonio", size = 24, colour = "#F7B05A", margin=margin(10,0,5,0)),
        plot.caption = element_text(family = "Antonio", size = 16, colour = "#99CCFF"),
        plot.margin = margin(1,1,1,1, "cm"),
        plot.background = element_rect(fill = "#000000", colour = "#000000"))

tncsnws1plt <- 
  base +
  inset_element(ep_ratings_plt, left = 0.01, right = 0.601, top = 0.99, bottom = 0.60) +
  inset_element(all_rating_plt, left = 0.605, right = 0.995, top = 0.99, bottom = 0.60) +
  inset_element(MVC_plt, left = 0.01, right = 0.601, top = 0.59, bottom = 0.20) +
  inset_element(cluster_plot, left = 0.605, right = 0.995, top = 0.59, bottom = 0.01) +
  inset_element(top3_plt_facet, left = 0.01, right = 0.48, top = 0.19, bottom = 0.01) +
  inset_element(top_episode, left = 0.481, right = 0.6, top = 0.19, bottom = 0.01)

tncsnws1plt

ggsave("star_trek_TNC/exports-snw-s1/tnc_snw_s1.png", plot = tncsnws1plt, width = 80, height = 60, units = "cm", dpi = 100) 
