# Data EDA for Star Trek: The Next Conversation podcast ------------------------
# Hosted by Matt Mira and Andy Secunda -----------------------------------------
# This EDA script examines all season 1 eps reviewed to date: 8/6/2023 ---------
# IMDb ratings are approximately as at: 30/4/2023 ------------------------------
# Latest update: v1.1 (09 June 2023) -------------------------------------------

library(tidyverse)
library(ggplot2)
library(cropcircles) # Credit to: https://github.com/doehm for his excellent pkg!
library(ggimage)
library(ggrepel)
library(ggbump)
library(ggtext)
library(glue)
library(showtext)
library(patchwork) 

font_add("Antonio", "/Users/tristanlouth-robins/Library/Fonts/Antonio-VariableFont_wght.ttf") # LCARS font
font_add("Star Trek TNG-Title", "/Users/tristanlouth-robins/Library/Fonts/Star Trek TNG-Title Regular.ttf")

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

# ----------------------------------------------------------------------
# import dataset and tidy ----------------------------------------------
# ----------------------------------------------------------------------
data <- read_csv("star_trek_TNC/data/tnc.csv") %>% 
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

# introduce new variable whether old or new trek -------------------------
data <- data %>% 
  mutate(trek_era = ifelse(Series %in% c("PIC", "SNW", "DIS"), "new_trek", "old_trek"))

# avg scores for series covered by TNC -----------------------------------
avg_all_TNC <- function(season) {
  data %>% 
   filter(Season == season) %>%   
   group_by(Series, trek_era) %>% 
   summarise(avg_TNC = mean(TNC)) %>% 
   arrange(desc(avg_TNC))
  }

all_scores <- avg_all_TNC(1)

all_scores$Series <- factor(all_scores$Series, levels = c("TNG", "DS9", "VOY", "ENT", "DIS", "PIC", "SNW"))

all_scores_plt <- 
all_scores %>% 
  mutate(era_col = ifelse(trek_era=="new_trek", "#FFCC33", "#3399FF")) %>% 
  ggplot() + 
  geom_col(aes(x=fct_reorder(Series, avg_TNC), y=avg_TNC, fill=era_col)) +
  scale_fill_identity() +
  coord_flip() +
  geom_point(aes(x=fct_reorder(Series, avg_TNC), y=avg_TNC-0.6), size=10, colour="#000000") +
  geom_text(aes(x=fct_reorder(Series, avg_TNC), y=avg_TNC-0.6, label=round(avg_TNC, 1)), family="Antonio", colour="#FFFFFF") +
  labs(subtitle = "MEAN JOINT TNC SCORES FOR ALL SERIES\n(SEASON 1)", 
       x="", y="") +
  scale_y_continuous(limits = c(0, 10), breaks = c(0:10)) +
  theme_trek() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust =1 , colour = "#FFFF33", size=12),
        axis.text.x = element_blank()) 

ggsave("star_trek_TNC/exports-wide_EDA/s1EDA-mean_joint_tnc_scores_for_all.png",width = 12, height = 12, units = "cm",  dpi = 100) 

# avg scores for series (IMDB reviews) -----------------------------------
avg_all_IMDB <- function(season) {
  data %>% 
    filter(Season == season) %>%   
    group_by(Series, trek_era) %>% 
    summarise(avg_IMDB = mean(IMDB)) %>% 
    arrange(desc(avg_IMDB))
}

all_scores <- avg_all_IMDB(1)

all_scores$Series <- factor(all_scores$Series, levels = c("TNG", "DS9", "VOY", "ENT", "DIS", "PIC", "SNW"))

all_scores_imdb_plt <- 
  all_scores %>% 
  mutate(era_col = ifelse(trek_era=="new_trek", "#FFCC33", "#3399FF")) %>% 
  ggplot() + 
  geom_col(aes(x=fct_reorder(Series, avg_IMDB), y=avg_IMDB, fill=era_col)) +
  scale_fill_identity() +
  coord_flip() +
  geom_point(aes(x=fct_reorder(Series, avg_IMDB), y=avg_IMDB-0.6), size=10, colour="#000000") +
  geom_text(aes(x=fct_reorder(Series, avg_IMDB), y=avg_IMDB-0.6, label=round(avg_IMDB, 1)), family="Antonio", colour="#FFFFFF") +
  labs(subtitle = "MEAN IMDB SCORES FOR ALL SERIES\n(SEASON 1)", 
       x="", y="") +
  scale_y_continuous(limits = c(0, 10), breaks = c(0:10)) +
  theme_trek() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust =1 , colour = "#FFFF33", size=12),
        axis.text.x = element_blank()) 

ggsave("star_trek_TNC/exports-wide_EDA/s1EDA-mean_imdb_scores_for_all.png",width = 12, height = 12, units = "cm",  dpi = 100) 


# who scores higher ---------------------------------------------
who_scores_higher <- function(season, era) { 
    data %>%   
    filter(Season == season,
           era == trek_era) %>% 
    select(Andy_rating, Matt_rating, trek_era) %>% 
    mutate(diff = Andy_rating - Matt_rating) %>% 
    mutate(whos.higher = ifelse(diff > 0, "Andy",
                                ifelse(diff < 0, "Matt",
                                       ifelse(diff == 0, "Agree", "?")))) %>% 
    arrange(desc(diff)) %>% 
    group_by(whos.higher, trek_era) %>% 
    mutate(whos.higher = factor(whos.higher, levels = c("Matt", "Agree", "Andy"))) %>% 
    summarise(count = n()) %>% 
    na.omit() %>% 
    ungroup() %>% 
    mutate(prop = round(count/sum(count), 2),
           sum = 1 - cumsum(prop),
           ypos = sum + (prop/2))
}  

(old <- who_scores_higher(1, "old_trek"))
(new <- who_scores_higher(1, "new_trek"))
(era_prop <- bind_rows(old, new))
# manually adjust prop for 'Agree' (new_trek) to 0.32 so that it sums to 1.00
era_prop[5,4] <- 0.32

# stacked bar plot for who scores higher (old and new trek) ---------------------
era_prop_plt <- 
era_prop %>%  
  ggplot(aes(fill=whos.higher, y=prop, x=trek_era)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#72E2E4", "#3399FF", "#e6d609"),
                    labels = c("Matt", "Agree", "Andy")) +
  geom_text(aes(x=trek_era, y=ypos, label=paste(toupper(whos.higher), ": ", prop * 100, "%", sep="")), family="Antonio", size=5) +
  labs(subtitle="WHO RATES HIGHER: BY NEW/OLD TREK",
       x="", y="") +
  scale_x_discrete(labels=c("NEW TREK", "OLD TREK")) +
  theme_trek() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5 , colour = "#FFFF33", size=14),
        legend.position = "none") 
  
ggsave("star_trek_TNC/exports-wide_EDA/s1EDA-who_rates_higher.png",width = 12, height = 12, units = "cm",  dpi = 100) 

# who scores each series higher -------------------------------------------------
who_scores_series_higher <- function(season) { 
  data %>%   
    filter(Season == 1) %>% 
    select(Andy_rating, Matt_rating, Series, trek_era) %>% 
    mutate(diff = Andy_rating - Matt_rating) %>% 
    mutate(whos.higher = ifelse(diff > 0, "Andy",
                                ifelse(diff < 0, "Matt",
                                       ifelse(diff == 0, "Agree", "?")))) %>% 
    arrange(desc(diff)) %>% 
    group_by(Series, whos.higher) %>% 
    mutate(whos.higher = factor(whos.higher, levels = c("Matt", "Agree", "Andy"))) %>% 
    summarise(count = n()) %>% 
    na.omit() %>% 
    mutate(prop = round(count/sum(count), 2),
           sum = 1 - cumsum(prop),
           ypos = sum + (prop/2))
}  

all_prop <- who_scores_series_higher(1)
# manually adjust prop for 'Agree' (DIS) to 0.34 so that it sums to 1.00
all_prop[2,4] <- 0.34

all_prop$Series <- factor(all_prop$Series, levels = c("TNG", "DS9", "VOY", "ENT", "DIS", "PIC", "SNW"))

all_prop_plt <- 
all_prop %>%  
  ggplot(aes(fill=whos.higher, y=prop, x=Series)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#72E2E4", "#3399FF", "#e6d609"),
                    labels = c("MATT", "AGREE", "ANDY")) +
  geom_text(aes(x=Series, y=ypos, label=paste(prop * 100, "%", sep="")), family="Antonio", size=3) +
  labs(subtitle="WHO RATES HIGHER: BY SERIES",
       x="", y="") +
  theme_trek() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(angle=0, vjust=0.5, hjust=0.5 , colour = "#FFFF33", size=14),
        legend.title = element_blank(),
        legend.position = "bottom") 
  
ggsave("star_trek_TNC/exports-wide_EDA/s1EDA-who_rates_higher_all_series.png",width = 12, height = 12, units = "cm",  dpi = 100) 

# looking ahead: how will Season 2 of SNW fare? --------------------------------
s1newtrek_TNC_scores <- avg_all_TNC(1) %>% filter(Series %in% c("DIS", "PIC")) %>% mutate(season = 1)
s2newtrek_TNC_scores <- avg_all_TNC(2) %>% filter(Series %in% c("DIS", "PIC")) %>% mutate(season = 2)
newtrek_TNC_compare <- bind_rows(s1newtrek_TNC_scores, s2newtrek_TNC_scores)

s1newtrek_IMDB_scores <- avg_all_IMDB(1) %>% filter(Series %in% c("DIS", "PIC")) %>% mutate(season = 1)
s2newtrek_IMDB_scores <- avg_all_IMDB(2) %>% filter(Series %in% c("DIS", "PIC")) %>% mutate(season = 2)
newtrek_IMDB_compare <- bind_rows(s1newtrek_IMDB_scores, s2newtrek_IMDB_scores) 

new_trek_TNCIMDB <- bind_rows(newtrek_TNC_compare, newtrek_IMDB_compare) %>% 
  pivot_longer(cols = starts_with("avg"), names_to = "rating", values_to = "avg_score") %>% 
  na.omit()

new_trek_TNCIMDB$rating[new_trek_TNCIMDB$rating == "avg_TNC"] = "JOINT TNC"
new_trek_TNCIMDB$rating[new_trek_TNCIMDB$rating == "avg_IMDB"] = "IMDb"

picard_plt <- 
  new_trek_TNCIMDB %>% 
  filter(Series == "PIC") %>% 
  mutate(rev_col = ifelse(rating=="JOINT TNC", "#3399FF", "#e6d609")) %>% 
  ggplot() + 
  geom_bar(aes(x=as.factor(season), y=avg_score, fill=rev_col), position="dodge", stat="identity") +
  scale_fill_identity() +
  geom_point(aes(x=season, y=avg_score-1), size=10, colour="#000000") +
  geom_text(aes(x=season, y=avg_score-1, label=round(avg_score, 1)), family="Antonio", colour="#FFFFFF") +
  labs(subtitle = "BEWARE THE NEW TREK SOPHOMORE SLUMP: PICARD SEASON 1 TO 2", 
       x="", y="") +
  scale_y_continuous(limits = c(0, 10), breaks = c(0:10)) +
  facet_wrap(~rating) +
  theme_trek() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust =1 , colour = "#FFFF33", size=12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust =1 , colour = "#FFFF33", size=16),
        strip.text.x = element_text(size = 12)) 

picard_plt

ggsave("star_trek_TNC/exports-wide_EDA/picard_slump.png",width = 12, height = 12, units = "cm",  dpi = 100) 

disco_plt <- 
  new_trek_TNCIMDB %>% 
  filter(Series == "DIS") %>% 
  mutate(rev_col = ifelse(rating=="JOINT TNC", "#3399FF", "#e6d609")) %>% 
  ggplot() + 
  geom_bar(aes(x=as.factor(season), y=avg_score, fill=rev_col), position="dodge", stat="identity") +
  scale_fill_identity() +
  geom_point(aes(x=season, y=avg_score-1), size=10, colour="#000000") +
  geom_text(aes(x=season, y=avg_score-1, label=round(avg_score, 1)), family="Antonio", colour="#FFFFFF") +
  labs(subtitle = "BEWARE THE NEW TREK SOPHOMORE SLUMP: DISCO SEASON 1 TO 2", 
       x="", y="") +
  scale_y_continuous(limits = c(0, 10), breaks = c(0:10)) +
  facet_wrap(~rating) +
  theme_trek() +
  theme(axis.text.y = element_text(angle = 0, vjust = 0.5, hjust =1 , colour = "#FFFF33", size=12),
        axis.text.x = element_text(angle = 0, vjust = 0.5, hjust =1 , colour = "#FFFF33", size=16),
        strip.text.x = element_text(size = 12)) 

disco_plt

ggsave("star_trek_TNC/exports-wide_EDA/disco_slump.png",width = 12, height = 12, units = "cm",  dpi = 100)

# patch it all together --------------------------------------------------------
showtext::showtext_auto()
# set up a base layer --
title <- "STAR TREK: THE NEXT CONVERSATION - SEASON 1 ANALYSIS OF ALL SERIES REVIEWED"
s1_eda <- str_wrap(glue("This is an analysis of episode ratings for season {data$Season[1]} all of Star Trek series 
                        covered by TNC (as of June 2023.) Perhaps what is most surprising is that all three of the 
                        'new' Trek series (Discovery, Picard, Strange New Worlds) have the highest average joint TNC 
                        ratings, whereas 'old' Trek series are lower on average, with Voyager rating the highest of old  
                        Trek. Additionally, when compared to the average episode ratings for IMDb, the result are 
                        very similar, with new Trek also ranking as the three highest rated series. Note that in this case, Enterprise rates the highest of old Trek.
                        When it comes to the host's individual scores, typically Andy scores higher than Matt, but their
                        scores match around 30% of the time, most often with Voyager, Enterprise and Discovery.                                             
                        Lastly, based on the respective sophomore slumps of new Trek (largely evidenced on TNC), it will be interesting 
                        to see how Season 2 of Strange New Worlds fares."), 180)

caption <- "DATA: TRISTAN LOUTH-ROBINS & JEFF MULLINS / EDA & CODE: TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins"
plot.bg <- "#000000"

base <- ggplot() +
  labs(title = title,
       subtitle = s1_eda,
       caption = caption) +
  theme_trek() +
  theme(plot.title = element_text(family = "Star Trek TNG-Title", size = 36, colour = "#3399FF", margin=margin(0,0,10,0)),
        plot.subtitle = element_text(family = "Antonio", size = 24, colour = "#FFFF33", margin=margin(10,0,5,0)),
        plot.caption = element_text(family = "Antonio", size = 16, colour = "#99CCFF"),
        plot.margin = margin(1,1,1,1, "cm"),
        plot.background = element_rect(fill = "#000000", colour = "#000000"))

tncs1EDA <- 
  base +
  inset_element(all_scores_plt, left = 0, right = 0.30, top = 0.99, bottom = 0.50) +
  inset_element(era_prop_plt, left = 0.31, right = 0.61, top = 0.99, bottom = 0.50) +
  inset_element(all_prop_plt, left = 0.62, right = 0.92, top = 0.99, bottom = 0.50) +
  inset_element(all_scores_imdb_plt, left = 0, right = 0.30, top = 0.49, bottom = 0) +
  inset_element(picard_plt, left = 0.31, right = 0.61, top = 0.49, bottom = 0) +
  inset_element(disco_plt, left = 0.62, right = 0.92, top = 0.49, bottom = 0) 

tncs1EDA 

ggsave("star_trek_TNC/exports-wide_EDA/s1EDA-showcase.png",width = 60, height = 40, units = "cm",  dpi = 100) 

