# Data EDA for Star Trek: The Next Conversation podcast ------------------------
# Hosted by Matt Mira and Andy Secunda -----------------------------------------
# This EDA script is for their episodes reviewing Star Trek: TNG ---------------
# Podcast episode time line ----------------------------------------------------
# Latest update: v1.0 (14 July 2023) -------------------------------------------
library(tidyverse)
library(showtext)
library(patchwork)

# import dataset ---------------------------------------------------------------
data <- read_csv("data/tnc.csv")  %>% 
  select(Series, pod_date, Season) %>% 
  mutate(pod_date = as.Date(pod_date, format = "%d/%m/%Y"),
         ep = ifelse(Series == "TNG", 0.5, 
                     ifelse(Series == "DS9", 1,
                            ifelse(Series == "VOY", 1.5,
                                   ifelse(Series == "ENT", 2,
                                          ifelse(Series == "DIS", 2.5,
                                                 ifelse(Series == "SNW", 3, 3.5))))))) %>% 
  na.omit()

theme_trek <- function(){
  theme(
    text = element_text(family = "Antonio", colour = "#FFFFFF"),
    plot.margin = margin(5,2,5,2, "mm"),
    plot.background = element_rect(fill = "#000000", colour = "#99CCFF"),
    plot.title = element_text(family = "Star Trek TNG-Title", size = 12, colour = "#3399FF", vjust = 0),
    plot.subtitle = element_text(family = "Antonio", face = "bold", size = 14, colour = "#99CCFF"),
    axis.title.x = element_text(family = "Antonio", face = "bold", size = 12),
    axis.title.y = element_text(family = "Antonio", face = "bold", size = 12),
    axis.text.x = element_text(family = "Antonio", face = "bold", size = 12, colour = "#646DCC"),
    axis.text.y = element_text(family = "Antonio", face = "bold", size = 12, colour = "#646DCC"),
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

font_add("Star Trek TNG-Title", "/Users/tristanlouth-robins/Library/Fonts/Star Trek TNG-Title Regular.ttf")
font_add("Antonio", "/Users/tristanlouth-robins/Library/Fonts/Antonio-VariableFont_wght.ttf")
showtext::showtext_auto()

library(ggplot2)

timeline <- 
data %>% 
  mutate(shape_type = ifelse(Season == 1, 1,
                             ifelse(Season == 2, 2,
                                    ifelse(Season == 3, 8,
                                           ifelse(Season == 4, 3,
                                                  ifelse(Season == 5, 4,
                                                        ifelse(Season == 6, 9,
                                                               ifelse(Season == 7, 6, 14)))))))) %>% 
  ggplot() +
  geom_vline(xintercept = as.Date("2020-03-11"), colour = "#FFCC33", size = 0.5, alpha = 0.7) +
  annotate(geom = "text", x=as.Date("2020-03-11"), y=4, label="COVID-19 Pandemic declared", size=4, colour="#E7FFFF", family = "Antonio", hjust=1.05, vjust=1, lineheight = 1.3) +
  geom_jitter(aes(x = pod_date, y = ep, colour = Series), alpha = 0.7, width = 0.025, height=0.1) +
  geom_vline(xintercept = as.Date("2017-01-01"), colour = "#8f5dde", size = 0.5, alpha = 0.2) +
  geom_vline(xintercept = as.Date("2018-01-01"), colour = "#8f5dde", size = 0.5, alpha = 0.2) +
  geom_vline(xintercept = as.Date("2019-01-01"), colour = "#8f5dde", size = 0.5, alpha = 0.2) +
  geom_vline(xintercept = as.Date("2020-01-01"), colour = "#8f5dde", size = 0.5, alpha = 0.2) +
  geom_vline(xintercept = as.Date("2021-01-01"), colour = "#8f5dde", size = 0.5, alpha = 0.2) +
  geom_vline(xintercept = as.Date("2022-01-01"), colour = "#8f5dde", size = 0.5, alpha = 0.2) +
  geom_vline(xintercept = as.Date("2023-01-01"), colour = "#8f5dde", size = 0.5, alpha = 0.2) +
  labs(subtitle="PODCAST SERIES COVERAGE TIMELINE", x="", y="") +
  scale_x_date(date_breaks ="1 year") +
  theme_trek() +
  theme(axis.text.y = element_blank())

timeline

pod_count <- data %>% 
  mutate(date = pod_date) %>% 
  separate(pod_date, into = c("year", "month", "day", sep = "-")) %>% 
  select(- `-`) %>% 
  group_by(year, month) %>% 
  summarise(count = n()) %>% 
  mutate(day = 1) %>% 
  mutate(date = as.Date(paste(year, month, day, sep="-"))) %>% 
  select(-year, -month, -day)

pod_count_covid <- 
  pod_count %>% 
  mutate(pandemic_times = ifelse(date < "2020-03-11", "before", "after")) 

pod_count_covid %>% 
  group_by(pandemic_times) %>% 
  summarise(total = sum(count), months = n(), avg = total/months)

# visualise
pod_freq <- 
ggplot(pod_count) +
  geom_point(aes(x=date, y=count), colour = "#FFF000") +
  geom_smooth(se = F, aes(x=date, y=count)) +
  geom_vline(xintercept = as.Date("2020-03-11"), colour = "#FFCC33", size = 0.5, alpha = 0.7) +
  annotate(geom = "text", x=as.Date("2020-03-11"), y=12, label="COVID-19 Pandemic declared", size=4, colour="#E7FFFF", family = "Antonio", hjust=1.05, vjust=1, lineheight = 1.3) +
  ylim(0, 13) +
  scale_x_date(date_breaks ="1 year") +
  scale_y_continuous(breaks = c(0:13)) +
  labs(subtitle="PODCAST FREQUENCY (BY MONTH): FREE AND PATREON", x="", y="") +
  theme_trek() 

pod_freq

caption <- "BROUGHT TO YOU BY TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins"

base.tng <- ggplot() +
  labs(title = "STAR TREK: THE NEXT CONVERSATION",
       subtitle = "",
       caption = caption) +
  theme_trek() +
  theme(plot.title = element_text(family = "Star Trek TNG-Title", size = 30, colour = "#3399FF", margin=margin(0,0,10,0)),
        plot.subtitle = element_text(family = "Antonio", size = 24, colour = "#F7B05A", margin=margin(0,0,10,0)),
        plot.caption = element_text(family = "Antonio", size = 10, colour = "#99CCFF"),
        plot.margin = margin(1,1,1,1, "cm"),
        plot.background = element_rect(fill = plot.bg, colour = plot.bg))

all <- 
base.tng +
  inset_element(timeline, left = 0.01, right = 0.49, top = 0.99, bottom = 0.0) +
  inset_element(pod_freq, left = 0.51, right = 0.999, top = 0.99, bottom = 0.0) 

ggsave("timelines.png", plot = all, width = 40, height = 20, units = "cm", dpi = 100) 

# Non-Patreon frequency of pods -----------------------------------------------

data.free <- read_csv("data/tnc.csv")  %>% 
  select(Series, tier, pod_date, Season) %>% 
  mutate(pod_date = as.Date(pod_date, format = "%d/%m/%Y")) %>%
  filter(tier == "free") %>% 
  na.omit()

data.free %>%  group_by(Series) %>% summarise(count = n())

pod_count.free <- data.free %>% 
  filter(tier == "free") %>% 
  mutate(date = pod_date) %>% 
  separate(pod_date, into = c("year", "month", "day", sep = "-")) %>% 
  select(- `-`) %>% 
  group_by(year, month) %>% 
  summarise(count = n()) %>% 
  mutate(day = 1) %>% 
  mutate(date = as.Date(paste(year, month, day, sep="-"))) %>% 
  select(-year, -month, -day)

# visualise
pod_freq.free <- 
  ggplot(pod_count.free) +
  geom_point(aes(x=date, y=count), colour = "#FFF000") +
  geom_smooth(se = F, aes(x=date, y=count)) +
  geom_vline(xintercept = as.Date("2020-03-11"), colour = "#FFCC33", size = 0.5, alpha = 0.7) +
  annotate(geom = "text", x=as.Date("2020-03-11"), y=12, label="COVID-19 Pandemic declared", size=4, colour="#E7FFFF", family = "Antonio", hjust=1.05, vjust=1, lineheight = 1.3) +
  ylim(0, 13) +
  scale_x_date(date_breaks ="1 year") +
  scale_y_continuous(breaks = c(0:13)) +
  labs(subtitle="PODCAST FREQUENCY (BY MONTH): NON-PATREON", x="", y="") +
  theme_trek() 

pod_freq
pod_freq.free

paid_vs_free <- 
  base.tng +
  inset_element(pod_freq, left = 0.01, right = 0.49, top = 0.99, bottom = 0.0) +
  inset_element(pod_freq.free, left = 0.51, right = 0.999, top = 0.99, bottom = 0.0) 

ggsave("paid_vs_free.png", plot = paid_vs_free, width = 40, height = 20, units = "cm", dpi = 100) 

pod_count.free.summary <- data.free %>% 
  mutate(date = pod_date) %>% 
  separate(pod_date, into = c("year", "month", "day", sep = "-")) %>% 
  filter(year != "2023") %>% 
  select(- `-`) %>% 
  group_by(year) %>% 
  summarise(count = n(), avg = count/12) 

pod_count.free.summary

pod_count.free.summary.2023 <- data.free %>% 
  mutate(date = pod_date) %>% 
  separate(pod_date, into = c("year", "month", "day", sep = "-")) %>% 
  filter(year == "2023") %>% 
  select(- `-`) %>% 
  group_by(year) %>% 
  summarise(count = n(), avg = count/7) 

pods2023 <- pod_count.free.summary.2023 # as at July 2023

summary <- full_join(pod_count.free.summary, pods2023)
