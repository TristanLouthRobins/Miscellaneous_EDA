library(tidyverse)
library(ggplot2)
library(ggbump)
library(lubridate)
library(chron)

# getwd()

# Import the current dataset ----------------------------------------------------
path <- "data/why_blue.csv"

# Tidy up the variables (esp. the date format) ----------------------------------
data <- read_csv(path, col_names = FALSE) %>% 
  rename("artist" = X1, "album" = X2, "track" = X3, "date" = X4) %>% 
  separate(date, into = c("day", "month", "year", "time"), sep = " ") %>% 
  mutate(month = ifelse(month == "Jan", 1,
                        ifelse(month == "Feb", 2,
                               ifelse(month == "Mar", 3, 
                                      ifelse(month == "Apr", 4, 
                                             ifelse(month == "May", 5, 
                                                    ifelse(month == "Jun", 6,
                                                           ifelse(month == "Jul", 7, 
                                                                  ifelse(month == "Aug", 8, 
                                                                         ifelse(month == "Sep", 9, 
                                                                                ifelse(month == "Oct", 10, 
                                                                                       ifelse(month == "Nov", 11, 
                                                                                              ifelse(month == "Dec", 12, "x"))))))))))))) %>% 
  mutate(date = str_c(day, month, year, sep = "/")) %>% 
  # <-- this is where we convert into SA timezone.
  mutate(time2 = time) %>% 
  separate(time2, into = c("hour", "min")) %>% 
  mutate(hour = as.numeric(hour)) %>% 
  mutate(min = as.numeric(min)) %>% 
  mutate(time = str_c(time, ":00")) %>% 
  mutate(date = as.Date(date, format = "%d/%m/%Y")) %>% 
  mutate(time = chron(times = time)) %>% 
  mutate(dt = str_c(date, time)) %>% 
  mutate(dt = as.POSIXct(dt)) %>% 
  select(-day, -month, -year, -hour, -min, -date, -time)

# Compensate for timezone issue and convert date/time to AU CST ---

secs <- 0 # <-- 0 seconds
mins <- 30 * 60 # <-- 30 mins
hrs <- 9 * 60 * 60 # <-- 9 hours

scrobbles <- data %>% 
  mutate(dt = dt + (secs + mins + hrs)) %>% 
  mutate(dt2 = dt) %>% 
  mutate(dt3 = dt) %>% 
  separate(dt2, into = c("date", "time"), sep = " ") %>% 
  separate(dt3, into = c("year", "month", "day"), sep = "-") %>%  
  separate(day, into = c("day", "nope", sep = " ")) %>% 
  mutate(month = as.numeric(month)) %>% 
  select(-nope) %>% 
  mutate(time = as.POSIXct(dt, format = "%H:%M:%S")) 

#----------------------
# MONTHLY REPORT SCRIPT
#----------------------
# Notes: would be cool to loop through the months and return separate lists/df of each month.

# Function: 

# Function accepts a vector and artist/album

count_per_month <- function(range, var) {
  cnt <- scrobbles %>% 
    filter(month %in% range, year == 2022) %>% 
    group_by_(var) %>% 
    select(var, month, year) %>% 
    unique() %>% group_by(month) %>% 
    summarise(count = n())
  
 for(i in 1:nrow(cnt)) {
   return(print(paste("In month", range, cnt$count, "unique", var, "were scrobbled.")))
  }  
}
  
count_per_month(7:12, "track")
count_per_month(7:12, "artist")

monthly_album_report <- function(m, size){
  scrobbles %>% filter(month == m) %>% 
    group_by(album) %>% summarise(cnt = n()) %>% 
    arrange(desc(cnt)) %>% 
    head(n=size) %>% 
    ungroup() %>% 
    mutate(prop = round(cnt/sum(cnt), 2),
           rank = 1:size,
           month = m)
}

# example - monthly_album_report(9, 112)

# Function: groups by TRACKS and summarises by scrobble count

count_tracks <- function(m) {
  cnt <- scrobbles %>% 
    filter(month == m) %>% 
    group_by(track) %>% 
    select(track) %>% 
    unique() %>% 
    nrow()
  
  return(print(paste("In month", m, cnt, "unique tracks were scrobbled.")))
}

count_tracks(12)

monthly_track_report <- function(m, size){
  scrobbles %>% filter(month == m) %>% 
    group_by(track) %>% summarise(cnt = n()) %>% 
    arrange(desc(cnt)) %>% 
    head(n=size) %>% 
    ungroup() %>% 
    mutate(prop = round(cnt/sum(cnt), 2),
           rank = 1:size,
           month = m)
}

# example - monthly_track_report(10, 165)

# Call monthly_artist_report to generate summaries for each month --
jul <- monthly_artist_report(7, 100)
aug <- monthly_artist_report(8, 100)
sep <- monthly_artist_report(9, 100)
oct <- monthly_artist_report(10, 100)
nov <- monthly_artist_report(11, 100)
dec <- monthly_artist_report(12, 100)

month_compiled <- list(jul, aug, sep, oct, nov, dec)
all <- Reduce(function(x, y) merge(x, y, all=TRUE), month_compiled)
all

####################################################

# Function and process: explore month of interest
# first filter to month of interest
monthly_summary <- function(m){
  tt_artist <- all %>% 
    filter(month == m, rank <= 10) %>% 
    arrange(rank) %>% 
    rename(scrobbles = cnt) %>% 
    select(-month)

# now from the larger dataset find the ranks of the top ten 
# artists from the previous month.

# we create a vector of the artists:
  artists <- tt_artist$artist

# now find the ranks of the artists from the previous month:
  prev <- all %>% 
    filter(artist %in% artists, month == (m-1)) %>% 
    select(artist, rank) %>% 
    rename(last_month = rank)

# join the prev df with the 'tt' df.
  tt_artist <- full_join(tt_artist, prev, by = "artist") %>% 
    mutate(trend = last_month - rank)
  
  return(tt_artist)
}

current_tt <- monthly_summary(12)




# VIZ: Explore the trend of the current top ten artists

h <- all %>% filter(artist %in% current_tt$artist) 

ggplot(h) + 
  geom_point(aes(x = month, y = rank, colour = as_factor(artist)), size = 5, alpha = 0.6) +
  geom_line(aes(x = month, y = rank, group = artist, colour = artist), size = 1, alpha = 0.5) +
  geom_label(aes(x = month, y = rank, label = rank, colour = artist), alpha = 0.6) +
  theme_dark() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(trans = "reverse", breaks = unique(h$rank)) 

# Bump plot of ranks over time

ggplot(h, aes(x = month, y = rank, colour = as_factor(artist))) +
  geom_point(size = 10, alpha = 0.6) +
  geom_bump(size = 2, alpha = 0.4) +
  theme_dark() +
  scale_color_brewer(palette = "RdBu") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank()) +
  scale_y_continuous(trans = "reverse", breaks = unique(h$rank)) 
