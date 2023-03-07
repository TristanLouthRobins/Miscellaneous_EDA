library(ggbump)

tally <- 
  data %>% 
  slice(0:eps_watched) %>% 
  pivot_longer(cols = c("Andy_MVC", "Matt_MVC"), names_to = "Host") %>% 
  rename("Character" = "value") %>% 
  select(Episode_name, Episode_number, Host, Character) %>% 
  group_by(Episode_name, Episode_number, Character) %>% 
  summarise(count = n()) %>% 
  arrange(Episode_number) 

tally2 <- 
  tally %>% 
  pivot_wider(names_from = Character, values_from = count, values_fill = 0) 

tally3 <- 
  tally2 %>% 
  pivot_longer(cols = 3:11, names_to = "Character", values_to = "count") %>% 
  group_by(Character) %>% 
  mutate(total = cumsum(count),
         Character = toupper(factor(Character)),
         Episode_number = as.integer(Episode_number))

tally4 <- 
  tally3 %>% 
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


ranks <- 
tally4 %>% 
  group_by(Episode_number) %>% 
  mutate(rank = rank(total, ties.method = "first")) %>% 
  select(-count, -total)


sel_chr <- c("BASHIR", "DAX", "O'BRIEN", "KIRA", "SISKO",
             "JAKE", "NOG", "ODO", "OTHER")

current_rank <- 
ranks %>% 
  mutate(Character = as_factor(Character)) %>% 
  filter(Episode_number == current_ep_num) %>% 
  arrange(desc(rank)) 

current_rank <- current_rank$Character

pos <- ranks %>% 
  filter(Episode_number == current_ep_num, Character %in% sel_chr)

lcars_pal1 <- c("#FF9C00", "#FFFF33", "#CC99CC", "#DDFFFF", "#3399FF", "#99FF66", "#FFCC33", "#31C924", "#4D6184")

sub_title <- glue("SEASON ONE: MVC VOTE RANKING TREND ({eps_watched}/19 EPISODES)")


# Highlight particular rank
hlt_rank_plt <- function(selection){
  deselect_ranks <- 
    ranks %>% 
    filter(Character %in% sel_chr) %>% 
    filter(Character != selection)
  
  select_ranks <- 
    ranks %>% 
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

MVC_plt
MVC_ranks_plt <- hlt_rank_plt("O'BRIEN")

MVC_votes_rank  <- MVC_plt | MVC_ranks_plt

MVC_votes_rank + plot_annotation(title = 'DEEP SPACE NINE', 
                            subtitle = "STAR TREK: THE NEXT CONVERSATION",
                            caption = "BROUGHT TO YOU BY TRISTAN LOUTH-ROBINS. GITHUB: https://github.com/TristanLouthRobins",theme=theme_trek_header())  


ggsave("exports/ranks.png", width = 16, height = 8) 
