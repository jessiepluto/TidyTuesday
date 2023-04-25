librarian::shelf(tidyverse,ggplot2)

tuesdata <- tidytuesdayR::tt_load('2023-04-25')
tuesdata <- tidytuesdayR::tt_load(2023, week = 17)

winners <- tuesdata$winners
london_marathon <- tuesdata$london_marathon

# trend of winners by Category over Year
ggplot(winners, aes(x=Year)) + 
  geom_line(aes(y=Time, col=Category)) + 
  theme_bw()

# facet grid of winners by Category over Year
ggplot(winners, aes(Year, Time)) + 
  geom_point(colour = "#88398A") + 
  facet_grid(cols = vars(Category)) + 
  theme_bw()

# facet grid of winners by Category and Gender over Year
winners %>% 
  mutate(Gender = gsub('Wheelchair ','',Category),
         Wheelchair = ifelse(nchar(Category) > 6,1,0)) %>% 
  ggplot(aes(Year, Time)) + 
  geom_point(colour = "#88398A") + 
  facet_grid(cols = vars(Gender), rows = vars(Wheelchair), scales = "free_y") + 
  theme_bw()


athlete_df <- winners %>% 
  filter(Category == "Women") %>% 
  mutate(d = (min(Time) - Time)/60,
         TimeRank = dense_rank(-d)) %>% 
  group_by(Athlete) %>% 
  mutate(minTime = min(Time)) %>% 
  ungroup() %>% 
  mutate(AthleteRank = dense_rank(minTime),
         Athlete = fct_reorder(Athlete, -AthleteRank),
         y = dense_rank(-AthleteRank)) %>%
  select(-minTime)



ggplot(athlete_df, aes(x=Athlete, y=d)) +
  geom_segment(aes(x=Athlete, xend=Athlete, y=0, yend=d), color="#D3D3D3") +
  geom_point(color="#88398A", size=5) +
  coord_flip() +
  scale_y_continuous(trans = "reverse") +
  labs(
    x = "",
    y = "Seconds From Fastest Record",
    title = "Winners of the Women's London Marathon"
  ) +
  annotate("text", label=athlete_df$TimeRank, x=athlete_df$y,y=athlete_df$d, color="white", size=3) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text.y=element_text(color = "#562457", size = 10, margin = margin(0,0,10,10)),
    axis.text.x=element_text(size = 10),
    axis.ticks = element_blank(),
    legend.position = 'none',
    plot.margin = margin(10,10,10,10)  
  )






# winners plot from https://github.com/nrennie/LondonMarathon/blob/main/inst/plots.R
ggplot(
  data = winners %>%
    # filter(Category == "Women") %>% 
    group_by(Nationality) %>%
    summarise(n = n()),
  mapping = aes(
    y = reorder(Nationality, n),
    x = n)) +
  geom_col(fill = "#88398A") +
  geom_text(aes(label = n),
            colour = "#88398A",
            hjust = -1 ) +
  labs(
    x = "Number of winners",
    title = "Nationality of London Marathon Winners") +
  scale_x_continuous(limits = c(0, 50)) +
  coord_cartesian(expand = FALSE) +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    plot.background = element_rect(fill = "white", colour = "white"),
    panel.background = element_rect(fill = "white", colour = "white")
  )


df <- winners %>% 
  filter(Category == "Women") %>% 
  mutate(TimeRank = dense_rank(Time)) %>% 
  group_by(Athlete) %>% 
  mutate(minTime = min(Time)) %>% 
  ungroup() %>% 
  mutate(AthleteRank = dense_rank(minTime),
         Athlete = fct_reorder(Athlete, -AthleteRank)) %>%
  select(-minTime)



ggplot(df, aes(x=Athlete, y=Time, label=Athlete)) +
  geom_segment( aes(x=Athlete, xend=Athlete, y=min(Time), yend=Time), color="#D3D3D3") +
  geom_point( aes(alpha=-TimeRank^3), color="#88398A", size=5) +
  geom_text(check_overlap = T, nudge_y = -500) +
  coord_flip() +
  labs(
    x = "",
    y = "Winning Time",
    title = "Fastest Winners of the Women's London Marathon"
  ) +
  #annotate("text", x = 1:length(unique(df$Athlete)), y = 2, label = unique(df$Athlete)) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text.y=element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none',
    plot.margin = margin(0,0,0,50)
  )



ggplot(df, aes(x=Athlete, y=Time)) +
  geom_segment( aes(x=Athlete, xend=Athlete, y=min(Time), yend=Time), color="#D3D3D3") +
  geom_point( aes(alpha=-TimeRank^3), color="#88398A", size=5) +
  annotate("text", label=axis_labels, x=1:28,y=unique(df$AthleteRank)) +
  coord_flip() +
  labs(
    x = "",
    y = "Winning Time",
    title = "Fastest Winners of the Women's London Marathon"
  ) +
  #annotate("text", x = 1:length(unique(df$Athlete)), y = 2, label = unique(df$Athlete)) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.text.y=element_blank(),
    axis.ticks = element_blank(),
    legend.position = 'none',
    plot.margin = margin(0,0,0,50)
  )







#ggsave(filename = "winners.png", height = 7, width = 5)