library(tidyverse)
library(magrittr)
library(ggthemes) 
library(lubridate)
library(gridExtra)
library(grid)


video_games <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-30/video_games.csv")

video_games %<>%
  mutate(release_date_pretty = as.Date(release_date, format = '%b %d, %Y'),
         age = as.numeric(Sys.Date()-release_date_pretty))

ggplot() + 
  geom_point(data = video_games %>% filter(price < 65),
              aes(x = price,
                  y = metascore,
                  group = price))

ggplot() +
  geom_point(data = video_games,
            aes(x = release_date_pretty,
                y = metascore))

p <- ggplot() +
  geom_jitter(data = video_games %>% 
               group_by(release_date_pretty) %>%
               summarise(count_games = n()) %>%
               mutate(day_of_week = weekdays(release_date_pretty),
                      weekend = ifelse(day_of_week %in% c('Saturday', 'Sunday'),
                                       'yes',
                                       'no')),
             aes(x = release_date_pretty,
                 y = count_games,
                 color = weekend),
             alpha = 0.3,
             size = 3) +
  geom_text(aes(x = ymd('2012-01-01'),
              y = 25,
              size = 20,
              label = 'Number of daily releases increased a lot since 2015, \nalthough releases on weekends are more rare')) +
  geom_curve(aes(x = ymd('2012-12-01'),
                 xend = ymd('2014-01-01'),
                 y = 20,
                 yend = 14),
             color = 'slategrey',
             arrow = arrow(length = unit(0.03, "npc"))) +
  theme_tufte(base_size = 18) + 
  theme(legend.position = 'none') +
  scale_color_brewer(palette = 'Set2') +
  labs(x = 'Date',
       y = 'Releases per day',
       caption = 'Data from #tidytuesday. Visualisation by @kuprinasha')
  

palette_colors = brewer.pal(3, 'Set2')
title = c('\nNumber of games released daily ','\non the weekends','\n and','\n weekdays')
colors = c('black', palette_colors[1], 'black', palette_colors[2],'black')

p_grid <- arrangeGrob(p, 
             top = tableGrob(t(title), 
                             theme=ttheme_minimal(padding=unit(c(1,1,1,1),'mm'),
                                                  base_colour = colors,
                                                  base_size = 24)))
ggsave('plot.png', p_grid,
       width = 10, height = 6, units = 'in')