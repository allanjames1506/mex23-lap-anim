# 1 Inspiration----
# 1.1* gganimate----
# https://gganimate.com/index.html
# https://rstudio.github.io/cheatsheets/gganimate.pdf
# https://cloud.r-project.org/web/packages/gganimate/vignettes/gganimate.html
# https://anderfernandez.com/en/blog/how-to-create-animations-in-r-with-gganimate/

# 2 Libraries----
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(showtext)
library(readr)
library(gganimate)
library(ggpubr)
#library(jpeg)
library(png)
library(magick)
library(ggimage)
library(ggfx)
remotes::install_github("wilkelab/gridtext")
remotes::install_github("lenkiefer/darklyplot")

# 3 Fonts----
# Loading Google fonts (https://fonts.google.com/)
# e.g. Orbitron font https://fonts.google.com/specimen/Orbitron

# https://www.rdocumentation.org/packages/sysfonts/versions/0.8.8/topics/font_families_google

font_add_google("Rubik","rubik")
font_add_google("Alfa Slab One", "alfa")
font_add_google("Calistoga", "vegas")
font_add_google("Rye", "rye")
showtext_auto()
showtext_opts(dpi = 320)

# 4 (optional) Chequered flag----
# 4.1* development----
# http://sape.inf.usi.ch/quick-reference/ggplot2/geom_rect
d=data.frame(x1=c(1,3,1,5,4), x2=c(2,4,3,6,6), y1=c(1,1,4,1,3), y2=c(2,2,5,3,5), t=c('a','a','a','b','b'), r=c(1,2,3,4,5))
ggplot() + 
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  geom_rect(data=d, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=t), color="black", alpha=0.5) +
  geom_text(data=d, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=4) 

# 4.2* 5 x 5----
d2 <- data.frame(x1 = c(1, 2, 3, 4, 5,
                        1, 2, 3, 4, 5,
                        1, 2, 3, 4, 5,
                        1, 2, 3, 4, 5,
                        1, 2, 3, 4, 5), x2 = c(2, 3, 4, 5, 6,
                                               2, 3, 4, 5, 6,
                                               2, 3, 4, 5, 6,
                                               2, 3, 4, 5, 6,
                                               2, 3, 4, 5, 6), y1 = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5), y2 = c(2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6), t=c('a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                                                         'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                                                         'a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                                                         'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                                                         'a', 'b', 'a', 'b', 'a'), r=c(1, 2, 3, 4, 5,
                                                                                                                                                                                                                                                                       6, 7, 8, 9, 10,
                                                                                                                                                                                                                                                                       11, 12, 13, 14, 15,
                                                                                                                                                                                                                                                                       16, 17, 18, 19, 20,
                                                                                                                                                                                                                                                                       21, 22, 23, 24, 25),
                 fill = c('white', 'black', 'white', 'black', 'white',
                          'black', 'white', 'black', 'white', 'black',
                          'white', 'black', 'white', 'black', 'white',
                          'black', 'white', 'black', 'white', 'black',
                          'white', 'black', 'white', 'black', 'white'))

ggplot() + 
  scale_x_continuous(name="x") + 
  scale_y_continuous(name="y") +
  scale_fill_identity() +
  geom_rect(data=d2, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), inherit.aes = FALSE, alpha=0.5) +
  geom_text(data=d2, aes(x=x1+(x2-x1)/2, y=y1+(y2-y1)/2, label=r), size=20)

# 4.3* 7 x 7 with scale appropriate to annimate plot----
d3 <- data.frame(x1 = c(-10, 0, 10, 20, 30, 40, 50, 60, 70,
                        -10, 0, 10, 20, 30, 40, 50, 60, 70,
                        -10, 0, 10, 20, 30, 40, 50, 60, 70,
                        -10, 0, 10, 20, 30, 40, 50, 60, 70,
                        -10, 0, 10, 20, 30, 40, 50, 60, 70,
                        -10, 0, 10, 20, 30, 40, 50, 60, 70,
                        -10, 0, 10, 20, 30, 40, 50, 60, 70,
                        -10, 0, 10, 20, 30, 40, 50, 60, 70,
                        -10, 0, 10, 20, 30, 40, 50, 60, 70), x2 = c(0, 10, 20, 30, 40, 50, 60, 70, 80,
                                                                    0, 10, 20, 30, 40, 50, 60, 70, 80,
                                                                    0, 10, 20, 30, 40, 50, 60, 70, 80,
                                                                    0, 10, 20, 30, 40, 50, 60, 70, 80,
                                                                    0, 10, 20, 30, 40, 50, 60, 70, 80,
                                                                    0, 10, 20, 30, 40, 50, 60, 70, 80,
                                                                    0, 10, 20, 30, 40, 50, 60, 70, 80,
                                                                    0, 10, 20, 30, 40, 50, 60, 70, 80,
                                                                    0, 10, 20, 30, 40, 50, 60, 70, 80), y1 = c(-0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5,
                                                                                                               2, 2, 2, 2, 2, 2, 2, 2, 2,
                                                                                                               4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5,
                                                                                                               7, 7, 7, 7, 7, 7, 7, 7, 7,
                                                                                                               9.5, 9.5, 9.5, 9.5, 9.5, 9.5, 9.5, 9.5, 9.5,
                                                                                                               12, 12, 12, 12, 12, 12, 12, 12, 12,
                                                                                                               14.5, 14.5, 14.5, 14.5, 14.5, 14.5, 14.5, 14.5, 14.5,
                                                                                                               17, 17, 17, 17, 17, 17, 17, 17, 17,
                                                                                                               19.5, 19.5, 19.5, 19.5, 19.5, 19.5, 19.5, 19.5, 19.5), y2 = c(2, 2, 2, 2, 2, 2, 2, 2, 2,
                                                                                                                                                                             4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5, 4.5,
                                                                                                                                                                             7, 7, 7, 7, 7, 7, 7, 7, 7,
                                                                                                                                                                             9.5, 9.5, 9.5, 9.5, 9.5, 9.5, 9.5, 9.5, 9.5,
                                                                                                                                                                             12, 12, 12, 12, 12, 12, 12, 12, 12,
                                                                                                                                                                             14.5, 14.5, 14.5, 14.5, 14.5, 14.5, 14.5, 14.5, 14.5,
                                                                                                                                                                             17, 17, 17, 17, 17, 17, 17, 17, 17,
                                                                                                                                                                             19.5, 19.5, 19.5, 19.5, 19.5, 19.5, 19.5, 19.5, 19.5,
                                                                                                                                                                             22, 22, 22, 22, 22, 22, 22, 22, 22), t=c('a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                                      'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                                      'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                                      'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                                      'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                                      'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                                      'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                                                      'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b',
                                                                                                                                                                                                                      'a', 'b', 'a', 'b', 'a', 'b', 'a', 'b', 'a'), r=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 
                                                                                                                                                                                                                                                                        10, 11, 12, 13, 14, 15, 16, 17, 18, 
                                                                                                                                                                                                                                                                        19, 20, 21, 22, 23, 24, 25, 26, 27, 
                                                                                                                                                                                                                                                                        28, 29, 30, 31, 32, 33, 34, 35, 36, 
                                                                                                                                                                                                                                                                        37, 38, 39, 40, 41, 42, 42, 44, 45, 
                                                                                                                                                                                                                                                                        46, 47, 48, 49, 50, 51, 52, 53, 54,
                                                                                                                                                                                                                                                                        55, 56, 57, 58, 59, 60, 61, 62, 63,
                                                                                                                                                                                                                                                                        64, 65, 66, 67, 68, 69, 70, 71, 72,
                                                                                                                                                                                                                                                                        73, 74, 75, 76, 77, 78, 79, 80, 81), fill = c('#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', 
                                                                                                                                                                                                                                                                                                                      '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', 
                                                                                                                                                                                                                                                                                                                      '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', 
                                                                                                                                                                                                                                                                                                                      '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', 
                                                                                                                                                                                                                                                                                                                      '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', 
                                                                                                                                                                                                                                                                                                                      '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000',
                                                                                                                                                                                                                                                                                                                      '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d',
                                                                                                                                                                                                                                                                                                                      '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000',
                                                                                                                                                                                                                                                                                                                      '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d'))

d4 <- d3 %>% 
  mutate(fill = case_when(fill == '#FF8000' ~ '#fffaf0', TRUE ~ '#4d4d4d'))

d5 <- data.frame(x1 = c(-10, 0, 10, 20, 30, 40, 50, 
                        -10, 0, 10, 20, 30, 40, 50, 
                        -10, 0, 10, 20, 30, 40, 50, 
                        -10, 0, 10, 20, 30, 40, 50, 
                        -10, 0, 10, 20, 30, 40, 50, 
                        -10, 0, 10, 20, 30, 40, 50, 
                        -10, 0, 10, 20, 30, 40, 50), x2 = c(0, 10, 20, 30, 40, 50, 60, 
                                                            0, 10, 20, 30, 40, 50, 60, 
                                                            0, 10, 20, 30, 40, 50, 60, 
                                                            0, 10, 20, 30, 40, 50, 60, 
                                                            0, 10, 20, 30, 40, 50, 60, 
                                                            0, 10, 20, 30, 40, 50, 60, 
                                                            0, 10, 20, 30, 40, 50, 60), y1 = c(-1, -1, -1, -1, -1, -1, -1, 
                                                                                               2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
                                                                                               6, 6, 6, 6, 6, 6, 6,
                                                                                               9.5, 9.5, 9.5, 9.5, 9.5, 9.5, 9.5,
                                                                                               13, 13, 13, 13, 13, 13, 13,
                                                                                               16.5, 16.5, 16.5, 16.5, 16.5, 16.5, 16.5,
                                                                                               20, 20, 20, 20, 20, 20, 20), y2 = c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 
                                                                                                                                   6, 6, 6, 6, 6, 6, 6,
                                                                                                                                   9.5, 9.5, 9.5, 9.5, 9.5, 9.5, 9.5,
                                                                                                                                   13, 13, 13, 13, 13, 13, 13, 
                                                                                                                                   16.5, 16.5, 16.5, 16.5, 16.5, 16.5, 16.5,
                                                                                                                                   20, 20, 20, 20, 20, 20, 20, 
                                                                                                                                   23.5, 23.5, 23.5, 23.5, 23.5, 23.5, 23.5), t=c('a', 'b', 'a', 'b', 'a', 'b', 'a', 
                                                                                                                                                                                  'b', 'a', 'b', 'a', 'b', 'a', 'b', 
                                                                                                                                                                                  'a', 'b', 'a', 'b', 'a', 'b', 'a', 
                                                                                                                                                                                  'b', 'a', 'b', 'a', 'b', 'a', 'b', 
                                                                                                                                                                                  'a', 'b', 'a', 'b', 'a', 'b', 'a',
                                                                                                                                                                                  'b', 'a', 'b', 'a', 'b', 'a', 'b', 
                                                                                                                                                                                  'a', 'b', 'a', 'b', 'a', 'b', 'a'), r=c(1, 2, 3, 4, 5, 6, 7, 
                                                                                                                                                                                                                          8, 9, 10, 11, 12, 13, 14, 
                                                                                                                                                                                                                          15, 16, 17, 18, 19, 20, 21, 
                                                                                                                                                                                                                          22, 23, 24, 25, 26, 27, 28, 
                                                                                                                                                                                                                          29, 30, 31, 32, 33, 34, 35, 
                                                                                                                                                                                                                          36, 37, 38, 39, 40, 41, 42, 
                                                                                                                                                                                                                          43, 44, 45, 46, 47, 48, 49), fill = c('#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', 
                                                                                                                                                                                                                                                                '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', 
                                                                                                                                                                                                                                                                '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', 
                                                                                                                                                                                                                                                                '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', 
                                                                                                                                                                                                                                                                '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', 
                                                                                                                                                                                                                                                                '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', 
                                                                                                                                                                                                                                                                '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d'))

d6 <- d5 %>% 
  mutate(fill = case_when(fill == '#FF8000' ~ '#fffaf0', TRUE ~ '#4d4d4d'))

# 5 Mexico GP----
# data https://www.statsf1.com/en/2023/mexico-city/tour-par-tour.aspx

# 5.1* Colours----
# https://www.reddit.com/r/Formula1Point5/comments/sri1rf/here_are_the_color_hex_codes_for_mclaren_mcl36/?rdt=56811
# https://teamcolorcodes.com

McLaren_colours_main <- c('#FF8000', '#00843D')
McLaren_colours_blue <- '#47c7fc'
McLaren_colours_papaya <- '#FF8000'
McLaren_colours_las_vegas <- c('#FF8000', '#FF8000', '#FF8000', '#00843D', '#00843D', '#00843D')

# 5.2* Data----
mexico_NOR_PIA <- read.csv('./00_raw_data/mexico_2023_NOR_PIA.csv') %>% 
  pivot_longer(cols ='Lando':'Oscar', names_to = 'driver', values_to = 'lap_position') %>% 
  mutate(driver = factor(driver, levels = c('Lando', 'Oscar')))

las_vegas_PIA <- read.csv('./00_raw_data/las_vegas_2023_PIA.csv') %>% 
  rename(lap_position = Oscar) %>% 
  mutate(driver = 'Oscar')

# 5.3* Static plot----
mex_point_graph <- mexico_NOR_PIA %>%
  ggplot(aes(x = Lap, y = lap_position, size = lap_position, colour = driver, group = driver)) + 
  geom_step(size = 2) +
  geom_point(shape=21, stroke = 4, fill = '#FFCD00') +
  geom_text(aes(Lap, lap_position, label = as.character(lap_position), hjust = ifelse(lap_position > 10, -0.5, 0.5)), vjust = 0.5, size = 12, fontface = 'bold', family = 'rubik', colour = 'grey40') +
  geom_text(aes(Lap , y = -1.5, label = as.character(Lap_id)), data = . %>% 
              filter(Lap %% 2 == 1), hjust = 0.5, size = 12, fontface = 'bold', col = "#4d4d4d", family = 'rubik') +
  #geom_rect(data=d3, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), inherit.aes = FALSE, alpha=0.1) +
  scale_colour_manual(values = McLaren_colours_main) +
  scale_fill_identity() +
  scale_size(range = c(40, 5)) +
  scale_y_reverse(breaks = c(1, 4, 8, 12, 16, 20), labels=c("1" = "1st", "4" = "4th", "8" = "8th", "12" = "12th", "16" = "16th", "20" = "20th"), position = 'right') +
  coord_cartesian(xlim = c(-10, 80), ylim = c(22, -2.5), expand = F) +
  theme_minimal() + 
  theme(plot.margin = unit(c(20, 100, 20, 40), "pt"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray90", color = "transparent"),
        plot.background = element_rect(fill = "gray90"),
        text = element_text(color = "#47c7fc"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family = 'rubik', face = 'bold', colour = '#4d4d4d', size = 32),
        plot.title = element_markdown(size = 40, family = "alfa", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 20, family = "alfa",hjust = 0.5)) +
  labs(x = NULL,
       y = NULL,
       title = "<span style = 'color:#006341'>Mexico </span><span style = 'color:#FFFFFF'> Grand </span><span style = 'color:#C8102E'>Prix </span>",
       caption = "Data: statsF1.com. | Allan James (@allanjames1506)") +
  annotate(
    "text",
    x = c(1, 1),
    y = c(15, 5),
    label = c('LN4', 'OP81'),
    hjust = c(1, 1),
    vjust = c(1, 1),
    size = 10,
    colour = c('#FF8000', '#00843D'),
    fontface = "bold",
    family = "alfa"
  )

mex_point_graph

# 5.4* Animate----
anim_mex <- mex_point_graph +
  transition_reveal(Lap) 

anim_save("./04_gifs/second_saved_animation_anim_mex.gif", anim_mex, height = 600, width = 800)

# 6 Lando's laps only Mexico----
# 6.1* Static plot----
# 6.1.1** with chequered background----
mex_point_graph_lando <- mexico_NOR_PIA %>%
  filter(driver == 'Lando') %>%
  ggplot(aes(x = Lap, y = lap_position, size = lap_position, colour = driver, group = driver)) + 
  geom_rect(data=d4, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), inherit.aes = FALSE, alpha = 0.2) +
  geom_step(size = 4) +
  geom_point(shape=21, stroke = 4, fill = '#47c7fc') +
  geom_text(aes(Lap, lap_position, label = as.character(lap_position), hjust = ifelse(lap_position > 10, -0.5, 0.5)), vjust = 0.5, size = 12, fontface = 'bold', family = 'rubik', colour = 'grey40') +
  scale_colour_manual(values = McLaren_colours_papaya) +
  scale_fill_identity() +
  scale_size(range = c(40, 5)) +
  coord_cartesian(xlim = c(-10, 80), ylim = c(19.5, 2), expand = F) +
  theme_minimal() + 
  theme(plot.margin = unit(c(20, 40, 20, 40), "pt"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray90", color = "transparent"),
        plot.background = element_rect(fill = "gray90"),
        text = element_text(color = '#4d4d4d'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(size = 16, family = "alfa",hjust = 0.5)) +
  labs(x = NULL,
       y = NULL,
       title = NULL,
       caption = "Data: statsF1.com. | Allan James (@allanjames1506)") +
  annotate(
    "text",
    x = 1,
    y = 15,
    label = 'LN4',
    hjust = 1,
    vjust = 1,
    size = 10,
    colour = '#FF8000',
    fontface = "bold",
    family = "alfa"
  )

mex_point_graph_lando

# 6.1.2** with stealth background----
mex_point_graph_lando_stealth <- mexico_NOR_PIA %>%
  filter(driver == 'Lando') %>%
  ggplot(aes(x = Lap, y = lap_position, size = lap_position, colour = driver, group = driver)) + 
  darklyplot::theme_dark2() +
  geom_rect(data=d4, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), inherit.aes = FALSE, alpha = 0.2) +
  with_outer_glow(geom_step(color='white', size = 4), colour='gold', sigma = 5, expand = 5) +
  with_outer_glow(
      geom_point(shape =21, stroke = 4, fill = 'white'),
    colour = "#47c7fc", sigma = 5, expand = 5) +
  #geom_point(shape=21, stroke = 4, fill = '#47c7fc') +
  geom_text(aes(Lap, lap_position, label = as.character(lap_position), hjust = ifelse(lap_position > 10, -0.5, 0.5)), vjust = 0.5, size = 12, fontface = 'bold', family = 'rubik', colour = 'gold') +
  geom_text(aes(Lap , y = -1.5, label = as.character(Lap_id)), data = . %>% 
              filter(Lap %% 2 == 1), hjust = 0.5, size = 12, fontface = 'bold', col = "gold", family = 'rubik') +
  scale_colour_manual(values = McLaren_colours_papaya) +
  scale_fill_identity() +
  scale_size(range = c(40, 5)) +
  scale_y_reverse(breaks = c(1, 4, 8, 12, 16, 20), labels=c("1" = "1st", "4" = "4th", "8" = "8th", "12" = "12th", "16" = "16th", "20" = "20th"), position = 'right') +
  coord_cartesian(xlim = c(-10, 80), ylim = c(22, -2.5), expand = F) +
  theme(plot.margin = unit(c(20, 100, 20, 40), "pt"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "gray30", color = "transparent"),
        #plot.background = element_rect(fill = "gray90"),
        text = element_text(color = 'gold'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(family = 'rubik', face = 'bold', colour = 'gold', size = 32),
        plot.caption = element_text(size = 16, family = "alfa",hjust = 0.5)) +
  labs(x = NULL,
       y = NULL,
       title = NULL,
       caption = "Data: statsF1.com. | Allan James (@allanjames1506)") +
  annotate(
    "text",
    x = 1,
    y = 15,
    label = 'LN4',
    hjust = 1,
    vjust = 1,
    size = 10,
    colour = '#FF8000',
    fontface = "bold",
    family = "alfa"
  )

mex_point_graph_lando_stealth

# 6.2* Animate----
# 6.2.1** with chequred background
anim_mex_lando <- mex_point_graph_lando +
  transition_reveal(Lap) 

anim_save("./04_gifs/first_saved_animation_anim_mex_lando.gif", anim_mex_lando, height = 600, width = 800)

# 6.2.2** with stealth background
anim_mex_lando_stealth <- mex_point_graph_lando_stealth +
  transition_reveal(Lap) 

anim_save("./04_gifs/first_saved_animation_anim_mex_lando_stealth.gif", anim_mex_lando_stealth, height = 600, width = 800)

# 7 Oscar's laps only Las Vegas----
# 7.1* Static plot----
# 7.1.1** with stealth and chequered background----

img <- readPNG('./00_raw_data/cherry_clipart3.png')

las_vegas_point_graph_oscar_stealth <- las_vegas_PIA %>%
  ggplot(aes(x = Lap, y = lap_position, size = lap_position, colour = driver, group = driver)) +
  background_image(img) +
  darklyplot::theme_dark2() +
  geom_rect(data = d6, mapping=aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2, fill = fill), inherit.aes = FALSE, alpha = 0.2) +
  with_outer_glow(geom_step(color='white', size = 4), colour='gold', sigma = 5, expand = 5) +
  with_outer_glow(
    geom_point(shape =21, stroke = 4, fill = 'floralwhite'),
    colour = "gold", sigma = 5, expand = 5) +
  with_outer_glow(geom_text(aes(Lap, lap_position, label = as.character(lap_position), hjust = ifelse(lap_position > 10, -0.5, 0.5)), vjust = 0.5, size = 12, fontface = 'bold', colour = '#FF8000'), colour = "#47c7fc", sigma = 5, expand = 5) +
  with_outer_glow(geom_text(aes(Lap , y = -2.5, label = as.character(Lap_id)), data = . %>% 
              filter(Lap %% 2 == 0), hjust = 0.5, size = 12, fontface = 'bold', col = 'white'), colour='gold', sigma = 5, expand = 5) +
  scale_colour_manual(values = 'white') +
  scale_fill_identity() +
  # geom_image(aes(Lap, y = y_axis, image = image, group = image), data = . %>%
  #              filter(Lap %% 5 == 1), hjust = 0.5, size = 0.1, alpha = 0.5, inherit.aes = FALSE) +
  scale_size(range = c(40, 5)) +
  scale_y_reverse(breaks = c(1, 5, 10, 15, 20), labels=c("1" = "1st", "5" = "5th", "10" = "10th", "15" = "15th", "20" = "20th"), position = 'right') +
  coord_cartesian(xlim = c(-10, 60), ylim = c(25, -4), expand = F) +
  theme(plot.margin = unit(c(20, 100, 20, 40), "pt"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        #panel.background = element_rect(fill = "grey10", color = "transparent"),
        #plot.background = element_rect(fill = "grey10"),
        #text = element_text(color = 'gold', family = 'rye'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_text(family = 'rubik', face = 'bold', colour = 'gold', size = 32),
        #plot.title = element_markdown(size = 40, family = "vegas", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 16, hjust = 0.5, family = "rye", colour = 'gold')) +
  labs(x = NULL,
       y = NULL,
       title = NULL,
       caption = "Data: statsF1.com. | Allan James (@allanjames1506)") +
  annotate(
    "text",
    x = -0.5,
    y = 16,
    label = 'OP',
    hjust = 1,
    vjust = 1,
    size = 20,
    colour = '#FF8000',
    fontface = "bold",
    family = "rye",
    alpha = 0.95
  ) +
  annotate(
    "text",
    x = 0,
    y = 18.5,
    label = '81',
    hjust = 1,
    vjust = 1,
    size = 24,
    colour = '#FF8000',
    fontface = "bold",
    family = "rye", 
    alpha = 0.95
  ) +
  annotate("text", y = 2.2, x = -10, label = "LAS", lineheight = 0.75, family = 'rye',  size = 55, color = "deeppink", hjust = 0, alpha = 0.75) +
  annotate("text", y = 20.5, x = 60, label = "VEGAS", lineheight = 0.75, family = 'rye',  size = 50, color = "chartreuse", hjust = 1, alpha = 0.75)
  
las_vegas_point_graph_oscar_stealth

# 7.2* Animate
anim_las_vegas_oscar_stealth <- las_vegas_point_graph_oscar_stealth +
  transition_reveal(Lap) 

anim_save("./04_gifs/first_saved_animation_anim_las_vegas_oscar_stealth.gif", anim_las_vegas_oscar_stealth, height = 600, width = 800)

# 8 All Teams----
# 8.1* Data----
mexico_all_drivers <- read.csv('./00_raw_data/mexico_2023_lap_pos.csv') %>%
  pivot_longer(cols ='LEC':'STR', names_to = 'driver', values_to = 'lap_position') %>% 
  mutate(constructor = case_when(driver %in% c('VER', 'PER') ~ 'Red Bull',
                                 driver %in% c('HAM', 'RUS') ~ 'Mercedes',
                                 driver %in% c('LEC', 'SAI') ~ 'Ferrari',
                                 driver %in% c('ALO', 'STR') ~ 'Aston Martin',
                                 driver %in% c('NOR', 'PIA') ~ 'McLaren',
                                 driver %in% c('OCO', 'GAS') ~ 'Alpine',
                                 driver %in% c('ALB', 'SAR') ~ 'Williams',
                                 driver %in% c('BOT', 'ZHO') ~ 'Alfa Romeo',
                                 driver %in% c('TSU', 'RIC') ~ 'Alpha Tauri',
                                 driver %in% c('MAG', 'HUL') ~ 'Haas',
                                 TRUE ~ NA)) %>% 
  mutate(driver = factor(driver, levels = c('VER', 'PER',
                                            'HAM', 'RUS',
                                            'LEC', 'SAI',
                                            'ALO', 'STR',
                                            'NOR', 'PIA',
                                            'OCO', 'GAS',
                                            'ALB', 'SAR',
                                            'BOT', 'ZHO',
                                            'TSU', 'RIC',
                                            'MAG', 'HUL'
                                            ))) %>% 
  mutate(constructor = factor(constructor, levels = c('Red Bull',
                                                        'Mercedes',
                                                        'Ferrari',
                                                        'Aston Martin',
                                                        'McLaren',
                                                        'Alpine',
                                                        'Williams',
                                                        'Alfa Romeo',
                                                        'Alpha Tauri',
                                                        'Haas')))

mexico_all_drivers_x_axis <- mexico_all_drivers %>% 
  mutate(x_axis = case_when(driver %in% c('VER', 'HAM', 'LEC', 'ALO', 'NOR', 'OCO', 'ALB', 'BOT', 'TSU', 'MAG') ~ -2.5,
                            driver %in% c('PER', 'RUS', 'SAI', 'STR', 'PIA', 'GAS', 'SAR', 'ZHO', 'RIC', 'HUL') ~ 2.5,
                            TRUE ~ NA),
         y_axis = 1) %>% 
  mutate(image = case_when(lap_position <= 5 ~ cherry2,
                           lap_position >5 & lap_position <= 15 ~ three_bar,
                           lap_position >15 ~ lucky7,
                           TRUE ~ NA))

puggy1 <- mexico_all_drivers_x_axis %>%
  filter(constructor == 'McLaren') %>%
  ggplot(aes(x = x_axis, y = y_axis, colour = driver, group = driver)) + 
  #geom_rect(data=d4, mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill=fill), inherit.aes = FALSE, alpha = 0.2) +
  #geom_step(size = 4) +
  geom_point(shape=21, stroke = 4, fill = '#47c7fc', size = 50) +
  geom_text(aes(x_axis, y_axis, label = as.character(lap_position)), hjust = 0.5, vjust = 0.5, size = 12, fontface = 'bold', family = 'rubik', colour = 'grey40') +
  scale_colour_manual(values = McLaren_colours_main) + 
  #scale_fill_identity() 
  scale_size(range = c(40, 5)) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0.5, 1.5), expand = F) +
  theme_minimal() + 
  theme(plot.margin = unit(c(20, 40, 20, 40), "pt"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray90", color = "transparent"),
        plot.background = element_rect(fill = "gray90"),
        text = element_text(color = '#4d4d4d'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(size = 16, family = "alfa",hjust = 0.5)) +
  labs(x = NULL,
       y = NULL,
       title = NULL,
       caption = "Data: statsF1.com. | Allan James (@allanjames1506)") +
  annotate(
    "text",
    x = 1,
    y = 15,
    label = 'LN4',
    hjust = 1,
    vjust = 1,
    size = 10,
    colour = '#FF8000',
    fontface = "bold",
    family = "alfa"
  )

puggy1

# 8.2* Animate----
anim_puggy1 <- puggy1 +
  transition_reveal(Lap) 

anim_save("./04_gifs/first_saved_animation_anim_puggy1.gif", anim_puggy1, height = 600, width = 800)

# image processing
cherry2 <- image_read("./00_raw_data/two_cherry.png")
cherry2
three_bar <- image_read("./00_raw_data/three_bar_no2.png")
three_bar <- image_scale(three_bar, geometry = "620")
three_bar
lucky7 <- image_read("./00_raw_data/lucky7_no2.png")
lucky7

# https://www.xsnoize.com/the-evolution-of-slot-machine-symbols/
# https://www.clipartmax.com
image_path <- "./00_raw_data/"

cherry2 <- paste0(image_path, 'two_cherry.png')
three_bar <- paste0(image_path, 'three_bar_no2.png')
lucky7 <- paste0(image_path, 'lucky7_no2.png')

cherry2_clipart <- paste0(image_path, 'cherry_clipart.png')
cherry2_clipart3 <- paste0(image_path, 'cherry_clipart3.png')
luck7_clipart <- paste0(image_path, 'seven_clipart.png')
lemon_clipart <- paste0(image_path, 'lemon2_clipart.png')

# Development work----

mexico_all_drivers_test <- read.csv('./00_raw_data/mexico_2023_lap_pos.csv') %>%
  select(1:2, 19) %>% 
  #group_by(Lap) %>% 
  #mutate(bottom = runif(72, 0.8, 1), middle = 1, top = runif(72, 1, 1.2)) %>%
  #pivot_longer(cols = 'bottom':'top', names_to = 'wobble', values_to = 'y_axis')%>%
  mutate(image1 = case_when(NOR <= 5 ~ cherry2,
                          NOR >5 & NOR <= 15 ~ three_bar,
                          NOR >15 ~ lucky7,
                          TRUE ~ NA),
         image2 = case_when(NOR  <= 5 ~ cherry2,
                          NOR >5 & NOR <= 15 ~ lucky7,
                          NOR >15 ~ three_bar,
                          TRUE ~ NA),
         image3 = case_when(NOR  <= 5 ~ cherry2,
                            NOR >5 & NOR <= 15 ~ cherry2,
                            NOR >15 ~ lucky7,
                            TRUE ~ NA)) %>% 
  mutate_if(is.integer, as.character) %>% 
  pivot_longer(cols ='image1':'image3', names_to = 'image', values_to = 'file') %>% 
  mutate(y_axis = 1) %>% 
  mutate_at(1, as.numeric)

glimpse(mexico_all_drivers_test)

a <- ggplot(Final, aes(x = Step, y = Value, group = Name, color = factor(Name))) + 
  geom_line(size=1) + 
  geom_image(aes(image=Image)) +
  transition_reveal(Step) + 
  coord_cartesian(clip = 'off') + 
  theme_minimal() +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5)) +
  theme(legend.position = "none") 

puggy2 <- mexico_all_drivers_test %>%
  #group_by(Lap) %>% 
  ggplot(aes(x = image, y = y_axis, image = file)) + 
  geom_image(size = 0.3) 
#+geom_point()
#%>% 
  geom_text(aes(x_axis, y_axis, label = as.character(lap_position)), hjust = 0.5, vjust = 0.5, size = 12, fontface = 'bold', family = 'rubik', colour = 'grey40') +
  scale_colour_manual(values = McLaren_colours_las_vegas) + 
  #scale_fill_identity() 
  scale_size(range = c(40, 5)) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(0.5, 1.5), expand = F) +
  theme_minimal() 
#+ 
  theme(plot.margin = unit(c(20, 40, 20, 40), "pt"),
        legend.position = "none",
        panel.grid = element_blank(),
        panel.background = element_rect(fill = "gray90", color = "transparent"),
        plot.background = element_rect(fill = "gray90"),
        text = element_text(color = '#4d4d4d'),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),
        plot.caption = element_text(size = 16, family = "alfa",hjust = 0.5)) 
#+
labs(x = NULL,
     y = NULL,
     title = NULL,
     caption = "Data: statsF1.com. | Allan James (@allanjames1506)") +
  annotate(
    "text",
    x = 1,
    y = 15,
    label = 'LN4',
    hjust = 1,
    vjust = 1,
    size = 10,
    colour = '#FF8000',
    fontface = "bold",
    family = "alfa"
  )

anim_puggy2 <- puggy2 +
  transition_states(Lap, transition_length = 2, state_length = 1) +
  enter_fade() +
  exit_fade() +
  ease_aes('sine-in-out')

anim_save("./04_gifs/first_saved_animation_anim_puggy2.gif", anim_puggy2, height = 600, width = 800)





