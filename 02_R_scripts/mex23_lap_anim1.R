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
remotes::install_github("wilkelab/gridtext")

# 3 Fonts----
# Loading Google fonts (https://fonts.google.com/)
# e.g. Orbitron font https://fonts.google.com/specimen/Orbitron

# https://www.rdocumentation.org/packages/sysfonts/versions/0.8.8/topics/font_families_google

font_add_google("Rubik","rubik")
font_add_google("Alfa Slab One", "alfa")
showtext_auto()

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
d3 <- data.frame(x1 = c(1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60,
                        1, 10, 20, 30, 40, 50, 60), x2 = c(10, 20, 30, 40, 50, 60, 70,
                                                           10, 20, 30, 40, 50, 60, 70,
                                                           10, 20, 30, 40, 50, 60, 70,
                                                           10, 20, 30, 40, 50, 60, 70,
                                                           10, 20, 30, 40, 50, 60, 70,
                                                           10, 20, 30, 40, 50, 60, 70,
                                                           10, 20, 30, 40, 50, 60, 70), y1 = c(-0.5, -0.5, -0.5, -0.5, -0.5, -0.5, -0.5,
                                                                                               2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
                                                                                               5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5,
                                                                                               8.5, 8.5, 8.5, 8.5, 8.5, 8.5, 8.5,
                                                                                               11.5, 11.5, 11.5, 11.5, 11.5, 11.5, 11.5,
                                                                                               14.5, 14.5, 14.5, 14.5, 14.5, 14.5, 14.5,
                                                                                               17.5, 17.5, 17.5, 17.5, 17.5, 17.5, 17.5), y2 = c(2.5, 2.5, 2.5, 2.5, 2.5, 2.5, 2.5,
                                                                                                                                                 5.5, 5.5, 5.5, 5.5, 5.5, 5.5, 5.5,
                                                                                                                                                 8.5, 8.5, 8.5, 8.5, 8.5, 8.5, 8.5,
                                                                                                                                                 11.5, 11.5, 11.5, 11.5, 11.5, 11.5, 11.5,
                                                                                                                                                 14.5, 14.5, 14.5, 14.5, 14.5, 14.5, 14.5,
                                                                                                                                                 17.5, 17.5, 17.5, 17.5, 17.5, 17.5, 17.5,
                                                                                                                                                 20.5, 20.5, 20.5, 20.5, 20.5, 20.5, 20.5), t=c('a', 'b', 'a', 'b', 'a', 'b', 'a',
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
                                                                                                                                                                                                                                        42, 44, 45, 46, 47, 48, 49), fill = c('#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', 
                                                                                                                                                                                                                                                                              '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', 
                                                                                                                                                                                                                                                                              '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d',
                                                                                                                                                                                                                                                                              '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000',
                                                                                                                                                                                                                                                                              '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d',
                                                                                                                                                                                                                                                                              '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000',
                                                                                                                                                                                                                                                                              '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d', '#FF8000', '#4d4d4d'))

# 5 Mexico GP----
# data https://www.statsf1.com/en/2023/mexico-city/tour-par-tour.aspx

# 5.1* Colours----
# https://www.reddit.com/r/Formula1Point5/comments/sri1rf/here_are_the_color_hex_codes_for_mclaren_mcl36/?rdt=56811
# https://teamcolorcodes.com

McLaren_colours_main <- c('#FF8000', '#00843D')

# 5.2* Data----
mexico_NOR_PIA <- read.csv('./00_raw_data/mexico_2023_NOR_PIA.csv') %>% 
  pivot_longer(cols ='Lando':'Oscar', names_to = 'driver', values_to = 'lap_position') %>% 
  mutate(driver = factor(driver, levels = c('Lando', 'Oscar')))

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
        plot.subtitle = element_markdown(size = 20, family = "alfa", face = "bold", hjust = 0.5),
        plot.caption = element_text(size = 20, family = "alfa",hjust = 0.5)) +
  labs(x = NULL,
       y = NULL,
       title = "<span style = 'color:#006341'>Mexico </span><span style = 'color:#FFFFFF'> Grand </span><span style = 'color:#C8102E'>Prix </span>",
       caption = "Data: statsF1.com. | Allan James (@allanjames1506)") +
  annotate(
    "text",
    x = c(1,1),
    y = c(15, 5),
    label = c('LN4', 'OP81'),
    hjust = c(1, 1),
    vjust = c(1, 1),
    size = 10,
    colour = c('#FF8000', '#00843D'),
    fontface = "bold",
    family = "alfa"
  )

#mex_point_graph

# 5.4* Animate----
anim_mex <- mex_point_graph +
  transition_reveal(Lap) 

anim_save("./04_gifs/first_saved_animation_anim_mex.gif", anim_mex, height = 600, width = 800)

