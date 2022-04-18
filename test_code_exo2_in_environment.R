source(file = Intro_DS_HW3_Exo2_data.R, local = T)

library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(gganimate)
library(directlabels)
library(circular)
library(units)

bacteria_plot_2 <- function(c,seed_choice,bacteria,move){
  
positions_2 <- bacteria_data(c, seed, choice, bacteria, move)

#Creating images of animation
for(i in seq(from = 1, to = steps)){
  
  data_positions = positions_2 %>%
    filter(time == i) %>%
    select(name, x, y)
  
  line_positions = positions_2 %>%
    filter(time <= i) %>%
    select(name, x, y)
  
  count_center = 0
  
  for(j in seq(from = 1, to = bacteria)){
    if ((sqrt(abs(data_positions[j,2])^2+abs(data_positions[j,3])^2))<=3){
      count_center = count_center + 1
    }
  }
  
  name = paste0("plot_bacteria_", i)
  
  plot_i = ggplot(data = data_positions, aes(x = x, y = y, color = name)) +
    geom_polygon(aes(x = x1, y = y1),fill = "tan2", linetype = 1, colour = "black", data = df, inherit.aes = F) +
    annotate("text", x = 0, y = 0, label = "SUGAR", col = "white", size = 6) +
    annotate("text", x = 6, y = 10, label = paste0("Bacteria inside sugar: ", count_center), col = "black", size = 6) +
    geom_point(pch = 11) +
    geom_line(data = line_positions, aes(x = x, y = y, color = name, group = name), alpha = 0.4) +
    theme_bw() +
    theme(legend.position = "none") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
    theme(axis.title.x = element_text(size = 14, face="bold"),
          axis.title.y = element_text(size=14, face="bold")) +
    scale_x_continuous(name = 'X Position', breaks = seq(-10,10, by = 5)) +
    scale_y_continuous(name = 'Y Position', breaks = seq(-10,10, by = 5)) +
    labs(title = 'Bacteria Mobility') +
    coord_cartesian(ylim = c(-10,10), xlim = c(-12,12))
  
  assign(name, plot_i)
}

desired_graph <- paste0("plot_bacteria_", graph_no)

plot_bacteria_10

}


