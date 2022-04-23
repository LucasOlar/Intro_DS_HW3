library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(gganimate)
library(directlabels)
library(circular)
library(units)

#bacteria 
bacteria <- 25

#Should it move once inside 
move <- TRUE

#value of C
c <- 36

#setting seed
seed_choice <- 1
set.seed(seed_choice)

#Number of steps
steps <- 100
  
#initial position
initial_position <- matrix(0, steps, bacteria*2)
  
#Give an initial position 
for (i in seq(0, bacteria*2)){
    initial_position [1,i] = runif(1, min = -8, max = 8)
}
  
# Start random walk
for (i in seq(1, bacteria*2, 2)){
  #determining the position at point t = 1
  for(j in seq(1, steps-1)){
      
    norm = sqrt(abs(initial_position[j,i])^2+abs(initial_position[j,i+1])^2)
    if (norm <= 3 && move == T){
      condition = 0
      while (condition != 1){
        # Draw a random number 
        St = runif(1, 0, 2)
          
        #creating the K of the Von Mises
        k = c / (sqrt(abs(initial_position[j,i])^2+abs(initial_position[j,i+1])^2))
          
        #Creating the mu of the Von Mises
        mu = atan2(-initial_position[j,i+1], -initial_position[j,i])
          
        #creating delta 
        delta = as.numeric(rvonmises(n = 1, mu = mu, kappa = k))
          
        #Using the delta to move
        initial_position[j+1,i] = initial_position[j,i] + St*cos(delta)
        initial_position[j+1,i+1] = initial_position[j,i+1] + St*sin(delta)
          
        if(sqrt(abs(initial_position[j+1,i])^2+abs(initial_position[j+1,i+1])^2)<=3){
          condition = 1
        }
      } 
    }
    else {
      # Draw a random number 
      St = runif(1, 0, 2)
      
      #creating the K of the Von Mises
      k = c / (sqrt(abs(initial_position[j,i])^2+abs(initial_position[j,i+1])^2))
        
      #Creating the mu of the Von Mises
      mu = atan2(-initial_position[j,i+1], -initial_position[j,i])
        
      #creating delta 
      delta = as.numeric(rvonmises(n = 1, mu = mu, kappa = k))
        
      #Using the delta to move
      initial_position[j+1,i] = initial_position[j,i] + St*cos(delta)
      initial_position[j+1,i+1] = initial_position[j,i+1] + St*sin(delta)
    }
  }
}
  
  if (move == F){
    #Creating the central zone after which it no longer moves
    for (i in seq(1, bacteria*2, 2)){
      for (j in seq(1, steps-1)){
        position_X = initial_position[j,i]
        position_Y = initial_position[j,i+1]
        norm = sqrt(abs(position_X)^2+abs(position_Y)^2)
        if (norm <= 3){
          initial_position[j+1,i] =  position_X
          initial_position[j+1,i+1] = position_Y 
        }
      }
    }
  }
  
  #putting the columns below each other 
  seq_even = seq(from = 2, to = 2*bacteria, by = 2)
  positions_even = data.frame(initial_position[,seq_even])
  positions_even = tidyr::gather(positions_even)
  
  seq_uneven = seq(from = 1, to = 2*bacteria, by = 2)
  positions_uneven = data.frame(initial_position[,seq_uneven])
  positions_uneven = tidyr::gather(positions_uneven)
  
  positions <- data.frame(cbind(positions_uneven, positions_even))
  
  #Naming data
  x = 1
  name_matrix <- matrix(NA, steps*bacteria, 1)
  time_matrix <- matrix(NA, steps*bacteria, 1)
  for (i in seq(from = 1, to = bacteria)){
    for (j in seq(from = 1, to = steps)){
      name_matrix[x,] = paste("Bacteria ", i)
      integer = as.integer(j)
      time_matrix[x,] = integer
      x = x+1
    }
  }
  positions[,1] = name_matrix[,1]
  positions[,3] = time_matrix[,1]
  
  colnames(positions) <- c("name", "x", "time", "y")
  
  #making the circle 
  angle <- seq(-pi, pi, length = 50)
  df <- data.frame(x1 = sin(angle)*3, y1 = cos(angle)*3)
  
  #Creating images of animation
  #dir.create(file.path("plotsHW3"), recursive = TRUE)
  
  #Change value here of from to get graph
  for(i in seq(from = 1, to = 2)){
    
    data_positions = positions %>%
      filter(time == i) %>%
      select(name, x, y)
    
    line_positions = positions %>%
      filter(time <= i) %>%
      select(name, x, y)
    
    count_center = 0
    
    for(j in seq(from = 1, to = bacteria)){
      if ((sqrt(abs(data_positions[j,2])^2+abs(data_positions[j,3])^2))<=3){
        count_center = count_center + 1
      }
    }
    
    plot_bacteria = ggplot(data = data_positions, aes(x = x, y = y, color = name)) +
      geom_polygon(aes(x = x1, y = y1),fill = "tan2", linetype = 1, colour = "black", data = df, inherit.aes = F) +
      annotate("text", x = 0, y = 0, label = "SUGAR", col = "white", size = 6) +
      annotate("text", x = 6, y = 10, label = paste0("Bacteria inside sugar:", count_center), col = "black", size = 6) +
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
    
    
   # fp <- file.path(file.path("plotsHW3"), paste0(i, ".png"))
   # ggsave(plot = plot_bacteria, filename = fp, device = "png")
  }
plot_bacteria


