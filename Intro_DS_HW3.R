library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)

#number of bacteria
bacteria <- 5

# Number of steps
steps <- 120

#initial position
initial_position <- matrix(0, steps, bacteria*2)

#Give an initial position 
for (i in seq(0, bacteria*2)){
  initial_position [1,i] = runif(1, min = -8, max = 8)
}

# Start random walk
for (i in seq(1, steps-1)){
  # Draw a random number 
  St = runif(1, 0, 2)
  
  #Drawing a random delta
  delta = runif(1, min = 0, max = 2*pi)
  
  #determining the position at point t = 1
  for(j in seq(1,bacteria*2)){
    if(j %% 2 == 0){
      initial_position[i+1,j] = initial_position[i,j] + St*sin(delta)
    }
    else {
      initial_position[i+1,j] = initial_position[i,j] + St*cos(delta)
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
for (i in seq(from = 1, to = bacteria)){
  for (j in seq(from = 1, to = steps)){
    name_matrix[x,] = paste("Bacteria ", i)
    x = x+1
  }
}
positions[,1] = name_matrix[,1]

df = subset(positions, select = -c(key.1) )
colnames(df) <- c("Names", "x_values","y_values")
df <- df %<% group_by(Names)

# Let's make a nice graph...
ggplot(df, aes(x = x_values, y = y_values)) +
  geom_line(size = 1, colour = "blue") +
  labs(title = 'Bacteria Mobility',
       x = 'X Position', 
       y = 'Y Position', )
