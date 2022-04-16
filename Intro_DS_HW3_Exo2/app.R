
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(gganimate)
library(directlabels)
library(circular)
library(units)

options(warn = - 1)  

bacteria_plot <- function(c,seed_choice,bacteria,move){
# Number of steps
steps <- 100

set.seed(seed_choice)

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
    if (norm <= 3 && move == 1){
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

if (move == 0){
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

# Implementation of animation 
p <- ggplot(data = positions, aes(x = y, y = x, color = name, group = name)) +
  geom_polygon(aes(x = x1, y = y1),fill = "tan2", linetype = 1, colour = "black", data = df, inherit.aes = F) +
  annotate("text", x = 0, y = 0, label = "SUGAR", col = "white", size = 6) +
  geom_point(pch = 11) +
  geom_line(alpha = 0.4) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20)) +
  theme(axis.title.x = element_text(size = 14, face="bold"),
        axis.title.y = element_text(size=14, face="bold")) +
  scale_x_continuous(name = 'X Position', breaks = seq(-10,10, by = 5)) +
  scale_y_continuous(name = 'Y Position', breaks = seq(-10,10, by = 5)) +
  transition_reveal(time) +
  labs(title = 'Bacteria Mobility') +
  coord_cartesian(ylim = c(-10,10), xlim = c(-12,12))
}




#--------------------------------------------------------------------------------------------



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bacteria Mobility App"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("bacteria_button", "Number of Bacteria:", value = 1),
            checkboxInput("move_button", "Bacteria keep moving inside the sugar"),
            numericInput("c_button", "K constant C", value = 0),
            numericInput("seed_choice_button", "Simulation seed", value = 1),
            actionButton("compute", "Compute Simulation", icon = icon("calculator"), width = 200, height = 100),
            sliderInput("stages", "Run animation of simulation", min = 1, max = 100, value = 1),
            actionButton("compute", "",icon = icon("play"), width = 100, height = 100)
        ),
          ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot"),
           
        )
  )

# Define server logic required to draw a histogram
server <- function(input, output, session) {


  
    output$plot <- renderPlot({
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
