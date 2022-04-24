source(file = "shiny_app_code.R", local = T)

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(h4("Buffon\'s needle experiment - Inputs:")),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("bacteria_button", "Number of Bacteria:", value = 1),
      checkboxInput("move_button", "Bacteria keep moving inside the sugar", value = F),
      numericInput("c_button", "K constant C", value = 0),
      numericInput("seed_choice_button", "Simulation seed", value = 1),
      actionButton("compute", "Useless Button", icon = icon("calculator"), width = 200, height = 100),
      sliderInput("stages", "Run animation of simulation", min = 1, max = 100, value = 1, animate = T)
    ),
    
    mainPanel(
      plotOutput("bacteriaPlot"),
      plotOutput("bacteria_initial")
    )    
  )
)

# Define server
server <- function(input, output) {
  output$bacteriaPlot <- renderPlot({  
    bacteria_plot(
      c = input$c_button, 
      seed_choice = input$seed_choice_button, 
      bacteria = input$bacteria_button, 
      move = input$move_button, 
      stages = input$stages)
  }, height = 500, width = 600)
}

# Run the application 
shinyApp(ui = ui, server = server)