
library(shiny)
library(shinyalert)
library(shinythemes)

source(file = "shiny_app_code.R", local = T)

# Define UI for application
ui <- fluidPage(theme = shinytheme("slate"),
  
  # Application title
  titlePanel(h4("Bacteria Mobility App")),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("bacteria_button", "Number of Bacteria:", value = 5),
      checkboxInput("move_button", "Bacteria keep moving inside the sugar", value = T),
      numericInput("c_button", "K constant C", value = 0),
      numericInput("seed_choice_button", "Simulation seed", value = 12345),
      actionButton("compute", "Confirm Values", icon = icon("calculator"), width = 200, height = 100),
      sliderInput("stages", "Run animation of simulation", min = 0, max = 100, value = 0, animate = T),
      actionButton("grade", "What grade should I give them ?")
    ),
    
    mainPanel(
      plotOutput("bacteriaPlot")
    )    
  )
)

# Define server
server <- function(input, output) {
  
  data_bact <- eventReactive(input$compute,{
    bacteria_data(
      input$bacteria_button,
      input$move_button,
      input$c_button,
      input$seed_choice_button)
  })
  
  output$bacteriaPlot <- renderPlot({ 
    bacteria_plot(
      positions = data_bact(),
      stages = input$stages)
  }, height = 600, width = 700)

  observeEvent(input$grade,{
    shinyalert(
      title = "THIS DESERVES A 6 !!!",
      text = "Please come on, we love you <3")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
