source(file = "shiny_code_test.R", local = T)


# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel(h4("Buffon\'s needle experiment - Inputs:")),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("bacteria_button", "Number of Bacteria:", value = 1),
      checkboxInput("move_button", "Bacteria keep moving inside the sugar", FALSE),
      numericInput("c_button", "K constant C", value = 0),
      numericInput("seed_choice_button", "Simulation seed", value = 1),
      actionButton("compute1", "Compute Simulation", icon = icon("calculator"), width = 200, height = 100),
      sliderInput("stages", "Run animation of simulation", min = 1, max = 100, value = 1),
      actionButton("compute", "",icon = icon("play"), width = 50, height = 50)
    ),
    
    imageOutput("plot1")
  )
)

# Define server
server <- function(input, output) {
  observeEvent(input$compute1, {
    output$plot <- renderImage({
      outfile <- tempfile(fileext='.gif')
      bacteria_plot(c = input$c_button, seed_choice = input$seed_choice_button, bacteria = input$bacteria_button, move = move_button)
      anim_save("outfile.gif", p)  
      list(src = "outfile.gif",contentType = 'image/gif'
    )}, deleteFile = TRUE)}
)}


# Run the application 
shinyApp(ui = ui, server = server)