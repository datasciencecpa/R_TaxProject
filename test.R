library(shiny)

ui <- fluidPage(
  radioButtons(inputId = "filingStatus_2018", label= "1. Please select your filing status below:",
               choices = c("Single", "Married Filing Jointly", "Married Filing Separately", "Head of Household", "Qualified Widower"), 
               selected = "Single", inline = FALSE),
  textOutput(outputId = "radioChoice")
)

server <- function(input, output, session) {
  output$radioChoice <- renderText ({
    input$filingStatus_2018
  })
}

shinyApp(ui, server)