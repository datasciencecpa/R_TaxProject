# This module will allow user to report income tax withheld

withholdingTaxUI <- function (id){
  ns <- NS(id)
  tabPanel("Report withholding taxes",
    fluidRow(
      column(5, h3("2018")),
      column(5, h3("2017"))
    ),
    hr(),
    fluidRow(
      column(5, numericInput(ns("yourW2Tax_2018"), label = "Report your withholding tax from W-2", value = 0, min=0, max=999999)),
      column(5, numericInput(ns("yourW2Tax_2017"), label = "Report your withholding tax from W-2", value = 0, min=0, max=999999))
    )
  )
}
withholding <- function (input, output, session){
  
}
  
