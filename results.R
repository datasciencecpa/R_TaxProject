resultsUI <- function (id){
  ns <- NS(id)
  tabPanel("Results",
     fluidRow(
       h4("Your AGI"),
       dataTableOutput(ns("AGI")),
       column(3,checkboxInput(ns("displayAGIGraph"),label = "Display Graph", value = TRUE)),
       column(9,plotOutput(ns("AGIGraph")))
     ), 
     hr(),
     fluidRow(
       h4("Your Adjustments to Income"),
       dataTableOutput(ns("AdjToIncome")),
       column(3, checkboxInput(ns("displayAdjToIncomeGraph"), label = "Display Graph", value = TRUE)),
       column(9, plotOutput(ns("AdjToIncomeGraph")))
     )
  )
}

