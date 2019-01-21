deductionsUI <- function (id){
  ns <- NS(id)
  tabPanel("Deductions",
     fluidRow(
       column(5, h3("2018")),
       column(5, h3("2017"))
     ), 
     hr()
  )
}
deduction <- function (input, output, sesson){
  
}