creditsUI <- function (id){
  ns <- NS(id)
  tabPanel("Credits",
     fluidRow(
       column(5, h3("2018")),
       column(5, h3("2017"))
     ), 
     hr()
  )
}
