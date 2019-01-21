# Project: Income Tax Project
# Author: Long Nguyen
# Date: 01/19/2019
library(shiny)
library(shinyjs) #loading addional package to enable more UI experience
library(DT)
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Federal Income Tax 2018 & 2017"),
  navbarPage("Navigation bar",
    tabPanel("Home",
       navlistPanel("Enter Your Information:",
         filingStatusUI("filingStatus"),
         tabPanel("Income"),
         tabPanel("Deductions"),
         tabPanel("Credits"),
         tabPanel("Result")
       )
    ),
    tabPanel("Help", 
        navlistPanel("General Questions",
          tabPanel("Filing Status",
                   "1.IRS Interactive Tax Assistant: ", a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?")
          )
        )         
    ),
    tabPanel("Information Summary",
      navlistPanel("Information you Enter:",
        tabPanel("Filing Status & Dependent Infor You Entered:",
                 dataTableOutput("FS_Summary"))                     
      )
    )       
             
  )
  
)

server <- function(input, output, session) {
  filingStatus <- callModule(filingStatus, "filingStatus")
  output$FS_Summary <- renderDataTable({
    filingStatus()
  })
}

shinyApp(ui, server)