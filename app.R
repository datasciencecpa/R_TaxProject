# Project: Income Tax Project
# Author: Long Nguyen
# Date: 01/19/2019
library(shiny)
library(shinyjs) #loading addional package to enable more UI experience
library(DT)
source("filingStatus.R")
source("income.R")
source("deductions.R")
source("credits.R")
source("results.R")
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Federal Income Tax 2018 & 2017"),
  navbarPage("Navigation bar",
    tabPanel("Home",
       navlistPanel("Enter Your Information:",
         filingStatusUI("filingStatus"),
         incomeUI("income"),
         deductionsUI("deductions"),
         creditsUI("credits"),
         resultsUI("results")
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
                 dataTableOutput("FS_Summary")),
        tabPanel("Income Summary",
                 dataTableOutput("Income_Summary"))
      )
    )       
  )
)

server <- function(input, output, session) {
  filingStatus <- callModule(filingStatus, "filingStatus", session = session)
  output$FS_Summary <- renderDataTable({
    filingStatus()
  })
  income <- callModule(income,"income", session = session)
  output$Income_Summary <- renderDataTable({
    income()
  })
}

shinyApp(ui, server)