# Project: Income Tax Project
# Author: Long Nguyen
# Date: 01/19/2019
library(shiny)

ui <- fluidPage(
  titlePanel("Federal Income Tax 2018"),
  navbarPage("Navigation bar",
    tabPanel("Home",
       navlistPanel("Step_1",
         tabPanel("Filing Status & Dependency",
            fluidRow(
              column(5, h3("2018"), 
                     radioButtons(inputId = "filingStatus_2018", label= "1. Please select your filing status below:",
                                  choices = c("Single", "Married Filing Jointly", "Married Filing Separately", "Head of Household", "Qualified Widower"), 
                                  selected = "Single", inline = FALSE),
                     "Need help? Visit IRS Interactive Tax Assistant: ",
                     a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?")
              ),
              column(5, h3("2017"), 
                     radioButtons(inputId = "filingStatus_2017", label= "1. Please select your filing status below:",
                                  choices = c("Single", "Married Filing Jointly", "Married Filing Separately", "Head of Household", "Qualified Widower"), 
                                  selected = "Single", inline = FALSE),
                     "Need help? Visit IRS Interactive Tax Assistant: ",
                     a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?")
              )
            ), # End first row of filing status
            hr(),
            fluidRow(
              column(5, 
                 radioButtons(inputId = "isDependent_2018", label = "Can someone claim you as a dependent on their tax return?",
                              choices = c("Yes", "No"), selected = "No") 
              ), 
              column(5, 
                     radioButtons(inputId = "isDependent_2017", label = "Can someone claim you as a dependent on their tax return?",
                                  choices = c("Yes", "No"), selected = "No") 
              ) 
            ), # End second row of checking dependency on other people return
            hr(),
            fluidRow(
              column(5, 
                     numericInput(inputId = "_2018", label = "Can someone claim you as a dependent on their tax return?",
                                  choices = c("Yes", "No"), selected = "No") 
              ), 
              column(5, 
                     radioButtons(inputId = "isDependent_2017", label = "Can someone claim you as a dependent on their tax return?",
                                  choices = c("Yes", "No"), selected = "No") 
              ) 
            )
         ),
         tabPanel("Income"),
         tabPanel("Deductions"),
         tabPanel("Credits"),
         tabPanel("Result")
       )
    ),
    tabPanel("Help", 
        navlistPanel("General Questions",
          tabPanel("Filing Status",
                   "1.IRS Interactive Tax Assistant: ", a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?"
          )
        )
             
             
    )
    #tabPanel("About",
      
    )       
             
             
  )
  
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)