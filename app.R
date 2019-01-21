# Project: Income Tax Project
# Author: Long Nguyen
# Date: 01/19/2019
library(shiny)
library(shinyjs)    #loading addional package to enable more UI experience
library(DT)
library(gdata)      # Use to read Excel file TaxRates.xls
source("filingStatus.R")
source("income.R")
source("deductions.R")
source("credits.R")
source("results.R")
#source("Introduction.R")
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Federal Income Tax 2018 & 2017 Analysis"),
  navbarPage("Navigation bar",
    tabPanel("Home",
       navlistPanel("Enter Your Information:",
         #introductionUI("introduction"),
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
    ),
    tabPanel("Tax Tables",
      navlistPanel("Tax Tables:",
        tabPanel("Tax Bracket:",
                 dataTableOutput("TaxBracket")),
        tabPanel("Long Term Capital Gain:",
                 dataTableOutput("LTCapGain")),
        tabPanel("Personal Exemption - 2017",
                 dataTableOutput("PerExemption")),
        tabPanel("Standard Deductions:",
                 dataTableOutput("StdDeductions"))
      )       
    ),
    tabPanel("Contact Us",
      navlistPanel("Contact & About Me Information:",
        tabPanel("About Me"),
        tabPanel("Contact me")
      )
    )
  )
)

server <- function(input, output, session) {
  # Render dataTable for Information Summary Tabs below
  filingStatus <- callModule(filingStatus, "filingStatus", session = session)
  output$FS_Summary <- renderDataTable({
    filingStatus()
  })
  income <- callModule(income,"income", session = session)
  output$Income_Summary <- renderDataTable({
    income()
  })
  # Reading data from TaxRates.xls
  taxBrakets <- read.xls(xls = "TaxRates.xls", sheet = 1)
  ltCapGains <- read.xls(xls = "TaxRates.xls", sheet = 2)
  perExemptions <- read.xls(xls = "TaxRates.xls", sheet = 3)
  stdDeductions <- read.xls(xls = "TaxRates.xls", sheet = 4)
  # Render dataTable for Tax Tables below
  output$TaxBracket <- renderDataTable({
    taxBrakets
  })
  output$LTCapGain <- renderDataTable({
    ltCapGains
  })
  output$PerExemption <- renderDataTable({
    perExemptions
  })
  output$StdDeductions <- renderDataTable({
    stdDeductions
  })
}

shinyApp(ui, server)