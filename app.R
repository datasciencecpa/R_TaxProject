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
source("Instructions.R")
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Federal Income Tax 2018 & 2017 Analysis"),
  navbarPage("Navigation bar",
    tabPanel("Home",
       navlistPanel("Enter Your Information:",
         instructionUI("instruction"),
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
        tabPanel("About Me",
                 p("Hi, my name is Long Nguyen. Currently, I am a graduate MSSD student at ", a(href = "http://www.bu.edu/csmet/academic-programs/mssd/", "Boston University."),
                    "This project is part my self-learning R-Shiny and Data-Science."),
                 p(strong("How this app comes about:"), "I deal with tax every day in my day job. Therefore, my first app would have something to do with the thing that everyone love to hate:Taxxxx!
                   Though, my goal is to make this less painful and fun while exercising my programming skills."),
                 p(strong("Source code:"), a(href = "https://github.com/datasciencecpa/R_TaxProject", "R_TaxProject on github"))
                 
        ),
        tabPanel("Contact me", h4("Email:  nguyenhlongvn@gmail.com"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Render dataTable for Information Summary Tabs below
  filingStatus <- callModule(filingStatus, "filingStatus", session = session)
  income <- callModule(income,"income", session = session)
  instructions <- callModule(instruction, "instruction", session = session)
  output$FS_Summary <- renderDataTable(filingStatus(), options= list(pageLength = 25), filter = "top")
  
  output$Income_Summary <- renderDataTable({
    income()
  })
  # Reading data from TaxRates.xls
  taxBrakets <- read.xls(xls = "TaxRates.xls", sheet = 1)
  ltCapGains <- read.xls(xls = "TaxRates.xls", sheet = 2)
  perExemptions <- read.xls(xls = "TaxRates.xls", sheet = 3)
  stdDeductions <- read.xls(xls = "TaxRates.xls", sheet = 4)
  # Render dataTable for Tax Tables below
  output$TaxBracket <- renderDataTable(taxBrakets, options = list(pageLength = 20), filter = "top")
  output$LTCapGain <- renderDataTable(ltCapGains, options = list(pageLength= 25), filter = "top")
  output$PerExemption <- renderDataTable(perExemptions, options = list(pageLength= 10))
  output$StdDeductions <- renderDataTable (stdDeductions, options = list(pageLength = 10), filter = "top")
  
  #Display image
  output$myImage <- renderImage(list(src = "img/img1.JPG"))
}

shinyApp(ui, server)