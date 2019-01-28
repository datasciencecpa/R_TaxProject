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

source("creditCalculation.R")
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Federal Income Tax 2018 & 2017 Analysis"),
  navbarPage(tags$img(src = "navImg.jpg", class = "img-responsive", width = "50", height = "60"),
    tabPanel("Home",
       navlistPanel("Enter Your Information:",
         instructionUI("instruction"),
         filingInformationUI("filingInformation"),
         incomeUI("income"),
         deductionsUI("deductions"),
         creditsUI("credits"),
         resultsUI("results"), 
         tabPanel("Testing", dataTableOutput("testing"))
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
                 dataTableOutput("StdDeductions")),
        tabPanel("Child Tax Credit:",
                 dataTableOutput("ChildTaxCrd"))
      )       
    ),
    tabPanel("Contact Us",
      navlistPanel("Contact & About Me Information:",
        tabPanel("About Me",
                 p("Hi, my name is Long Nguyen. Currently, I am a graduate MSSD student at ", a(href = "http://www.bu.edu/csmet/academic-programs/mssd/", "Boston University."),
                    "This project is part my self-learning R-Shiny and Data-Science."),
                 p(strong("How this app comes about:"), "I deal with tax every day in my day job. Therefore, my first app would have something to do with the thing that everyone love to hate:Taxxxx!
                   Though, my goal is to make this less painful and fun while exercising my programming skills."),
                 p(strong("Source code:"), a(href = "https://github.com/datasciencecpa/R_TaxProject", "R_TaxProject on github")),
                 h5("This is me and my wife - Hanna!"),
                 tags$img(src = "img1.JPG",height = "1000", width = "700")
                 
        ),
        tabPanel("Contact me", h4("Email:  nguyenhlongvn@gmail.com"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Render dataTable for Information Summary Tabs below
  statusInformation <- callModule(filingInformation, "filingInformation", session = session)
  income <- callModule(income,"income", session = session)
  instructions <- callModule(instruction, "instruction", session = session)
  output$FS_Summary <- renderDataTable(statusInformation(), options= list(pageLength = 25), filter = "top")
  
  output$Income_Summary <- renderDataTable(
    income(), options = list(pageLength = 25), filter = 'top'
  )
  # Reading data from TaxRates.xls
  taxBrakets <- read.xls(xls = "TaxRates.xls", sheet = 1, as.is = TRUE)
  ltCapGains <- read.xls(xls = "TaxRates.xls", sheet = 2, as.is = TRUE)
  perExemptions <- read.xls(xls = "TaxRates.xls", sheet = 3, as.is = TRUE)
  stdDeductions <- read.xls(xls = "TaxRates.xls", sheet = 4, as.is = TRUE)
  childTaxCredit <- read.xls(xls = "TaxRates.xls", sheet = 5, as.is = TRUE)
  # Render dataTable for Tax Tables below
  output$TaxBracket <- renderDataTable(taxBrakets, options = list(pageLength = 20), filter = "top" )
  output$LTCapGain <- renderDataTable(ltCapGains, options = list(pageLength= 25), filter = "top")
  output$PerExemption <- renderDataTable(perExemptions, options = list(pageLength= 10))
  output$StdDeductions <- renderDataTable (stdDeductions, options = list(pageLength = 10), filter = "top")
  output$ChildTaxCrd <- renderDataTable(childTaxCredit, filter= "top")
  output$testing <- renderDataTable({
    filingStatus <- statusInformation()["Filing_Status", "Status_2018"]
    numChildUnder17 <- statusInformation()["Qualifying_Child_Under_17", "Status_2018"]
    numQualifyingRel <- statusInformation()["Qualifying_Relative", "Status_2018"]
    yourAGI <- income()["Your_Wages", "income_2018"]
    return (childTaxCrd(yourAGI, filingStatus, 2018, numChildUnder17, childTaxCredit, 8000, numQualifyingRelative = numQualifyingRel))
  }, options = list(pageLength = 15))
}

shinyApp(ui, server)