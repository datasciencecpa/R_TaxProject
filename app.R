# Project: Income Tax Project
# Author: Long Nguyen
# Date: 01/19/2019
library (ggplot2)
library (shiny)
library (shinyjs)    #loading addional package to enable more UI experience
library (DT)
library (gdata)      # Use to read Excel file TaxRates.xls
source ("filingStatus.R")
source ("income.R")
source ("deductions.R")
source ("credits.R")
source ("Instructions.R")
#source ("creditCalculation.R")
source ("incometaxCalculation.R")
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
         tabPanel("Results",
                  fluidRow(
                    h4("Your Income"),
                    column(3,checkboxInput("displayIncomeChb", label="Display Income Table", value = TRUE)),
                    dataTableOutput("totalIncomeTbl"),
                    column(3,checkboxInput("displayTotalIncGraph",label = "Display Graph", value = TRUE)),
                    column(9,plotOutput("totalIncGraph"))
                  ), # End Income section
                  hr(),
                  fluidRow(
                    h4("Your Deductions Above AGI"),
                    column(3,checkboxInput("displayDeductionChb", label="Display Deduction Table", value = TRUE)),
                    dataTableOutput("totalDeductionTbl"),
                    column(3, checkboxInput("displayDeductionGraph", label = "Display Graph", value = TRUE)),
                    column(9, plotOutput("deductionGraph"))
                  ), # End Deduction section
                  hr(),
                  fluidRow(
                    h4("Your Standard Deduction or Itemized Deduction"),
                    column(3, checkboxInput("displayItemizedChb", label = "Display Deduction Table", value = TRUE)),
                    dataTableOutput("totalItemizedTbl"),
                    column(3, checkboxInput("displayTotalItemGraph", label = "Display Graph", value = TRUE)),
                    column(9, plotOutput("itemizedGraph"))
                  ), # End Itemized section
                  hr(),
                  fluidRow(
                    h4("Your Tax and Credits")
                  )
         ),
         uiOutput("resultTab"),
         tabPanel("Testing Child Tax Credit", dataTableOutput("testingCTC")),
         tabPanel("Testing Child and Dependent Care Exp", dataTableOutput("testingCDC"))
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
                 dataTableOutput("Income_Summary")),
        tabPanel("Deduction Summary",
                 dataTableOutput("Deduction_Summary"))
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
  # get statusInformation, incomeInformation, and deductions information entered by user.

  statusInformation <- callModule(filingInformation, "filingInformation", session = session)
  income <- callModule(income,"income", session = session)
  deductions <- callModule(deductions, "deductions", session = session)
  #--------------------------------------------------------------------------------
  
  
  
  #--------------------------------------------------------------------------------
  # Display user entered information to information summary tab
  output$FS_Summary <- renderDataTable(statusInformation(), options= list(pageLength = 25), filter = "top")
  output$Income_Summary <- renderDataTable(
    income(), options = list(pageLength = 25), filter = 'top'
  )
  output$Deduction_Summary <- renderDataTable(
    deductions(), options = list(pageLength = 25), filter="top"
  )
  #--------------------------------------------------------------------------------
  # First step - Reading data from TaxRates.xls
  taxBraketsTbl <- read.xls(xls = "TaxRates.xls", sheet = 1, as.is = TRUE)
  ltCapGainsTbl <- read.xls(xls = "TaxRates.xls", sheet = 2, as.is = TRUE)
  perExemptionsTbl <- read.xls(xls = "TaxRates.xls", sheet = 3, as.is = TRUE)
  stdDeductionsTbl <- read.xls(xls = "TaxRates.xls", sheet = 4, as.is = TRUE)
  childTaxCreditTbl <- read.xls(xls = "TaxRates.xls", sheet = 5, as.is = TRUE)
  childDepExpTbl <- read.xls(xls = "TaxRates.xls", sheet=6, as.is = TRUE)
  AOCTbl <- read.xls(xls = "TaxRates.xls", sheet = 7, as.is = TRUE)
  LLTbl <- read.xls(xls = "TaxRates.xls", sheet = 8, as.is = TRUE)
  # *********************************************************************************
  # Render dataTable for Tax Tables below
  output$TaxBracket <- renderDataTable(taxBraketsTbl, options = list(pageLength = 20), filter = "top" )
  output$LTCapGain <- renderDataTable(ltCapGainsTbl, options = list(pageLength= 25), filter = "top")
  output$PerExemption <- renderDataTable(perExemptionsTbl, options = list(pageLength= 10))
  output$StdDeductions <- renderDataTable (stdDeductionsTbl, options = list(pageLength = 10), filter = "top")
  output$ChildTaxCrd <- renderDataTable(childTaxCreditTbl, filter= "top")
  # -- End Render Tax Tables -----------------------------------------
  
  
  # -- Observe when user check on display graph---
  observe({
    if (!input$displayTotalIncGraph){
      hide("totalIncGraph")
    } else show ("totalIncGraph")
    if (!input$displayIncomeChb) { 
      hide ("totalIncomeTbl")  
    } else {
      show ("totalIncomeTbl")
    }
    if (!input$displayDeductionGraph){
      hide ("deductionGraph")
    } else show ("deductionGraph")
    if (!input$displayDeductionChb) {
      hide ("totalDeductionTbl")
    } else show ("totalDeductionTbl")
  })
  #-------------------------------------------------------------------

  output$totalIncomeTbl <- renderDataTable({
    AGIIncome <- totalIncomeCalculation(income())
    valueRow <- AGIIncome$AGI_2018 !=0 | AGIIncome$AGI_2017 !=0  # Interested in non-zero value income type.
    AGIIncome <- AGIIncome[valueRow,]
    output$totalIncGraph <- renderPlot({
      
      ggplot(data = DFConverter(AGIIncome), aes(x= TaxYear, y = Amount, fill = Income_Type)) +
        geom_bar(stat = "identity")
    })
    return (AGIIncome)
  })
  #-----------------------------------------------------------------------
  output$totalDeductionTbl <- renderDataTable({
    deductionsToAGI <- totalDeductionToAGI (deductions(), statusInformation(),totalIncomeCalculation(income()))
    AGI <- deductionsToAGI["Adjusted_Gross_Income",]
    deductionDF <- deductionsToAGI[c("Educator_Expense", "HSA_Deduction_Amt", "Your_IRA_Deduction", "Your_Spouse_IRA_Deduction",
                                    "Student_Loan_Deduction"), ]
    valueRow <- (deductionDF$Deduction_2018 !=0) | (deductionDF$Deduction_2017 !=0)
    
    deductionDF <- deductionDF[valueRow,]
    Deduction_Type <- rownames(deductionDF)
    deductionDF <- data.frame(Deduction_Type, deductionDF)
    rownames(deductionDF) <- NULL
    output$deductionGraph <- renderPlot({
      ggplot(data= DFConverter(deductionDF), aes(x = TaxYear, y= Amount, fill = Deduction_Type)) + geom_bar(stat = "identity")
    })
    return (deductionsToAGI)
  },options = list(pageLength = 25))
  #---------------------------------------------------------------------------
  output$totalItemizedTbl <- renderDataTable({
    itemizedItems <- c("Medical_Exp","State_Local_Taxes", "Real_Estate_Taxes","Personal_Property_Tax",
                       "Mortgage_Interest","Premium_Mortage_Interest","Charitable_Contribution")
    deductionsToAGI <- totalDeductionToAGI (deductions(), statusInformation(),totalIncomeCalculation(income()))
    print(paste("AGI Amount:", deductionsToAGI["Adjusted_Gross_Income",]))
    itemizeDF <- totalItemizedDeduction (deductions()[itemizedItems,], statusInformation(),deductionsToAGI["Adjusted_Gross_Income",] )
  }, options= list(pageLength = 25))
  # Testing Code Below ----------------------------------------------
  output$testingCTC<- renderDataTable({
    filingStatus <- statusInformation()["Filing_Status", "Status_2018"]
    numChildUnder17 <- statusInformation()["Qualifying_Child_Under_17", "Status_2018"]
    numQualifyingRel <- statusInformation()["Qualifying_Relative", "Status_2018"]
    yourAGI <- income()["Your_Wages", "Income_Tax_2018"]
    return (childTaxCrd(yourAGI, filingStatus, 2018, numChildUnder17, childTaxCreditTbl, 8000, numQualifyingRelative = numQualifyingRel))
  }, options = list(pageLength = 15))
  output$testingCDC <- renderDataTable({
    
  })
}

shinyApp(ui, server)