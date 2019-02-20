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
                    h4("Your Tax Summary"),
                    checkboxInput("taxSummary", label = "Your Tax Summary", value = TRUE),
                    dataTableOutput("taxSummaryTbl"),
                    column(3, checkboxInput("displaySummaryGraph", label = "Display Graph", value = TRUE)),
                    plotOutput("summaryGraph")
                  ),
                  hr(),
                  fluidRow(
                    h4("Your Income"),
                    checkboxInput("displayIncomeChb", label="Display Income Table", value = TRUE),
                    dataTableOutput("totalIncomeTbl"),
                    checkboxInput("displayTotalIncGraph",label = "Display Graph", value = TRUE),
                    plotOutput("totalIncGraph")
                  ), # End Income section
                  hr(),
                  fluidRow(
                    h4("Your Deductions Above AGI"),
                    checkboxInput("displayDeductionChb", label="Display Deduction Table", value = TRUE),
                    dataTableOutput("totalDeductionTbl"),
                    checkboxInput("displayDeductionGraph", label = "Display Graph", value = TRUE),
                    plotOutput("deductionGraph")
                  ), # End Deduction section
                  hr(),
                  fluidRow(
                    h4("Your Standard Deduction or Itemized Deduction"),
                    checkboxInput("displayDeductionBlAGI", label = "Display Deduction Table", value = TRUE),
                    checkboxInput("displayItemizedChb", label="Display Itemized Table", value = FALSE),
                    dataTableOutput("belowAGIDeductionTbl"),
                    dataTableOutput("detailItemizedTbl")
                  ) # End Itemized section
         )
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
  # Display user entered information to information summary tab
  output$FS_Summary <- renderDataTable(statusInformation(), options= list(pageLength = 25), filter = "top")
  output$Income_Summary <- renderDataTable(
    income(), options = list(pageLength = 25), filter = 'top'
  )
  output$Deduction_Summary <- renderDataTable(
    deductions(), options = list(pageLength = 25), filter="top"
  )
  
  # -- Observe when user check on display graph---
  observe({
    if (!input$displayTotalIncGraph){ # Display total income graph
      hide("totalIncGraph")
    } else show ("totalIncGraph")
    if (!input$displayIncomeChb) {  # Display Income Table
      hide ("totalIncomeTbl")  
    } else {
      show ("totalIncomeTbl")
    }
    if (!input$displayDeductionGraph){ # Display above AGI Deduction graph
      hide ("deductionGraph")
    } else show ("deductionGraph")
    if (!input$displayDeductionChb) { # Display above AGI Deduction table
      hide ("totalDeductionTbl")
    } else show ("totalDeductionTbl")
    if (!input$displayDeductionBlAGI){ # Display below AGI Deduction talbe
      hide ("belowAGIDeductionTbl")
    } else show ("belowAGIDeductionTbl")
    if (!input$displayItemizedChb) { # Display detailed itemized deduction
      hide ("detailItemizedTbl")
    } else show ("detailItemizedTbl")
  })
  #-------------------------------------------------------------------
  output$taxSummaryTbl <- renderDataTable({
      #------------------------------------------------------------------
      # Step 1: Get income by calling function totalIncomeCalculation
      totalIncome <- totalIncomeCalculation(income())
      valueRow <- totalIncome$AGI_2018 !=0 | totalIncome$AGI_2017 !=0  # Interested in non-zero value income type.
      AGIIncome <- totalIncome[valueRow,]
      # Step 2: Plot AGIIncome out to graph
      output$totalIncomeTbl <- renderDataTable(AGIIncome, options = list(pageLength = 25))
      output$totalIncGraph <- renderPlot({
        ggplot(data = DFConverter(AGIIncome), aes(x= TaxYear, y = Amount, fill = Income_Type)) +
          geom_bar(stat = "identity")
      })
      
      #------------------------------------------------------------------
      # Step 2: Calculate deductions above AGI
      deductionsToAGI <- totalDeductionToAGI (deductions(), statusInformation(),totalIncome)
      AGI <- as.numeric(deductionsToAGI["Adjusted_Gross_Income",]) # Getting adjusted gross income for 2 years
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
      output$totalDeductionTbl <- renderDataTable(deductionsToAGI, options = list(pageLength = 25))
      #----------------------------------------------------------------------- End deductions above AGI
      # Step 3: Calculate below AGI deductions: Standard Deductions or Itemized Deductions and Exemption
      itemizedItems <- c("Medical_Exp","State_Local_Taxes", "Real_Estate_Taxes","Personal_Property_Tax",
                         "Mortgage_Interest","Premium_Mortage_Interest","Charitable_Contribution")
      result <- belowAGIDeduction (deductions()[itemizedItems,], statusInformation(),AGI)
      deductionSummary <- result [[1]]
      detailItemized <- result[[2]]
      output$detailItemizedTbl <- renderDataTable(detailItemized,options= list(pageLength = 25))
      output$belowAGIDeductionTbl <- renderDataTable(deductionSummary)
      
      # 2018 <- c(AGI[1],deductionSummary["Your_Deduction", 1], 
      #           deductionSummary["Exemption_Deduction", 1])
      # 2017 <- c(AGI[2],deductionSummary["Your_Deduction", 1],
      #           deductionSummary["Exemption_Deduction", 1])
      # rowNames <- c("AGI", "Deduction", "Exemption" )
      # return (data.frame(2018, 2017, row.names = rowNames))
  })
  
  
 
  #---------------------------------------------------------------------------

}

shinyApp(ui, server)