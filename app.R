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
source ("helper.R")
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
                    checkboxInput("viewCreditChb", label = "Detail Credit Calculation", value = FALSE),
                    selectInput("creditsSelect", label = "Select Credit:", choices = list()),
                    dataTableOutput("CreditTbl"),
                    column(3, checkboxInput("displaySummaryGraph", label = "Display Graph", value = FALSE)),
                    plotOutput("summaryGraph")
                  ),
                  hr(),
                  fluidRow(
                    h4("Your Income"),
                    checkboxInput("displayIncomeChb", label="Display Income Table", value = TRUE),
                    dataTableOutput("totalIncomeTbl"),
                    checkboxInput("displayTotalIncGraph",label = "Display Graph", value = FALSE),
                    plotOutput("totalIncGraph")
                  ), # End Income section
                  hr(),
                  fluidRow(
                    h4("Your Deductions Above AGI"),
                    checkboxInput("displayDeductionChb", label="Display Deduction Table", value = TRUE),
                    dataTableOutput("totalDeductionTbl"),
                    checkboxInput("displayDeductionGraph", label = "Display Graph", value = FALSE),
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
                 # p(strong("Source code:"), a(href = "https://github.com/datasciencecpa/R_TaxProject", "R_TaxProject on github")),
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
  credits <- callModule(credits, "credits", session = session)
  
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
    if (!input$taxSummary){
      hide ("taxSummaryTbl")
    } else {
      show("taxSummaryTbl")
    }
    if (!input$viewCreditChb) {
      hide ("CreditTbl")
      hide ("creditsSelect")
    }
    else{
      show ("CreditTbl")
      show ("creditsSelect")
    } 
    if(!input$displaySummaryGraph) hide("summaryGraph")
    else  show("summaryGraph")
  })
  #-------------------------------------------------------------------
  output$taxSummaryTbl <- renderDataTable({
      #------------------------------------------------------------------+
      hide("viewCreditChb") # Hide this until some credit are available.
      # Step 1: Get income by calling function totalIncomeCalculation
      totalIncome <- totalIncomeCalculation(income())
      deductionsToAGI <- totalDeductionToAGI (deductions(), statusInformation(),totalIncome)

      # Step 2: Plot totalIncome out to graph
      valueRow <- totalIncome$AGI_2018 !=0 | totalIncome$AGI_2017 !=0  # Interested in non-zero value income type.
      totalIncome <- totalIncome[valueRow,]
      output$totalIncomeTbl <- renderDataTable(totalIncome, options = list(pageLength = 25))
      output$totalIncGraph <- renderPlot({
        Income_Type = rownames(totalIncome)
        AGIIncome <- data.frame(Income_Type, totalIncome, row.names = NULL)
        ggplot(data = DFConverter(AGIIncome), aes(x= TaxYear, y = Amount, fill = Income_Type)) +
          geom_bar(stat = "identity")
      })
      #------------------------------------------------------------------
      # Step 2: Calculate deductions above AGI
      
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
      filingStatus <- as.character(toupper(statusInformation()["Filing_Status",]))
      summary_2018 <- c(AGI[1],deductionSummary["Your_Deduction","Below_AGI_Deduction_2018"],
                deductionSummary["Exemption_Deduction", "Below_AGI_Deduction_2018"])
      summary_2017 <- c(AGI[2],deductionSummary["Your_Deduction", "Below_AGI_Deduction_2017"],
                deductionSummary["Exemption_Deduction", "Below_AGI_Deduction_2017"])
      rowNames <- c("AGI", "Deduction", "Exemption" )
      summaryDF <- data.frame(summary_2018, summary_2017, row.names = rowNames)
      summaryDF["Taxable_Income",] <- summaryDF[1,] - apply(summaryDF[2:3,], 2, sum)
      summaryDF["Taxable_Income",1] <- ifelse(summaryDF["Taxable_Income",1]<0,0, summaryDF["Taxable_Income",1])
      summaryDF["Taxable_Income",2] <- ifelse(summaryDF["Taxable_Income",2]<0,0, summaryDF["Taxable_Income",2])
      
      # Step 4 --- Calculate Tax Amount
      tax <- taxCalculation (as.numeric(summaryDF["Taxable_Income",]), income(), filingStatus)
      summaryDF["Tax_Amount",] <- tax
      print (summaryDF)
      
      # Step 5 - Calculate Credits if applicable.
      taxCredits <- creditCalculation(summaryDF, income(), filingStatus, credits()) 
      creditNames <- names(taxCredits)
      print (paste("Credit Names: ", creditNames))
      creditLogical <- sapply(taxCredits, is.data.frame)
      creditNames <- creditNames[creditLogical]
      print (paste("Credit Names with Logical filter: ", creditNames))
      if (sum(creditLogical)>0){
        #------- Step 1: Show Credit Table checkbox and update selectInput with choices = creditNames[Logical]
        show("viewCreditChb")
        updateSelectInput(session,inputId = "creditsSelect",label = "Select Credit:", choices = creditNames, selected = creditNames[1])
        output$CreditTbl <- renderDataTable({
          creditSelected <- input$creditsSelect
          print (paste("Credit Selected:", creditSelected))
          return (taxCredits[[creditSelected]])
        }, options = list(pageLength = 25))
        # --------- Determine which credit is available through list of credit logical ------------
        if (is.data.frame(taxCredits[["CDC"]])) { # CDC is dataframe, CDC credit available
          if (length(colnames(taxCredits[["CDC"]])) == 2){
            # print ("Two year credit")
            summaryDF["Child_Dependent_Care_Credit", ] <- taxCredits[["CDC"]]["Child_Dependent_Care_Credit",]
          } else {
            # print("One year credit")
            taxYear <- colnames(taxCredits[["CDC"]])
            if (taxYear =="2018"){
              summaryDF["Child_Dependent_Care_Credit",1] <- taxCredits[["CDC"]]["Child_Dependent_Care_Credit",]
            }else {
              summaryDF["Child_Dependent_Care_Credit",2] <- taxCredits[["CDC"]]["Child_Dependent_Care_Credit",]
            }
          }
        }
        if(is.data.frame(taxCredits[["Education"]])){
          if (length(colnames(taxCredits[["Education"]]))==2){
            # print ("Two year credit")
            summaryDF["Nonrefundable_Educational_Credit", ] <- taxCredits[["Education"]]["Line_19",]
          }else {
            # print("One year credit")
            taxYear <- colnames(taxCredits[["Education"]])
            if (taxYear=="2018"){
              summaryDF["Nonrefundable_Education_Credit",1] <- taxCredits[["Education"]]["Line_19",]
            }
            else {
              summaryDF["Nonrefundable_Education_Credit",2] <- taxCredits[["Education"]]["Line_19",]
            }
          }
        }
        # 
      }
      
      summaryDF[is.na.data.frame(summaryDF)] <- 0
      return (summaryDF)
  })
  
  
 
  #---------------------------------------------------------------------------

}

shinyApp(ui, server)