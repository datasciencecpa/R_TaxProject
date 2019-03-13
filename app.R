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
                    fluidRow(column(8,h3("Your Tax Summary")),
                        column(4, checkboxInput("hideTaxSummary", label= "Hide Tax Summary", value = FALSE))
                    ),
                    fluidRow(column(8,checkboxInput("showTaxSummaryTbl", label = "Show Tax Summary Table", value = TRUE)),
                        column(4,checkboxInput("add2017", label = "See Results Under 2017 Rules", value = FALSE))
                    ),
                    dataTableOutput("taxSummaryTbl"),
                    checkboxInput("viewCreditChb", label = "Detail Credit Calculation", value = FALSE),
                    selectInput("creditsSelect", label = "Select Credit:", choices = list()),
                    dataTableOutput("CreditTbl"),
                    checkboxInput("displaySummaryGraph", label = "Display Graph", value = FALSE),
                    plotOutput("summaryGraph"),
                    hr(),
                    fluidRow( column(4,h3("Tax Planning")),
                              column(4, checkboxInput("taxPlanning", label = "Tax Planning", value = FALSE)),
                              column(4, checkboxInput("hideTaxPlanning", label = "Hide Tax Planning", value = TRUE)
                              )
                    ),
                    h4("Change in Filing Status"),
                    selectInput("filingStatus", label = "Select filing status",
                                choices = c("Single", "Married Filing Jointly", "Married Filing Separately", "Qualifying Widower",
                                            "Head of Household"), selected = "SINGLE"),
                    hr(),
                    h4("Change in Dependent"),
                    sliderInput("qualifyingChild", label = "Number of Qualifying Child:", min= 0, max=10, value = 0, 
                                round = TRUE, width = "500px", step=1),
                    sliderInput("qualifyingRelatives", label = "Number of Qualifying Relative", min = 0, max = 10, 
                                value = 0, round = TRUE,step = 1,width = "500px"),
                    hr(),
                    h4("Change in IRA Contribution"),
                    sliderInput("IRAAmountSld", label = "IRA Contribution", min=0, max=6500, value=0, step = 50, 
                                round = TRUE, width = "500px"),
                    hr(),
                    h4("Change in HSA Contribution"),
                    sliderInput("HSAAmountSld", label = "HSA Contribution", min = 0, max = 6850, value = 0, step=50, width = "500px")
                    
                  ),
                  hr(),
                  fluidRow(
                    fluidRow(column(8,h3("Other Details Summary")),
                        column(4, checkboxInput("hideDetailSummary", label = "Hide Detail Summary", value= TRUE))
                    ),
                    selectInput("otherDetailSummary",label = "Select Other Taxes Detail Summary", choices = c("NONE"),selected = "NONE"),
                    dataTableOutput("otherDetailTbl"),
                    checkboxInput("displayOtherDetailGrh", label = "Display Graph", value= FALSE),
                    plotOutput("detailGraph")
                  ) # End Other Detail Section
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

  # get statusInformation, incomeInformation,deductions, and credits information entered by user.
  
  statusInformation <- callModule(filingInformation, "filingInformation", session = session)
  income <- callModule(income,"income", session = session)
  deductions <- callModule(deductions, "deductions", session = session)
  credits <- callModule(credits, "credits", session = session)
  
  #--------------------------------------------------------------------------------
  # Display user entered information to information summary tab
  output$FS_Summary <- renderDataTable(statusInformation(), options= list(pageLength = 25), filter = "top")
  output$Income_Summary <- renderDataTable(income(), options = list(pageLength = 25), filter = 'top')
  output$Deduction_Summary <- renderDataTable(deductions(), options = list(pageLength = 25), filter="top")
  
  # -- Observe when user check on display graph---
  observe({
    if (input$hideTaxSummary){ # Hide Tax Summary  Section
      updateCheckboxInput(session,"showTaxSummaryTbl",value = FALSE )
      updateCheckboxInput(session, "displaySummaryGraph",value = FALSE)
      updateCheckboxInput(session, "viewCreditChb", value= FALSE)
      hideshow(c("showTaxSummaryTbl", "add2017","taxSummaryTbl","viewCreditChb",
                 "displaySummaryGraph"), TRUE)
    } else {
      hideshow(c("showTaxSummaryTbl", "add2017","taxSummaryTbl","viewCreditChb",
                 "displaySummaryGraph"), FALSE)
    } # End Hide Tax Summary Section----------------------------------------------
    
    if (input$hideTaxPlanning){ # Hide Tax Planning
      hideshow(c("filingStatus","qualifyingChild","qualifyingRelatives","IRAAmountSld","HSAAmountSld"), TRUE)
    } 
    else {
      hideshow(c("filingStatus","qualifyingChild","qualifyingRelatives","IRAAmountSld","HSAAmountSld"), FALSE)
    }# End Hide Tax Planning. Uncheck this box will be handled separately within function below.
    if (input$hideDetailSummary){
      updateCheckboxInput(session, "displayOtherDetailGrh",value = FALSE)
      hideshow(c("otherDetailSummary","otherDetailTbl","displayOtherDetailGrh"), TRUE)
    } else {
      hideshow(c("otherDetailSummary","otherDetailTbl","displayOtherDetailGrh"), FALSE)
    }# End hide Other Detail Summary Section.
    if (input$viewCreditChb){# Credit checkbox under tax summary
      show ("creditsSelect")
      show ("CreditTbl")
    } else {
      hide ("creditsSelect")
      hide ("CreditTbl")
    } # End credit checkbox -----------
    if (input$displaySummaryGraph) { #Show summary Graph
      show("summaryGraph")
    } else {
      hide("summaryGraph")
    } # End Summary Graph -----------------------
    
    if (input$displayOtherDetailGrh) {
      show("detailGraph")
    } else {
      hide("detailGraph")
    }
    if (input$showTaxSummaryTbl){
      show("taxSummaryTbl")
    } else {
      hide ("taxSummaryTbl")
    }
  })
  #-------------------------------------------------------------------

  output$taxSummaryTbl <- renderDataTable({
      #------------------------------------------------------------------+
      # Step 1: Calculate TotalIncome, TotalDeductions Above AGI, SD or Itemized Deductions, Exemption, taxes, and credits
      # local variables for this function ------------------------------------
      statusDF <- statusInformation()
      filingStatus <- toupper(statusDF["Filing_Status",1]) # commonly use variable
      incomeDF <- income() # getting dataframe from income.           # commonly use variable
      IRAContribution <- as.numeric(deductions()[c("Your_IRA_Contribution","Spouse_IRA_Contribution"),1])
      taxes2017 <- NULL                # Vector variable used to store taxes 2017
      taxPlanning <- NULL              # Vector variable used to store taxPlanning
   
      taxes2018 <- taxSummary(2018,statusDF, incomeDF, deductions(), credits())
      
      print (taxes2018[[5]])
      print (taxes2018[[6]])
      summaryDF <- data.frame(taxes2018[[1]], row.names = names(taxes2018[[1]]))
      colnames(summaryDF) <- "Tax_2018"
      
      # Finish Step 1- Calculate Taxes_2018 --------------------------------------------------------------------------------
      if (input$add2017){
        # Step 1: Calculate taxes2017
        taxes2017 <- taxSummary(2017,statusDF, incomeDF, deductions(), credits())
        # Step 2: Add to summaryDF
        summaryDF <- cbind(summaryDF, taxes2017[[1]])
        colnames(summaryDF) <- c("Tax_2018", "Tax_2017")
      } else {
        summaryDF <- data.frame(taxes2018[[1]], row.names = names(taxes2018[[1]]))
        colnames(summaryDF) <- "Tax_2018"
      }
      
      if (!input$hideDetailSummary) { # Update selected input
        detailLabel <- "Above_AGI_Deduction_Summary"
        rowValues <- taxes2018[[2]][,1] !=0
        if (sum(rowValues)>0) detailLabel <- append(c("Income_Summary"), detailLabel)
        updateSelectInput(session, "otherDetailSummary", choices= detailLabel )
      }
      output$otherDetailTbl <- renderDataTable({

        if (input$otherDetailSummary == "Income_Summary"){
          rowNames <- rownames(taxes2018[[2]])
          rowValues <- taxes2018[[2]][,1] !=0
          rowNames <- rowNames[rowValues]
          totalIncomeDF <- as.data.frame(taxes2018[[2]][rowValues,1], row.names = rowNames)
          colnames(totalIncomeDF) <- "Tax_2018"
          return (totalIncomeDF)
        } else {  # return Deductions To AGI

          return (taxes2018[[3]])
        }
      },options= list(pageLength = 25))


      #     summaryDF["Taxable_Income",] <- summaryDF[1,] - apply(summaryDF[2:3,], 2, sum)

      #     # Step 5 - Calculate Credits if applicable.
      #     taxCredits <- creditCalculation(summaryDF, incomeDF, filingStatus, credits()) 
      #     creditNames <- names(taxCredits)
      #     print (paste("Credit Names: ", creditNames))
      #     creditLogical <- sapply(taxCredits, is.data.frame)
      #     creditNames <- creditNames[creditLogical]
      #     print (paste("Credit Names with Logical filter: ", creditNames))
      #     if (sum(creditLogical)>0){
      #       #------- Step 1: Show Credit Table checkbox and update selectInput with choices = creditNames[Logical]
      #       show("viewCreditChb")
      #       updateSelectInput(session,inputId = "creditsSelect",label = "Select Credit:", choices = creditNames, selected = creditNames[1])
      #       output$CreditTbl <- renderDataTable({
      #         creditSelected <- input$creditsSelect
      #         print (paste("Credit Selected:", creditSelected))
      #         return (taxCredits[[creditSelected]])
      #       }, options = list(pageLength = 25))
      #       # --------- Determine which credit is available through list of credit logical ------------
      #       if (is.data.frame(taxCredits[["CDC"]])) { # CDC is dataframe, CDC credit available
      #         if (length(colnames(taxCredits[["CDC"]])) == 2){
      #           # print ("Two year credit")
      #           summaryDF["Child_Dependent_Care_Credit", ] <- taxCredits[["CDC"]]["Child_Dependent_Care_Credit",]
      #         } else {
      #           # print("One year credit")
      #           taxYear <- colnames(taxCredits[["CDC"]])
      #           if (taxYear =="2018"){
      #             summaryDF["Child_Dependent_Care_Credit",1] <- taxCredits[["CDC"]]["Child_Dependent_Care_Credit",]
      #           }else {
      #             summaryDF["Child_Dependent_Care_Credit",2] <- taxCredits[["CDC"]]["Child_Dependent_Care_Credit",]
      #           }
      #         }
      #       }
      #       if(is.data.frame(taxCredits[["Education"]])){
      #         if (length(colnames(taxCredits[["Education"]]))==2){
      #           # print ("Two year credit")
      #           summaryDF["Nonrefundable_Educational_Credit", ] <- taxCredits[["Education"]]["Line_19",]
      #         }else {
      #           # print("One year credit")
      #           taxYear <- colnames(taxCredits[["Education"]])
      #           if (taxYear=="2018"){
      #             summaryDF["Nonrefundable_Education_Credit",1] <- taxCredits[["Education"]]["Line_19",]
      #           }
      #           else {
      #             summaryDF["Nonrefundable_Education_Credit",2] <- taxCredits[["Education"]]["Line_19",]
      #           }
      #         }
      #       }
      #       # 
      #     }
      #     
      #     summaryDF[is.na.data.frame(summaryDF)] <- 0
           return (summaryDF)
  }) # End Tax Summary
}

shinyApp(ui, server)