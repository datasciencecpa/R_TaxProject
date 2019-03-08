deductionsUI <- function (id){
  ns <- NS(id)
  tabPanel("Deductions",
    # Inputs for above the line deductions
     h2("Above the line deductions"),
     hr(),
     h3("Educator Expense"),
     fluidRow(
       column(9, numericInput(ns("educatorExp"), label = "Educator Expense ($250/person, up to $500 for MFJ):", value = 0, min=0)),
       column(3, tags$strong("Help:"), a(href="https://www.irs.gov/taxtopics/tc458", "Topic #458-Educator Expense Deduction"))
     ), # End Educator Expense
     hr(), h3("HSA Deductions"),
     fluidRow(
       column(9, numericInput(ns("HSA"), label = "HSA contributions:", value = 0, min=0),
              column(8,
                     numericInput(ns("W2HSA"), label = "Enter your HSA contribution as reported on all of your W-2", value = 0, min=0)
              ),
              column(4,
                     selectInput(ns("HSAPlan"), label = "Select your HSA plan", choices = c("Single", "Family"), selected = "Single"))     
       ),
       column(3, tags$strong("Help:"), a(href="https://www.irs.gov/publications/p969", "Pub #969: Health Saving Accounts"))
     ), # End HSA Contribution
     hr(), h3("IRA Deduction"),
     fluidRow(
       column(6,numericInput(ns("YourIRA"), label = "Your Deductible IRA Contribution:", value = 0, min=0)
       ),
       column(6,numericInput(ns("SpouseIRA"), label = "Spouse Deductible IRA Contribution:", value = 0, min=0)),
       column(6, radioButtons(ns("YourIRACover"), label = "Were you covered by a retirement plan at work or through self-employment?",
                              choices = c("YES", "NO"), selected = "NO")),
       column(6, radioButtons(ns("SpouseIRACover"), label ="Were you covered by a retirement plan at work or through self-employment?",
                              choices = c("YES", "NO"), selected = "NO")),
       tags$strong("More Information:"), a(href="https://www.irs.gov/retirement-plans/ira-deduction-limits", "IRD Deduction limits")
     ), # End IRA Deduction
     hr(), h3("Student Loan Interest"),
     fluidRow(
       numericInput(ns("studentLoan"), label = "Enter your student loan interest(Max $2500):", value = 0, min=0)
     ), # End student loan deduction
    
    # Itemized Deduction start below -----------------------------------------------------------------------------
    
          
     hr(), #Input for itemize deductions
     h2("Itemized Deductions"),
     hr(),
     fluidRow(
       helpText("Enter expenses below if you would like to check itemized deductions. Standard deduction will be claimed if it is higher than your itemized expenses!"),
       tags$strong("More information:"), a(href="https://www.irs.gov/forms-pubs/about-schedule-a-form-1040","Itemized Deductions")
     ),
     fluidRow(
        column(6,numericInput(ns("medicalExp"), label = "Enter eligible medical expenses:", value = 0, min=0)),
        column(6,numericInput(ns("stateTax"), label = "Enter state and local income taxes or general sales tax:", value = 0, min=0)),
        column(6,numericInput(ns("realEstateTax"), label = "Enter real estate taxes:", value = 0, min=0 )),
        column(6,numericInput(ns("personalTax"), label = "Enter personal property taxes:", value = 0, min=0 )),
        column(6,numericInput(ns("mortgageInterest"), label = "Enter your eligible mortgage interest:", value = 0, min=0 )),
        column(6,numericInput(ns("charitable"), label = "Enter your charitable deductions:", value = 0, min=0)),
        column(6,numericInput(ns("PMI"), label = "Enter your premium mortgage insurance:", value = 0, min=0 )),
        column(6, helpText("PMI was not extended for tax year 2018. Number you entered here would be used for calculation of 2017 tax effect only."))
          
     ) # End Itemized Deductions
  )
}# End UI
deductions <- function (input, output, session){
  
  deductionDF <- reactive ({
      rowNames <- c("Educator_Expense", "HSA_Contribution", "HSA_Contribution_Per_W2", "HSA_Plan_Type","Your_IRA_Contribution",
                    "Spouse_IRA_Contribution", "Your_IRA_Cover","Spouse_IRA_Cover","Student_Loan_Interest", 
                    "Medical_Exp","State_Local_Taxes", "Real_Estate_Taxes","Personal_Property_Tax", "Mortgage_Interest", 
                    "Premium_Mortage_Interest","Charitable_Contribution")
      Deduction_2018 <- c(
        ifelse(input$educatorExp>500, 500, input$educatorExp), # Reduce amount above $500 to $500, this won't check if user is MFJ or not
        input$HSA,
        input$W2HSA,
        input$HSAPlan,
        input$YourIRA,
        input$SpouseIRA,
        input$YourIRACover,
        input$SpouseIRACover,
        ifelse(input$studentLoan>2500, 2500, input$studentLoan), # Max amount allowed are $2500 before checking for limitation based on MAGI
        input$medicalExp,
        input$stateTax,
        input$realEstateTax,
        input$personalTax,
        input$mortgageInterest,
        input$PMI,
        input$charitable
      )
      dfDeduction <- data.frame(Deduction_2018, row.names = rowNames, stringsAsFactors = FALSE)
      dfDeduction[is.na.data.frame(dfDeduction)] <- 0
      return (dfDeduction)
  })
}
