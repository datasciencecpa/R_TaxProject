deductionsUI <- function (id){
  ns <- NS(id)
  tabPanel("Deductions",
     fluidRow(
       column(6,checkboxInput(ns("same"), label = "Applied everything from 2018 to 2017", value = TRUE )),
       column(5, helpText("Check this box if you want to apply the same inputs from 2018 to 2017. Uncheck this box if you would like to make change to 2017"))
     ), 
     hr(), # Inputs for above the line deductions
     fluidRow(
       column(6, h4("2018"),
          numericInput(ns("educatorExp_2018"), label = "Educator Expense ($250/person, up to $500 for MFJ):", value = 0),
          numericInput(ns("HSA_2018"), label = "HSA contributions:", value = 0),
          column(8,
            numericInput(ns("W2HSA_2018"), label = "Enter your HSA contribution as reported on all of your W-2", value = 0)
          ),
          column(4,
            selectInput(ns("HSAPlan_2018"), label = "Select your HSA plan", choices = c("Single", "Family"), selected = "Single")),
          column(6,numericInput(ns("YourIRA_2018"), label = "Your Deductible IRA Contribution:", value = 0)
                 ),
          column(6,numericInput(ns("SpouseIRA_2018"), label = "Spouse Deductible IRA Contribution:", value = 0)),
          column(6, radioButtons(ns("YourIRACover_2018"), label = "Were you covered by a retirement plan at work or through self-employment?",
                                 choices = c("YES", "NO"), selected = "NO")),
          column(6, radioButtons(ns("SpouseIRACover_2018"), label ="Were you covered by a retirement plan at work or through self-employment?",
                                 choices = c("YES", "NO"), selected = "NO")),
          numericInput(ns("studentLoan_2018"), label = "Enter your student loan interest(Max $2500):", value = 0)
       ),
       column(6, h4("2017"),
          numericInput(ns("educatorExp_2017"), label = "Educator Expense ($250/person, up to $500 for MFJ):", value = 0),
          numericInput(ns("HSA_2017"), label = "HSA contributions:", value = 0),
          column(8,
                 numericInput(ns("W2HSA_2017"), label = "Enter your HSA contribution as reported on all of your W-2", value = 0)
          ),
          column(4,selectInput(ns("HSAPlan_2017"), label = "Select your HSA plan", choices = c("Single", "Family"), selected = "Single")),
          column(6,numericInput(ns("YourIRA_2017"), label = "Your Deductible IRA Contribution:", value = 0)
          ),
          column(6,numericInput(ns("SpouseIRA_2017"), label = "Spouse Deductible IRA Contribution:", value = 0)),
          column(6, radioButtons(ns("YourIRACover_2017"), label = "Were you covered by a retirement plan at work or through self-employment?",
                                 choices = c("YES", "NO"), selected = "NO")),
          column(6, radioButtons(ns("SpouseIRACover_2017"), label ="Were you covered by a retirement plan at work or through self-employment?",
                                 choices = c("YES", "NO"), selected = "NO")),
          numericInput(ns("studentLoan_2017"), label = "Enter your student loan interest(Max $2500):", value = 0)
       )
     ), hr(), #Input for itemize deductions
     fluidRow(
       helpText("Enter expenses below if you would like to check itemized deductions. Standard deduction will be claimed if it is higher than your itemized expenses!")
     ),
     fluidRow(
       column(6, h4("2018"),
          numericInput(ns("medicalExp_2018"), label = "Enter eligible medical expenses:", value = 0),
          numericInput(ns("stateTax_2018"), label = "Enter state and local income taxes or general sales tax:", value = 0),
          numericInput(ns("realEstateTax_2018"), label = "Enter real estate taxes:", value = 0 ),
          numericInput(ns("personalTax_2018"), label = "Enter personal property taxes:", value = 0 ),
          numericInput(ns("mortgageInterest_2018"), label = "Enter your eligible mortgage interest:", value = 0 ),
          numericInput(ns("PMI_2018"), label = "Enter your premium mortgage insurance:", value = 0 ),
          numericInput(ns("charitable_2018"), label = "Enter your charitable deductions:", value = 0 )
       ),
       column(6, h4("2018"),
          numericInput(ns("medicalExp_2017"), label = "Enter eligible medical expenses:", value = 0 ),
          numericInput(ns("stateTax_2017"), label = "Enter state and local income taxes or general sales tax:", value = 0 ),
          numericInput(ns("realEstateTax_2017"), label = "Enter real estate taxes:", value = 0 ),
          numericInput(ns("personalTax_2017"), label = "Enter personal property taxes:", value = 0 ),
          numericInput(ns("mortgageInterest_2017"), label = "Enter your eligible mortgage interest:", value = 0 ),
          numericInput(ns("PMI_2017"), label = "Enter your premium mortgage insurance:", value = 0 ),
          numericInput(ns("charitable_2017"), label = "Enter your charitable deductions:", value = 0 )
       )
     )
  )
}
deductions <- function (input, output, session){
  observe({  
    if (input$same){
      updateNumericInput(session, "educatorExp_2017", label = "Educator Expense ($250/person, up to $500 for MFJ):", value = input$educatorExp_2018)
      updateNumericInput(session, "HSA_2017", label = "HSA contributions:", value = input$HSA_2018)
      updateNumericInput(session, "YourIRA_2017", label = "Your Deductible IRA Contribution:", value = input$YourIRA_2018)
      updateNumericInput(session, "SpouseIRA_2017", label = "Spouse Deductible IRA Contribution:", value = input$SpouseIRA_2018)
      updateNumericInput(session, "studentLoan_2017", label = "Enter your student loan interest(Max $2500):", value = input$studentLoan_2018)
      updateNumericInput(session, "medicalExp_2017", label = "Enter eligible medical expenses:", value = input$medicalExp_2018)
      updateNumericInput(session, "stateTax_2017", label = "Enter state and local income taxes or general sales tax:", value = input$stateTax_2018)
      updateNumericInput(session, "realEstateTax_2017", label = "Enter real estate taxes:", value = input$realEstateTax_2018)
      updateNumericInput(session, "personalTax_2017", label = "Enter personal property taxes:", value = input$personalTax_2018)
      updateNumericInput(session, "mortgageInterest_2017", label = "Enter your eligible mortgage interest:", value = input$mortgageInterest_2018)
      updateNumericInput(session, "PMI_2017", label = "Enter your premium mortgage insurance:", value = input$PMI_2018)
      updateNumericInput(session, "charitable_2017", label = "Enter your charitable deductions:", value = input$charitable_2018)
      updateNumericInput(session, "W2HSA_2017", label = "Enter your HSA contribution as reported on all of your W-2", value = input$W2HSA_2018)
      updateSelectInput(session, "HSAPlan_2017", label = "Select your HSA plan", choices = c("Single", "Family"), selected = input$HSAPlan_2018)
      updateRadioButtons(session, "YourIRACover_2017", label = "Were you covered by a retirement plan at work or through self-employment?",
                         choices = c("YES", "NO"), selected = input$YourIRACover_2018)
      updateRadioButtons(session, "SpouseIRACover_2017", label = "Were you covered by a retirement plan at work or through self-employment?",
                         choices = c("YES", "NO"), selected = input$SpouseIRACover_2018)
    }
  })
  deductionDF <- reactive ({
      rowNames <- c("Educator_Expense", "HSA_Contribution", "HSA_Contribution_Per_W2", "HSA_Plan_Type","Your_IRA_Contribution",
                    "Spouse_IRA_Contribution", "Your_IRA_Cover","Spouse_IRA_Cover","Student_Loan_Interest", 
                    "Medical_Exp","State_Local_Taxes", "Real_Estate_Taxes","Personal_Property_Tax", "Mortgage_Interest", 
                    "Premium_Mortage_Interest","Charitable_Contribution")
      Deduction_2018 <- c(
        ifelse(input$educatorExp_2018>500, 500, input$educatorExp_2018), # Reduce amount above $500 to $500, this won't check if user is MFJ or not
        input$HSA_2018,
        input$W2HSA_2018,
        input$HSAPlan_2018,
        input$YourIRA_2018,
        input$SpouseIRA_2018,
        input$YourIRACover_2018,
        input$SpouseIRACover_2018,
        ifelse(input$studentLoan_2018>2500, 2500, input$studentLoan_2018), # Max amount allowed are $2500 before checking for limitation based on MAGI
        input$medicalExp_2018,
        input$stateTax_2018,
        input$realEstateTax_2018,
        input$personalTax_2018,
        input$mortgageInterest_2018,
        input$PMI_2018,
        input$charitable_2018
        
      )
      Deduction_2017 <- c(
        ifelse(input$educatorExp_2017>500, 500, input$educatorExp_2017), # Reduce amount above $500 to $500, this won't check if user is MFJ or not
        input$HSA_2017,
        input$W2HSA_2017,
        input$HSAPlan_2017,
        input$YourIRA_2017,
        input$SpouseIRA_2017,
        input$YourIRACover_2017,
        input$SpouseIRACover_2017,
        ifelse(input$studentLoan_2017>2500, 2500, input$studentLoan_2017), # Max amount allowed are $2500 before checking for limitation based on MAGI
        input$medicalExp_2017,
        input$stateTax_2017,
        input$realEstateTax_2017,
        input$personalTax_2017,
        input$mortgageInterest_2017,
        input$PMI_2017,
        input$charitable_2017
      )
      dfDeduction <- data.frame(Deduction_2018, Deduction_2017, row.names = rowNames, stringsAsFactors = FALSE)
      dfDeduction[is.na.data.frame(dfDeduction)] <- 0
      return (dfDeduction)
  })
}
