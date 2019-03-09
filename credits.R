creditsUI <- function (id){
  ns <- NS(id)
  tabPanel("Credits",
     
     h3("Credits"),
     hr(),
     h3("Child and Dependent Care Expenses - See ", a(href = "https://www.irs.gov/pub/irs-pdf/i2441.pdf", "IRS link")," for more information"),
     helpText("Currently, this form does not support calculation if you received dependent care benefits from employers. Fill out the fields below if you have \n
              dependent care expenses, otherwise please skip to the next section."),
     
     fluidRow(
        h4("Married Filing Separately Exception"),
        checkboxInput(ns("MFSException"), label = "Married Filing Separately - Uncheck this box if you do not meet the MFS exception to claim this credit", value = TRUE),
        column(6,numericInput(ns("numQualifying"), label = "Enter number of qualifying person", value = 0, min= 0, max = 2)),
        column(6,numericInput(ns("qualifiedExp"), label = "Enter total of qualified expenses (Max $6000)", value = 0, min = 0, max = 6000)),
        helpText("If one or both spouses were students/disabled, check the boxes below and enter numbers of months you were students or disabled. \n
                 See form 2441 Instruction for more detail."),
        column(6,checkboxInput(ns("youFTStudent"), label = "You were a full-time student or disabled and did not have any earned income"),
            numericInput(ns("yourMonths"), label = "Enter number of months you were full-time student or disabled", value = 0, min=0, max = 12)),
        column(6,checkboxInput(ns("spouseFTStudent"), label = "You were a full-time student or disabled and did not have any earned income"),
            numericInput(ns("spouseMonths"), label = "Enter number of months your spouse were FT student or disabled", value = 0, min = 0, max = 12))
     ), #End CDC Section
     #---------------------------------------------------------------------------------------------------------------------------------------
     hr(),
     h3("Education Credits - See ", a(href= "https://www.irs.gov/pub/irs-pdf/p970.pdf", "IRS Publication 970"), " for more information"),
     helpText("Currently, this form only supports the calculation of Education Credits of up to 2 students. See detailed example of ,\n
              Education Credits if you have more than 3 qualified educational expenses."),
     fluidRow( # First student
         h4("First Student"),
         helpText("Checkboxes below are used to identify if a student was qualified for AOC credit. It's default that a student was eligible for AOC credit"),
         column(4,checkboxInput(ns("claimedAOC4Yrs1"), label = "Has the Hope Credit or AOC been claimed for this student for any 4 tax years before the current year?",
                        value = FALSE)),
         column(4, checkboxInput(ns("completePost4Yrs1"), label = "Did the student complete the first 4 years of postsecondary education before the current years?",
                        value = FALSE)),
         column(4,checkboxInput(ns("halfTime1"),label = "Student enrolled at least half-time for at least one academic period?", value = TRUE)),
         column(6,numericInput(ns("expenses1"), label = "Enter total adjusted qualified education expenses:",value=0, min=0)),
         column(6,numericInput(ns("numStudent1"), label = "Enter number of qualified students related to expenses entered above", value = 1, min=1, max=5))
     ),
     # End input of first student ------------------------------------------------------------------------------------------ 
     hr(),
     fluidRow(  # Second Student
         h4("2018 - Second Student"),
         helpText("Checkboxes below are used to identify if a student was qualified for AOC credit. It's default that a student was eligible for AOC credit"),
         column(4,checkboxInput(ns("claimedAOC4Yrs2"), label = "Has the Hope Credit or AOC been claimed for this student for any 4 tax years before the current year?",
                        value = FALSE)),
         column(4,checkboxInput(ns("completePost4Yrs2"), label = "Did the student complete the first 4 years of postsecondary education before the current years?",
                        value = FALSE)),
         column(4,checkboxInput(ns("halfTime2"),label = "Student enrolled at least half-time for at least one academic period?", value = TRUE)),
         column(6,numericInput(ns("expenses2"), label = "Enter total adjusted qualified education expenses:",value=0, min=0)),
         column(6,numericInput(ns("numStudent2"), label = "Enter number of qualified students related to expenses entered above", value = 1, min=1, max=5))
              
      ),# End input of second student
      hr(), # Next credit is saver's credit
      h3("Saver's Credit - Form 8880 See ", a(href="https://www.irs.gov/retirement-plans/plan-participant-employee/retirement-savings-contributions-savers-credit", "IRS.Gov"), " for more information"),
      helpText("You're eligible for this credit if you're 18 or older, not a full-time student, and not a dependent on another person's return. \n
               Enter amount of contribution as showed on your W-2, box 12 (Codes:D,E,G,H). Do not enter in boxes below if you were not qualified."),
      fluidRow(
        column(6, numericInput(ns("yourContribution"), label = "Enter your contribution", value=0, min= 0)),
        column(6, numericInput(ns("yourSpouseContribution"), label = "Enter spouse contribution", value=0, min= 0))               
      ), # End saver's credit
     hr()
  )
}
credits <- function(input, output, session){
  CDCLabel <-  ""
  observe ({
    if (!input$MFSException) {
      hideshow(c("numQualifying", "qualifiedExp","youFTStudent","yourMonths","spouseFTStudent",
                 "spouseMonths"), hide = TRUE)
      CDCLabel <- "You do not qualify for this credit. Check this box if you meet the MFS exception to claim this credit"
      updateCheckboxInput(session, "MFSException", label = CDCLabel)
      updateNumericInput(session,"numQualifying", value = 0 )
      updateNumericInput(session, "qualifiedExp", value = 0)
      updateNumericInput(session,"yourMonths",   value = 0)
      updateNumericInput(session,"spouseMonths", value = 0)
    }
    else {
      hideshow(c("numQualifying", "qualifiedExp","youFTStudent","yourMonths","spouseFTStudent",
                 "spouseMonths"), hide = FALSE)
      CDCLabel <- "Married Filing Separately - Uncheck this box if you do not meet the MFS exception to claim this credit"
      updateCheckboxInput (session, "MFSException", label = CDCLabel)
    }
  })
  creditDF <- reactive({
    Credit_18 <- c (input$MFSException,
                    input$numQualifying,
                    input$qualifiedExp,
                    input$youFTStudent,
                    ifelse(input$yourMonths>12, 12, input$yourMonths),
                    input$spouseFTStudent,
                    ifelse(input$spouseMonths>12,12,input$spouseMonths),
                    #--------------Add education----------------------------------
                    input$claimedAOC4Yrs1,
                    input$completePost4Yrs1,
                    input$halfTime1,
                    input$expenses1,
                    input$numStudent1,
                    #---------- Student2----------------------------------------
                    input$claimedAOC4Yrs2,
                    input$completePost4Yrs2,
                    input$halfTime2,
                    input$expenses2,
                    input$numStudent2
                    )
    
    rowName <- c("MFS_Exception", "Qualifying_Person", "Expense", "You_FT_Student", "FT_Student_Month",
                 "Spouse_FT_Student","Spouse_FT_Student_Month", "Claimed_AOC_4Yrs_1", "Complete_Post_4Yrs_1",
                 "At_least_half-time_student_1", "Expense_1", "Number_Student_1", "Claimed_AOC_4Yrs_2", "Complete_Post_4Yrs_2",
                 "At_least_half-time_student_2", "Expense_2", "Number_Student_2")
    dfCredit <- data.frame(Credit_18, row.names = rowName, stringsAsFactors = FALSE)
    dfCredit[is.na.data.frame(dfCredit)] <- 0
    return (dfCredit)
  })
}