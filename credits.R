creditsUI <- function (id){
  ns <- NS(id)
  tabPanel("Credits",
     fluidRow(
       column(6,checkboxInput(ns("same"), label = "Applied everything from 2018 to 2017", value = TRUE )),
       column(5, helpText("Check this box if you want to apply the same inputs from 2018 to 2017. Uncheck this box if you would like to make change to 2017"))
     ), 
     hr(),
     h3("Child and Dependent Care Expenses - See ", a(href = "https://www.irs.gov/pub/irs-pdf/i2441.pdf", "IRS link")," for more information"),
     helpText("Currently, this form does not support calculation if you received dependent care benefits from employers. Fill out the fields below if you have ,\n
              dependent care expenses, otherwise please skip to the next section."),
     
     fluidRow(
       column(6, h4("2018"),
          checkboxInput(ns("MFSException_2018"), label = "Married Filing Separately - Uncheck this box if you do not meet the MFS exception to claim this credit", value = TRUE),
          numericInput(ns("numQualifying_2018"), label = "Enter number of qualifying person", value = 0, min= 0),
          numericInput(ns("qualifiedExp_2018"), label = "Enter total of qualified expenses (Max $6000)", value = 0, min = 0),
          column(6,checkboxInput(ns("youFTStudent_2018"), label = "You were a full-time student or disabled and did not have any earned income"),
            numericInput(ns("yourMonths_2018"), label = "Enter number of months you were full-time student or disabled", value = 0)
          ),
          column(6,checkboxInput(ns("spouseFTStudent_2018"), label = "You were a full-time student or disabled and did not have any earned income"),
            numericInput(ns("spouseMonths_2018"), label = "Enter number of months your spouse were FT student or disabled", value = 0))
       ),
       column(6, h4("2017"),
          checkboxInput(ns("MFSException_2017"), label = "Married Filing Separately - Uncheck this box if you do not meet the MFS exception to claim this credit", value = TRUE),
          numericInput(ns("numQualifying_2017"), label = "Enter number of qualifying person", value = 0, min= 0),
          numericInput(ns("qualifiedExp_2017"), label = "Enter total of qualified expenses (Max $6000)", value = 0, min = 0),
          column(6,checkboxInput(ns("youFTStudent_2017"), label = "You were a full-time student or disabled and did not have any earned income"),
                 numericInput(ns("yourMonths_2017"), label = "Enter number of months you were full-time student or disabled", value = 0)
          ),
          column(6,checkboxInput(ns("spouseFTStudent_2017"), label = "You were a full-time student or disabled and did not have any earned income"),
                 numericInput(ns("spouseMonths_2017"), label = "Enter number of months your spouse were FT student or disabled", value = 0))
       )
     ), hr(),
     h3("Education Credits - See ", a(href= "https://www.irs.gov/pub/irs-pdf/p970.pdf", "IRS Publication 970"), " for more information"),
     helpText("Currently, this form only supports the calculation of Education Credits of up to 2 students. See detailed example of ,\n
              Education Credits if you have more than 3 qualified educational expenses."),
     fluidRow( # First student
       column(6, h4("2018 - First Student"),
          checkboxInput(ns("claimedAOC4Yrs1_2018"), label = "Has the Hope Credit or AOC been claimed for this student for any 4 tax years before the current year?",
                        value = FALSE),
          checkboxInput(ns("completePost4Yrs1_2018"), label = "Did the student complete the first 4 years of postsecondary education before the current years?",
                        value = FALSE),
          checkboxInput(ns("halfTime1_2018"),label = "Student enrolled at least half-time for at least one academic period?", value = TRUE),
          numericInput(ns("expenses1_2018"), label = "Enter total adjusted qualified education expenses:",value=0, min=0),
          numericInput(ns("numStudent1_2018"), label = "Enter number of qualified students related to expenses entered above", value = 1, min=1, max=5)
       ),
       column(6, h4("2017 - First Student"),
          checkboxInput(ns("claimedAOC4Yrs1_2017"), label = "Has the Hope Credit or AOC been claimed for this student for any 4 tax years before the current year?",
                        value = FALSE),
          checkboxInput(ns("completePost4Yrs1_2017"), label = "Did the student complete the first 4 years of postsecondary education before the current years?",
                        value = FALSE),
          checkboxInput(ns("halfTime1_2017"),label = "Student enrolled at least half-time for at least one academic period?", value = TRUE),
          numericInput(ns("expenses1_2017"), label = "Enter total adjusted qualified education expenses:",value=0, min=0),
          numericInput(ns("numStudent1_2017"), label = "Enter number of qualified students related to expenses entered above", value = 1, min=1, max=5)
       )
     ),
     hr(),
     fluidRow(  # Second Student
       column(6, h4("2018 - Second Student"),
          checkboxInput(ns("claimedAOC4Yrs2_2018"), label = "Has the Hope Credit or AOC been claimed for this student for any 4 tax years before the current year?",
                        value = FALSE),
          checkboxInput(ns("completePost4Yrs2_2018"), label = "Did the student complete the first 4 years of postsecondary education before the current years?",
                        value = FALSE),
          checkboxInput(ns("halfTime2_2018"),label = "Student enrolled at least half-time for at least one academic period?", value = TRUE),
          numericInput(ns("expenses2_2018"), label = "Enter total adjusted qualified education expenses:",value=0, min=0),
          numericInput(ns("numStudent2_2018"), label = "Enter number of qualified students related to expenses entered above", value = 1, min=1, max=5)
              
       ),
       column(6, h4("2017 - Second Student"),
          checkboxInput(ns("claimedAOC4Yrs2_2017"), label = "Has the Hope Credit or AOC been claimed for this student for any 4 tax years before the current year?",
                        value = FALSE),
          checkboxInput(ns("completePost4Yrs2_2017"), label = "Did the student complete the first 4 years of postsecondary education before the current years?",
                        value = FALSE),
          checkboxInput(ns("halfTime2_2017"),label = "Student enrolled at least half-time for at least one academic period?", value = TRUE),
          numericInput(ns("expenses2_2017"), label = "Enter total adjusted qualified education expenses:",value=0, min=0),
          numericInput(ns("numStudent2_2017"), label = "Enter number of qualified students related to expenses entered above", value = 1, min=1, max=5)
       )
     ), # End Education credit section
     hr()
  )
}
credits <- function(input, output, session){
  CDCLabel <-  ""
  observe ({
    if (!input$MFSException_2018) {
      hideshow(c("numQualifying_2018", "qualifiedExp_2018","youFTStudent_2018","yourMonths_2018","spouseFTStudent_2018",
                 "spouseMonths_2018"), hide = TRUE)
      CDCLabel <- "You do not qualify for this credit. Check this box if you meet the MFS exception to claim this credit"
      updateCheckboxInput(session, "MFSException_2018", label = CDCLabel)
      updateNumericInput(session,"numQualifying_2018", value = 0 )
      updateNumericInput(session, "qualifiedExp_2018", value = 0)
      updateNumericInput(session,"yourMonths_2018",   value = 0)
      updateNumericInput(session,"spouseMonths_2018", value = 0)
    }
    else {
      hideshow(c("numQualifying_2018", "qualifiedExp_2018","youFTStudent_2018","yourMonths_2018","spouseFTStudent_2018",
                 "spouseMonths_2018"), hide = FALSE)
      CDCLabel <- "Married Filing Separately - Uncheck this box if you do not meet the MFS exception to claim this credit"
      updateCheckboxInput (session, "MFSException_2018", label = CDCLabel)
    }
    if (!input$MFSException_2017) {
      hideshow(c("numQualifying_2017", "qualifiedExp_2017","youFTStudent_2017","yourMonths_2017","spouseFTStudent_2017",
                 "spouseMonths_2017"), hide = TRUE)
      updateCheckboxInput(session, "MFSException_2017", label = "You do not qualify for this credit. Check this box if you meet the MFS exception to claim this credit")
      updateNumericInput(session,"numQualifying_2017", value = 0 )
      updateNumericInput(session, "qualifiedExp_2017", value = 0)
      updateNumericInput(session,"yourMonths_2017",   value = 0)
      updateNumericInput(session,"spouseMonths_2017", value = 0)
    } 
    else {
      hideshow(c("numQualifying_2017", "qualifiedExp_2017","youFTStudent_2017","yourMonths_2017","spouseFTStudent_2017",
                 "spouseMonths_2017"), hide = FALSE)
      updateCheckboxInput (session, "MFSException_2017", label = "Married Filing Separately - Uncheck this box if you do not meet the MFS exception to claim this credit")
    }
    if (input$same){
      updateCheckboxInput(session, "MFSException_2017", label= CDCLabel, value = input$MFSException_2018)
      updateNumericInput(session, "numQualifying_2017", label = "Enter number of qualifying person", value = input$numQualifying_2018)
      updateNumericInput(session, "qualifiedExp_2017", label = "Enter total of qualified expenses (Max $6000)", value = input$qualifiedExp_2018)
      updateCheckboxInput(session, "youFTStudent_2017", label = "You were a full-time student or disabled and did not have any earned income",
                          value = input$youFTStudent_2018)
      updateNumericInput(session, "yourMonths_2017", label = "Enter number of months you were full-time student or disabled",
                         value = input$yourMonths_2018)
      updateCheckboxInput(session,"spouseFTStudent_2017", label = "You were a full-time student or disabled and did not have any earned income",
                          value = input$spouseFTStudent_2018)
      updateNumericInput(session, "spouseMonths_2017", label="Enter number of months your spouse were FT student or disabled",
                         value = input$spouseMonths_2018)
      #-------------------------------------------------------------- End CDC Section---------------------
      updateCheckboxInput(session,"claimedAOC4Yrs1_2017", label ="Has the Hope Credit or AOC been claimed for this student for any 4 tax years before the current year?",
                          value = input$claimedAOC4Yrs1_2018)
      updateCheckboxInput(session,"completePost4Yrs1_2017", label = "Did the student complete the first 4 years of postsecondary education before the current years?", 
                          value = input$completePost4Yrs1_2018)
      updateCheckboxInput(session, "halfTime1_2017", label = "Student enrolled at least half-time for at least one academic period?",
                          value = input$halfTime1_2018)
      updateNumericInput(session, "expenses1_2017", label = "Enter total adjusted qualified education expenses:",min= 0, value = input$expenses1_2018)
      updateNumericInput(session,"numStudent1_2017", label = "Enter number of qualified students related to expenses entered above",
                         min=1, max=5, value = input$numStudent1_2018)
      #-------------- Update Student 2 -------------------------------------------------------------------
      updateCheckboxInput(session,"claimedAOC4Yrs2_2017", label ="Has the Hope Credit or AOC been claimed for this student for any 4 tax years before the current year?",
                          value = input$claimedAOC4Yrs2_2018)
      updateCheckboxInput(session,"completePost4Yrs2_2017", label = "Did the student complete the first 4 years of postsecondary education before the current years?", 
                          value = input$completePost4Yrs2_2018)
      updateCheckboxInput(session, "halfTime2_2017", label = "Student enrolled at least half-time for at least one academic period?",
                          value = input$halfTime2_2018)
      updateNumericInput(session, "expenses2_2017", label = "Enter total adjusted qualified education expenses:",min= 0, value = input$expenses2_2018)
      updateNumericInput(session,"numStudent2_2017", label = "Enter number of qualified students related to expenses entered above",
                         min=1, max=5, value = input$numStudent2_2018)
    }
  })
  creditDF <- reactive({
    Credit_18 <- c (input$MFSException_2018,
                    input$numQualifying_2018,
                    input$qualifiedExp_2018,
                    input$youFTStudent_2018,
                    ifelse(input$yourMonths_2018>12, 12, input$yourMonths_2018),
                    input$spouseFTStudent_2018,
                    ifelse(input$spouseMonths_2018>12,12,input$spouseMonths_2018),
                    #--------------Add education----------------------------------
                    input$claimedAOC4Yrs1_2018,
                    input$completePost4Yrs1_2018,
                    input$halfTime1_2018,
                    input$expenses1_2018,
                    input$numStudent1_2018,
                    #---------- Student2----------------------------------------
                    input$claimedAOC4Yrs2_2018,
                    input$completePost4Yrs2_2018,
                    input$halfTime2_2018,
                    input$expenses2_2018,
                    input$numStudent2_2018
                    )
    Credit_17 <- c (input$MFSException_2017,
                    input$numQualifying_2017,
                    input$qualifiedExp_2017,
                    input$youFTStudent_2017,
                    ifelse(input$yourMonths_2017>12,12,input$yourMonths_2017),
                    input$spouseFTStudent_2017,
                    ifelse(input$spouseMonths_2017>12, 12, input$spouseMonths_2017),
                    #--------------Add education----------------------------------
                    input$claimedAOC4Yrs1_2017,
                    input$completePost4Yrs1_2017,
                    input$halfTime1_2017,
                    input$expenses1_2017,
                    input$numStudent1_2017,
                    #---------- Student2----------------------------------------
                    input$claimedAOC4Yrs2_2017,
                    input$completePost4Yrs2_2017,
                    input$halfTime2_2017,
                    input$expenses2_2017,
                    input$numStudent2_2017
                    )
    rowName <- c("MFS_Exception", "Qualifying_Person", "Expense", "You_FT_Student", "FT_Student_Month",
                 "Spouse_FT_Student","Spouse_FT_Student_Month", "Claimed_AOC_4Yrs_1", "Complete_Post_4Yrs_1",
                 "At_least_half-time_student_1", "Expense_1", "Number_Student_1", "Claimed_AOC_4Yrs_2", "Complete_Post_4Yrs_2",
                 "At_least_half-time_student_2", "Expense_2", "Number_Student_2")
    dfCredit <- data.frame(Credit_18, Credit_17, row.names = rowName, stringsAsFactors = FALSE)
    dfCredit[is.na.data.frame(dfCredit)] <- 0
    return (dfCredit)
  })
}