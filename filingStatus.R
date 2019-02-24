filingInformationUI <- function (id){
  ns <- NS(id)
  tabPanel("Filing Status & Dependency",
     
     fluidRow(
       column(6,checkboxInput(ns("same"), label = "Applied everything from 2018 to 2017", value = TRUE )),
       column(4, helpText("Check this box if you want to apply the same inputs from 2018 to 2017. Uncheck this box if you would like to make change to 2017"))
     ), hr(),
     fluidRow(
        h3("Filing Status"),
        column(4, 
              radioButtons(ns ("filingStatus_2018"), label= "2018: Select your status below:",
                           choices = c(Single = "Single", Married_Filing_Jointly = "MFJ", Married_Filing_Separately = "MFS", Head_of_Household = "HOH", Qualified_Widower = "QW"), 
                            inline = FALSE)
        ),
        column(4, 
              radioButtons(ns("filingStatus_2017"), label= "2017: Select your status below:",
                           choices = c(Single = "Single", Married_Filing_Jointly = "MFJ", Married_Filing_Separately = "MFS", Head_of_Household = "HOH", Qualified_Widower = "QW"), 
                            inline = FALSE)
        ),
        column(4,"Need help? Visit IRS Interactive Tax Assistant: ",
               a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?"))
              
     ), # End first row of filing status
      hr(),        
      fluidRow(
        h3("Number of Dependents"),
        column(4, h4("2018"),
              numericInput(ns("numQualifiedChildUnder17_2018"), label = "Enter number of your qualifying child under age of 17", 0,
                           min= 0, max=10),
              numericInput(ns("numQualifiedChildOver17_2018"), label = "Enter number of your qualifying child over age of 17", 0,
                          min= 0, max=10),
              numericInput(ns("numQualifiedRelative_2018"), label = "Enter number of your qualifying relative",0,
                           min= 0, max=10)),
        column(4, h4("2017"),
              numericInput(ns("numQualifiedChildUnder17_2017"), label = "Enter number of your qualifying child under age of 17",
                           min= 0, max=5, value = 0),
              numericInput(ns("numQualifiedChildOver17_2017"), label = "Enter number of your qualifying child over age of 17",
                           min= 0, max=5, value = 0),
              numericInput(ns("numQualifiedRelative_2017"), label = "Enter number of your qualifying relative",
                           min= 0, max=5, value = 0)),
        column(4, tags$strong("Help:"), "Need help? Visit this Turbo Tax article to learn more: ",
               a(href = "https://turbotax.intuit.com/tax-tips/family/rules-for-claiming-a-dependent-on-your-tax-return/L8LODbx94", 
                 "Rules for claiming a dependent on your tax return"))
      ),  # Ending third row, entering number of dependents             
     hr(),
     h3("Ages"),
     fluidRow(
       column(6, h4("2018"),
              numericInput(ns("yourAge_2018"), label = "Enter your age:",
                           min= 0, max=200, value = 30),
              checkboxInput(ns("youBlind_2018"), label = "Check if you are blind", value = FALSE),
              numericInput(ns("spouseAge_2018"), label = "Enter your spouse age:",
                           min= 0, max=200, value = 0),
              checkboxInput(ns("spouseBlind_2018"), label = "Check if you are blind", value = FALSE)
       ), 
       column(6, h4("2017"),
              numericInput(ns( "yourAge_2017"), label = "Enter your age:",
                           min= 0, max=200, value = 30),
              checkboxInput(ns("youBlind_2017"), label = "Check if you are blind", value = FALSE),
              numericInput(ns( "spouseAge_2017"), label = "Enter your spouse age:",
                           min= 0, max=200, value = 0),
              checkboxInput(ns("spouseBlind_2017"), label = "Check if you are blind", value = FALSE)
       ) 
     )
  )
}
filingInformation <- function (input, output, session){
  observe({
    
    if (input$filingStatus_2018 != "MFJ"){
      hideshow(c("spouseAge_2018","spouseBlind_2018"), hide = TRUE)
      updateNumericInput(session, "spouseAge_2018", label = "Enter your spouse age:",value = 0)
      updateCheckboxInput(session, "spouseBlind_2018", label = "Check if you are blind",value = FALSE)
    }
    else {
      hideshow(c("spouseAge_2018", "spouseBlind_2018"), hide = FALSE)
    }
    if (input$filingStatus_2017 != "MFJ"){
      hideshow(c("spouseAge_2017", "spouseBlind_2017"), hide = TRUE)
      updateNumericInput(session, "spouseAge_2017", value = 0)
      updateCheckboxInput(session, "spouseBlind_2017", value = FALSE)
    }
    else {
      hideshow(c("spouseAge_2017", "spouseBlind_2017"), hide = FALSE)
    }
    if (input$same) {
      updateRadioButtons(session,"filingStatus_2017", choices = c(Single = "Single", Married_Filing_Jointly = "MFJ", 
                        Married_Filing_Separately = "MFS", Head_of_Household = "HOH", Qualified_Widower = "QW"), 
                        selected = input$filingStatus_2018)
      updateNumericInput(session, "numQualifiedChildUnder17_2017", label = "Enter number of your qualifying child under age of 17",value= input$numQualifiedChildUnder17_2018 ,
                         min= 0, max=10)
      updateNumericInput(session, "numQualifiedChildOver17_2017", label = "Enter number of your qualifying child over age of 17",value= input$numQualifiedChildOver17_2018 ,
                         min= 0, max=10)
      updateNumericInput(session, "numQualifiedRelative_2017", label = "Enter number of your qualifying relative",value= input$numQualifiedRelative_2018,
                         min= 0, max=10)
      updateNumericInput(session, "yourAge_2017", label = "Enter your age:", value = ifelse (input$yourAge_2018>0, input$yourAge_2018-1, input$yourAge_2018))
      updateNumericInput(session, "spouseAge_2017", label = "Enter your spouse age:", value = ifelse(input$spouseAge_2018>0, input$spouseAge_2018-1, input$spouseAge_2018))
      updateCheckboxInput(session, "youBlind_2017", label = "Check if you are blind", value= input$youBlind_2018)
      updateCheckboxInput(session, "spouseBlind_2017", label = "Check if you are blind", value = input$spouseBlind_2018)
      
    } 
  }) # Finish cosmetic issues.
  filingStatusDF <- reactive ({
    rowNames <- c("Filing_Status", "Qualifying_Child_Under_17", "Qualifying_Child_Over_17","Qualifying_Relative",
                  "Your_Age", "Spouse_Age", "You_Blind", "Spouse_Blind")

    Status_2018 <-  c(input$filingStatus_2018, 
                      input$numQualifiedChildUnder17_2018,
                      input$numQualifiedChildOver17_2018,
                      input$numQualifiedRelative_2018,
                      input$yourAge_2018,
                      input$spouseAge_2018,
                      input$youBlind_2018,
                      input$spouseBlind_2018
                      ) 
    Status_2017 <- c( input$filingStatus_2017, 
                      input$numQualifiedChildUnder17_2017,
                      input$numQualifiedChildOver17_2017,
                      input$numQualifiedRelative_2017,
                      input$yourAge_2017,
                      input$spouseAge_2017,
                      input$youBlind_2017,
                      input$spouseBlind_2017
                      ) 

    return (data.frame(Status_2018, Status_2017, row.names = rowNames, stringsAsFactors = FALSE))
  })
  
}
