filingInformationUI <- function (id){
  ns <- NS(id)
  tabPanel("Filing Status & Dependency",
     fluidRow(
       column(4, h3("2018")),
       column(4, h3("2017")),
       column(3, h3("Help"))
     ), hr(),
     fluidRow(
       column(5,checkboxInput(ns("same"), label = "Applied everything from 2018 to 2017", value = TRUE )),
       column(5, helpText("Check this box if you want to apply the same inputs from 2018 to 2017. Uncheck this box if you would like to make change to 2017"))
     ), hr(),
     fluidRow(
        h3("Filing Status"),
        column(4, 
              radioButtons(ns ("filingStatus_2018"), label= "Select your status below:",
                           choices = c(Single = "Single", Married_Filing_Jointly = "MFJ", Married_Filing_Separately = "MFS", Head_of_Household = "HOH", Qualified_Widower = "QW"), 
                            inline = FALSE)
        ),
        column(4, 
              radioButtons(ns("filingStatus_2017"), label= "Select your status below:",
                           choices = c(Single = "Single", Married_Filing_Jointly = "MFJ", Married_Filing_Separately = "MFS", Head_of_Household = "HOH", Qualified_Widower = "QW"), 
                            inline = FALSE)
        ),
        column(3,"Need help? Visit IRS Interactive Tax Assistant: ",
               a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?"))
              
     ), # End first row of filing status
      hr(),        
      fluidRow(
        h3("Number of Dependents"),
        column(4, 
              numericInput(ns("numQualifiedChildUnder17_2018"), label = "Enter number of your qualifying child under age of 17", 0,
                           min= 0, max=10),
              numericInput(ns("numQualifiedChildOver17_2018"), label = "Enter number of your qualifying child over age of 17", 0,
                          min= 0, max=10),
              numericInput(ns("numQualifiedRelative_2018"), label = "Enter number of your qualifying relative",0,
                           min= 0, max=10)),
        column(4,
              numericInput(ns("numQualifiedChildUnder17_2017"), label = "Enter number of your qualifying child under age of 17",
                           min= 0, max=5, value = 0),
              numericInput(ns("numQualifiedChildOver17_2017"), label = "Enter number of your qualifying child over age of 17",
                           min= 0, max=5, value = 0),
              numericInput(ns("numQualifiedRelative_2017"), label = "Enter number of your qualifying relative",
                           min= 0, max=5, value = 0)),
        column(3, tags$strong("Help:"), "Need help? Visit this Turbo Tax article to learn more: ",
               a(href = "https://turbotax.intuit.com/tax-tips/family/rules-for-claiming-a-dependent-on-your-tax-return/L8LODbx94", 
                 "Rules for claiming a dependent on your tax return"))
      ),  # Ending third row, entering number of dependents             
     hr(),
     h3("Ages:"),
     fluidRow(
       column(5, 
              numericInput(ns("yourAge_2018"), label = "Enter your age:",
                           min= 0, max=200, value = 30),
              checkboxInput(ns("youBlind_2018"), label = "Check if you are blind", value = FALSE),
              numericInput(ns("spouseAge_2018"), label = "Enter your spouse age:",
                           min= 0, max=200, value = 0),
              checkboxInput(ns("spouseBlind_2018"), label = "Check if you are blind", value = FALSE),
              numericInput(ns("child1Age_2018"), label = "Enter your child 1 age:",
                           min = 0, max=200, value = 0),
              
              numericInput(ns("child2Age_2018"), label = "Enter your child 2 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns("child3Age_2018"), label = "Enter your child 3 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns("child4Age_2018"), label = "Enter your child 4 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "child5Age_2018"), label = "Enter your child 5 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "qualifiedRel1_2018"), label = "Enter your qualifying relative 1 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "qualifiedRel2_2018"), label = "Enter your qualifying relative 2 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "qualifiedRel3_2018"), label = "Enter your qualifying relative 3 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "qualifiedRel4_2018"), label = "Enter your qualifying relative 4 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "qualifiedRel5_2018"), label = "Enter your qualifying relative 5 age:",
                           min = 0, max=200, value = 0)
       ), 
       column(5, 
              numericInput(ns( "yourAge_2017"), label = "Enter your age:",
                           min= 0, max=200, value = 30),
              checkboxInput(ns("youBlind_2017"), label = "Check if you are blind", value = FALSE),
              numericInput(ns( "spouseAge_2017"), label = "Enter your spouse age:",
                           min= 0, max=200, value = 0),
              checkboxInput(ns("spouseBlind_2017"), label = "Check if you are blind", value = FALSE),
              numericInput(ns( "child1Age_2017"), label = "Enter your child 1 age:",
                           min = 0, max=200, value = 0),
              
              numericInput(ns( "child2Age_2017"), label = "Enter your child 2 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "child3Age_2017"), label = "Enter your child 3 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "child4Age_2017"), label = "Enter your child 4 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "child5Age_2017"), label = "Enter your child 5 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "qualifiedRel1_2017"), label = "Enter your qualifying relative 1 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "qualifiedRel2_2017"), label = "Enter your qualifying relative 2 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "qualifiedRel3_2017"), label = "Enter your qualifying relative 3 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "qualifiedRel4_2017"), label = "Enter your qualifying relative 4 age:",
                           min = 0, max=200, value = 0),
              numericInput(ns( "qualifiedRel5_2017"), label = "Enter your qualifying relative 5 age:",
                           min = 0, max=200, value = 0)
              
       ) 
     )
  )
}
filingInformation <- function (input, output, session){
  observe({
    if (input$filingStatus_2018 == "Single"){
      hide(id= "numQualifiedChildUnder17_2018")
      hide(id= "numQualifiedChildOver17_2018")
      hide(id= "numQualifiedRelative_2018")
    }
    else {
      show(id= "numQualifiedChildUnder17_2018")
      show(id= "numQualifiedChildOver17_2018")
      show(id= "numQualifiedRelative_2018")
    }
    if (input$filingStatus_2017 == "Single"){
      hide(id = "numQualifiedChildUnder17_2017")
      hide(id= "numQualifiedChildOver17_2017")
      hide(id= "numQualifiedRelative_2017")
    }
    else {
      show(id = "numQualifiedChildUnder17_2017")
      show(id= "numQualifiedChildOver17_2017")
      show(id= "numQualifiedRelative_2017")
    }
    if (input$filingStatus_2018 != "MFJ"){
      hide(id = "spouseAge_2018")
      hide(id = "spouseBlind_2018")
    }
    else {
      show(id = "spouseAge_2018")
      show(id = "spouseBlind_2018")
    }
    if (input$filingStatus_2017 != "MFJ"){
      hide(id = "spouseAge_2017")
      hide(id = "spouseBlind_2017")
    }
    else {
      show(id = "spouseAge_2017")
      show(id = "spouseBlind_2017")
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
      updateNumericInput(session, "child1Age_2017", label = "Enter your child 1 age:", value = ifelse (input$child1Age_2018>0, input$child1Age_2018-1,input$child1Age_2018))
      updateNumericInput(session, "child2Age_2017", label = "Enter your child 2 age:", value = ifelse (input$child2Age_2018>0,input$child2Age_2018-1,input$child2Age_2018))
      updateNumericInput(session, "child3Age_2017", label = "Enter your child 3 age:", value = ifelse (input$child3Age_2018>0,input$child3Age_2018-1, input$child3Age_2018))
      updateNumericInput(session, "child4Age_2017", label = "Enter your child 4 age:", value = ifelse (input$child4Age_2018>0, input$child4Age_2018-1, input$child4Age_2018))
      updateNumericInput(session, "child5Age_2017", label = "Enter your child 5 age:", value = ifelse (input$child5Age_2018>0, iinput$child5Age_2018-1, input$child5Age_2018))
      updateNumericInput(session, "qualifiedRel1_2017", label = "Enter your qualifying relative 1 age:", value = ifelse(input$qualifiedRel1_2018>0,input$qualifiedRel1_2018-1, input$qualifiedRel1_2018))
      updateNumericInput(session, "qualifiedRel2_2017", label = "Enter your qualifying relative 2 age:", value = ifelse(input$qualifiedRel2_2018>0,input$qualifiedRel2_2018-1, input$qualifiedRel2_2018))
      updateNumericInput(session, "qualifiedRel3_2017", label = "Enter your qualifying relative 3 age:", value = ifelse(input$qualifiedRel3_2018>0,input$qualifiedRel3_2018-1, input$qualifiedRel3_2018))
      updateNumericInput(session, "qualifiedRel4_2017", label = "Enter your qualifying relative 4 age:", value = ifelse(input$qualifiedRel4_2018>0,input$qualifiedRel4_2018-1, input$qualifiedRel4_2018))
      updateNumericInput(session, "qualifiedRel5_2017", label = "Enter your qualifying relative 5 age:", value = ifelse(input$qualifiedRel5_2018>0,input$qualifiedRel5_2018-1, input$qualifiedRel5_2018))
    } 
  }) # Finish cosmetic issues.
  filingStatusDF <- reactive ({
    rowNames <- c("Filing_Status", "Qualifying_Child_Under_17", "Qualifying_Child_Over_17","Qualifying_Relative",
                  "Your_Age", "Spouse_Age", "You_Blind", "Spouse_Blind", "Child_1_Age", "Child_2_Age",
                  "Child_3_Age", "Child_4_Age", "Child_5_Age", "Relative_1_Age", "Relative_2_Age",
                  "Relative_3_Age", "Relative_4_Age", "Relative_5_Age")

    Status_2018 <-  c(input$filingStatus_2018, 
                      input$numQualifiedChildUnder17_2018,
                      input$numQualifiedChildOver17_2018,
                      input$numQualifiedRelative_2018,
                      input$yourAge_2018,
                      input$spouseAge_2018,
                      input$youBlind_2018,
                      input$spouseBlind_2018,
                      input$child1Age_2018,
                      input$child2Age_2018,
                      input$child3Age_2018,
                      input$child4Age_2018,
                      input$child5Age_2018,
                      input$qualifiedRel1_2018,
                      input$qualifiedRel2_2018,
                      input$qualifiedRel3_2018,
                      input$qualifiedRel4_2018,
                      input$qualifiedRel5_2018) 
  
    Status_2017 <- c( input$filingStatus_2017, 
                      input$numQualifiedChildUnder17_2017,
                      input$numQualifiedChildOver17_2017,
                      input$numQualifiedRelative_2017,
                      input$yourAge_2017,
                      input$spouseAge_2017,
                      input$youBlind_2017,
                      input$spouseBlind_2017,
                      input$child1Age_2017,
                      input$child2Age_2017,
                      input$child3Age_2017,
                      input$child4Age_2017,
                      input$child5Age_2017,
                      input$qualifiedRel1_2017,
                      input$qualifiedRel2_2017,
                      input$qualifiedRel3_2017,
                      input$qualifiedRel4_2017,
                      input$qualifiedRel5_2017) 

    return (data.frame(Status_2018, Status_2017, row.names = rowNames, stringsAsFactors = FALSE))
  })
  
}
