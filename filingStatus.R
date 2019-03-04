filingInformationUI <- function (id){
  ns <- NS(id)
  tabPanel("Filing Status & Dependency",

     fluidRow(
        h3("Filing Status"),
        column(8, 
              radioButtons(ns ("filingStatus"), label= "2018: Select your status below:",
                           choices = c(Single = "Single", Married_Filing_Jointly = "MFJ", Married_Filing_Separately = "MFS", Head_of_Household = "HOH", Qualified_Widower = "QW"), 
                            inline = FALSE)
        ),
        column(4,"Need help? Visit IRS Interactive Tax Assistant: ",
               a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?"))
              
     ), # End first row of filing status
      hr(),        
      fluidRow(
        h3("Number of Dependents"),
        column(8, h4("2018"),
              numericInput(ns("numQualifiedChildUnder17"), label = "Enter number of your qualifying child under age of 17", 0,
                           min= 0, max=10),
              numericInput(ns("numQualifiedChildOver17"), label = "Enter number of your qualifying child over age of 17", 0,
                          min= 0, max=10),
              numericInput(ns("numQualifiedRelative"), label = "Enter number of your qualifying relative",0,
                           min= 0, max=10)),
        column(4, tags$strong("Help:"), "Need help? Visit this Turbo Tax article to learn more: ",
               a(href = "https://turbotax.intuit.com/tax-tips/family/rules-for-claiming-a-dependent-on-your-tax-return/L8LODbx94", 
                 "Rules for claiming a dependent on your tax return"))
      ),  # Ending third row, entering number of dependents             
     hr(),
     h3("Ages"),
     fluidRow(
       column(9, h4("2018"),
              numericInput(ns("yourAge"), label = "Enter your age:",
                           min= 0, max=200, value = 0)),
       column(3,checkboxInput(ns("youBlind"), label = "Check if you are blind", value = FALSE)),
       column(9,numericInput(ns("spouseAge"), label = "Enter your spouse age:",
                             min= 0, max=200, value = 0)),      
       column(3,checkboxInput(ns("spouseBlind"), label = "Check if you are blind", value = FALSE))
     )
  )
}
filingInformation <- function (input, output, session){
  observe({
    
    if (input$filingStatus != "MFJ"){
      hideshow(c("spouseAge","spouseBlind"), hide = TRUE)
      updateNumericInput(session, "spouseAge", label = "Enter your spouse age:",value = 0)
      updateCheckboxInput(session, "spouseBlind", label = "Check if you are blind",value = FALSE)
    }
    else {
      hideshow(c("spouseAge", "spouseBlind"), hide = FALSE)
    }
  }) # Finish cosmetic issues.
  filingStatusDF <- reactive ({
    rowNames <- c("Filing_Status", "Qualifying_Child_Under_17", "Qualifying_Child_Over_17","Qualifying_Relative",
                  "Your_Age", "Spouse_Age", "You_Blind", "Spouse_Blind")

    Status_2018 <-  c(input$filingStatus, 
                      input$numQualifiedChildUnder17,
                      input$numQualifiedChildOver17,
                      input$numQualifiedRelative,
                      input$yourAge,
                      input$spouseAge,
                      input$youBlind,
                      input$spouseBlind
                      ) 
    return (data.frame(Status_2018, row.names = rowNames, stringsAsFactors = FALSE))
  })
  
}
