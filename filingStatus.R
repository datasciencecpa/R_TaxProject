filingStatusUI <- function (id){
  ns <- NS(id)
  tabPanel("Filing Status & Dependency",
           fluidRow(
             column(5, h3("2018"), 
                    radioButtons(ns ("filingStatus_2018"), label= "1. Please select your filing status below:",
                                 choices = c("Single", "Married Filing Jointly", "Married Filing Separately", "Head of Household", "Qualified Widower"), 
                                 selected = "Single", inline = FALSE),
                    "Need help? Visit IRS Interactive Tax Assistant: ",
                    a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?")
             ),
             column(5, h3("2017"), 
                    radioButtons(ns("filingStatus_2017"), label= "1. Please select your filing status below:",
                                 choices = c("Single", "Married Filing Jointly", "Married Filing Separately", "Head of Household", "Qualified Widower"), 
                                 selected = "Single", inline = FALSE),
                    "Need help? Visit IRS Interactive Tax Assistant: ",
                    a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?")
             )
           ), # End first row of filing status
           hr(),
           fluidRow(
             column(5, 
                    radioButtons(ns ("isDependent_2018"), label = "Can someone claim you as a dependent on their tax return?",
                                 choices = c("Yes", "No"), selected = "No")
             ), 
             column(5, 
                    radioButtons(ns ("isDependent_2017"), label = "Can someone claim you as a dependent on their tax return?",
                                 choices = c("Yes", "No"), selected = "No") 
             ) 
           ), # End second row of checking dependency on other people return
           hr(),
           h3("Number of Dependents"),
           fluidRow(
             column(5, 
                    numericInput(ns("numQualifiedChild_2018"), label = "Enter number of your qualifying child",
                                 min= 0, max=10, value = 0),
                    numericInput(ns("numQualifiedRelative_2018"), label = "Enter number of your qualifying relative",
                                 min= 0, max=10, value = 0),
                    "Need help? Visit this Turbo Tax article to learn more: ",
                    a(href = "https://turbotax.intuit.com/tax-tips/family/rules-for-claiming-a-dependent-on-your-tax-return/L8LODbx94", 
                      "Rules for claiming a dependent on your tax return")
             ), 
             column(5, 
                    numericInput(ns("numQualifiedChild_2017"), label = "Enter number of your qualifying child",
                                 min= 0, max=5, value = 0),
                    numericInput(ns("numQualifiedRelative_2017"), label = "Enter number of your qualifying relative",
                                 min= 0, max=5, value = 0),
                    "Need help? Visit this Turbo Tax article to learn more: ",
                    a(href = "https://turbotax.intuit.com/tax-tips/family/rules-for-claiming-a-dependent-on-your-tax-return/L8LODbx94", 
                      "Rules for claiming a dependent on your tax return")
             ) 
           ),# Ending third row, entering number of dependents
           hr(),
           h3("Ages:"),
           fluidRow(
             column(5, 
                    numericInput(ns("yourAge_2018"), label = "Enter your age:",
                                 min= 0, max=200, value = 30),
                    checkboxInput(ns("youBlind_2018"), label = "Check if you are blind", value = FALSE),
                    numericInput(ns("spouseAge_2018"), label = "Enter your spouse age:",
                                 min= 0, max=200, value = 30),
                    checkboxInput(ns("spouseBlind_2018"), label = "Check if you are blind", value = FALSE),
                    numericInput(ns("child1Age_2018"), label = "Enter your child 1 age:",
                                 min = 0, max=200, value = 10),
                    
                    numericInput(ns("child2Age_2018"), label = "Enter your child 2 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns("child3Age_2018"), label = "Enter your child 3 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns("child4Age_2018"), label = "Enter your child 4 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "child5Age_2018"), label = "Enter your child 5 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "qualifiedRel1_2018"), label = "Enter your qualifying relative 1 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "qualifiedRel2_2018"), label = "Enter your qualifying relative 2 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "qualifiedRel3_2018"), label = "Enter your qualifying relative 3 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "qualifiedRel4_2018"), label = "Enter your qualifying relative 4 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "qualifiedRel5_2018"), label = "Enter your qualifying relative 5 age:",
                                 min = 0, max=200, value = 10)
             ), 
             column(5, 
                    numericInput(ns( "yourAge_2017"), label = "Enter your age:",
                                 min= 0, max=200, value = 30),
                    checkboxInput(ns("youBlind_2017"), label = "Check if you are blind", value = FALSE),
                    numericInput(ns( "spouseAge_2017"), label = "Enter your spouse age:",
                                 min= 0, max=200, value = 30),
                    checkboxInput(ns("spouseBlind_2017"), label = "Check if you are blind", value = FALSE),
                    numericInput(ns( "child1Age_2017"), label = "Enter your child 1 age:",
                                 min = 0, max=200, value = 10),
                    
                    numericInput(ns( "child2Age_2017"), label = "Enter your child 2 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "child3Age_2017"), label = "Enter your child 3 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "child4Age_2017"), label = "Enter your child 4 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "child5Age_2017"), label = "Enter your child 5 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "qualifiedRel1_2017"), label = "Enter your qualifying relative 1 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "qualifiedRel2_2017"), label = "Enter your qualifying relative 2 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "qualifiedRel3_2017"), label = "Enter your qualifying relative 3 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "qualifiedRel4_2017"), label = "Enter your qualifying relative 4 age:",
                                 min = 0, max=200, value = 10),
                    numericInput(ns( "qualifiedRel5_2017"), label = "Enter your qualifying relative 5 age:",
                                 min = 0, max=200, value = 10)
             ) 
           )
  )
}
filingStatus <- function (input, output, session){
  observe({
    if (input$filingStatus_2018 == "Single"){
      hide(id = "numQualifiedChild_2018")
      hide(id = "numQualifiedRelative_2018")
    } else {
      show(id = "numQualifiedChild_2018")
      show(id = "numQualifiedRelative_2018")
    }
    if (input$filingStatus_2017 == "Single"){
      hide(id = "numQualifiedChild_2017")
      hide(id = "numQualifiedRelative_2017")
    } else {
      show(id = "numQualifiedChild_2017")
      show(id = "numQualifiedRelative_2017")
    }
    if (input$filingStatus_2018 != "Married Filing Jointly"){
      hide(id = "spouseAge_2018")
      hide(id = "spouseBlind_2018")
    }else {
      show(id = "spouseAge_2018")
      show(id = "spouseBlind_2018")
    }
    if (input$filingStatus_2017 != "Married Filing Jointly"){
      hide(id = "spouseAge_2017")
      hide(id = "spouseBlind_2017")
    }else {
      show(id = "spouseAge_2017")
      show(id = "spouseBlind_2017")
    }
  })
}
