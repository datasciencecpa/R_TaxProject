# Project: Income Tax Project
# Author: Long Nguyen
# Date: 01/19/2019
library(shiny)
library(shinyjs) #loading addional package to enable more UI experience

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Federal Income Tax 2018 & 2017"),
  navbarPage("Navigation bar",
    tabPanel("Home",
       navlistPanel("Enter Your Information:",
         tabPanel("Filing Status & Dependency",
            fluidRow(
              column(5, h3("2018"), 
                     radioButtons(inputId = "filingStatus_2018", label= "1. Please select your filing status below:",
                                  choices = c("Single", "Married Filing Jointly", "Married Filing Separately", "Head of Household", "Qualified Widower"), 
                                  selected = "Single", inline = FALSE),
                     "Need help? Visit IRS Interactive Tax Assistant: ",
                     a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?")
              ),
              column(5, h3("2017"), 
                     radioButtons(inputId = "filingStatus_2017", label= "1. Please select your filing status below:",
                                  choices = c("Single", "Married Filing Jointly", "Married Filing Separately", "Head of Household", "Qualified Widower"), 
                                  selected = "Single", inline = FALSE),
                     "Need help? Visit IRS Interactive Tax Assistant: ",
                     a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?")
              )
            ), # End first row of filing status
            hr(),
            fluidRow(
              column(5, 
                 radioButtons(inputId = "isDependent_2018", label = "Can someone claim you as a dependent on their tax return?",
                              choices = c("Yes", "No"), selected = "No")
              ), 
              column(5, 
                     radioButtons(inputId = "isDependent_2017", label = "Can someone claim you as a dependent on their tax return?",
                                  choices = c("Yes", "No"), selected = "No") 
              ) 
            ), # End second row of checking dependency on other people return
            hr(),
            h3("Number of Dependents"),
            fluidRow(
              column(5, 
                     numericInput(inputId = "numQualifiedChild_2018", label = "Enter number of your qualifying child",
                                  min= 0, max=10, value = 0),
                     numericInput(inputId = "numQualifiedRelative_2018", label = "Enter number of your qualifying relative",
                                  min= 0, max=10, value = 0),
                     "Need help? Visit this Turbo Tax article to learn more: ",
                     a(href = "https://turbotax.intuit.com/tax-tips/family/rules-for-claiming-a-dependent-on-your-tax-return/L8LODbx94", 
                       "Rules for claiming a dependent on your tax return")
              ), 
              column(5, 
                     numericInput(inputId = "numQualifiedChild_2017", label = "Enter number of your qualifying child",
                                  min= 0, max=5, value = 0),
                     numericInput(inputId = "numQualifiedRelative_2017", label = "Enter number of your qualifying relative",
                                  min= 0, max=5, value = 0),
                     "Need help? Visit this Turbo Tax article to learn more: ",
                     a(href = "https://turbotax.intuit.com/tax-tips/family/rules-for-claiming-a-dependent-on-your-tax-return/L8LODbx94", 
                       "Rules for claiming a dependent on your tax return")
              ) 
            ), # Ending third row, entering number of dependents
            hr(),
            h3("Ages:"),
            fluidRow(
              column(5, 
                     numericInput(inputId = "yourAge_2018", label = "Enter your age:",
                                  min= 0, max=200, value = 30),
                     numericInput(inputId = "spouseAge_2018", label = "Enter your spouse age:",
                                  min= 0, max=200, value = 30),
                     numericInput(inputId = "child1Age_2018", label = "Enter your child 1 age:",
                                  min = 0, max=200, value = 10),
                     
                     numericInput(inputId = "child2Age_2018", label = "Enter your child 2 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "child3Age_2018", label = "Enter your child 3 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "child4Age_2018", label = "Enter your child 4 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "child5Age_2018", label = "Enter your child 5 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "qualifiedRel1_2018", label = "Enter your qualifying relative 1 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "qualifiedRel2_2018", label = "Enter your qualifying relative 2 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "qualifiedRel3_2018", label = "Enter your qualifying relative 3 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "qualifiedRel4_2018", label = "Enter your qualifying relative 4 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "qualifiedRel5_2018", label = "Enter your qualifying relative 5 age:",
                                  min = 0, max=200, value = 10)
              ), 
              column(5, 
                     numericInput(inputId = "yourAge_2017", label = "Enter your age:",
                                  min= 0, max=200, value = 30),
                     numericInput(inputId = "spouseAge_2017", label = "Enter your spouse age:",
                                  min= 0, max=200, value = 30),
                     numericInput(inputId = "child1Age_2017", label = "Enter your child 1 age:",
                                  min = 0, max=200, value = 10),
                     
                     numericInput(inputId = "child2Age_2017", label = "Enter your child 2 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "child3Age_2017", label = "Enter your child 3 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "child4Age_2017", label = "Enter your child 4 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "child5Age_2017", label = "Enter your child 5 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "qualifiedRel1_2017", label = "Enter your qualifying relative 1 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "qualifiedRel2_2017", label = "Enter your qualifying relative 2 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "qualifiedRel3_2017", label = "Enter your qualifying relative 3 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "qualifiedRel4_2017", label = "Enter your qualifying relative 4 age:",
                                  min = 0, max=200, value = 10),
                     numericInput(inputId = "qualifiedRel5_2017", label = "Enter your qualifying relative 5 age:",
                                  min = 0, max=200, value = 10)
              ) 
            )
         ),
         tabPanel("Income"),
         tabPanel("Deductions"),
         tabPanel("Credits"),
         tabPanel("Result")
       )
    ),
    tabPanel("Help", 
        navlistPanel("General Questions",
          tabPanel("Filing Status",
                   "1.IRS Interactive Tax Assistant: ", a(href = "https://www.irs.gov/help/ita/what-is-my-filing-status", "What is my filing status?"
          )
        )
             
             
    )
    #tabPanel("About",
      
    )       
             
             
  )
  
)

server <- function(input, output, session) {
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
    # Display numericinput Ages depends on status checked above
    if (input$filingStatus_2018 == "Married Filing Jointly"){
      show(id = "spouseAge_2018")
    } else hide(id = "spouseAge_2018")
    if (input$filingStatus_2017 == "Married Filing Jointly"){
      show(id = "spouseAge_2017")
    } else hide(id = "spouseAge_2017")
  })
  
}

shinyApp(ui, server)