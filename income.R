incomeUI <- function (id){
  ns <- NS(id)
  tabPanel("Income",
    # Header
    fluidRow(  
      column(5, h3("2018")),
      column(5, h3("2017"))
    ),
    hr(),
    # Wages
    fluidRow(  
      column(5,
             numericInput(ns("yourWages_2018"), label = "Enter your wages from W-2, box 1:", value = 0, min=0),
             numericInput(ns("spouseWages_2018"), label = "Enter spouse wages from W-2, box 1:", value = 0, min=0),
             numericInput(ns("addWages1_2018"), label = "Enter additional wages from W-2, box 1:", value = 0, min=0),
             actionButton(ns("hideWages1_2018"), label = "Delete additional wages", class="btn btn-danger"),
             numericInput(ns("addWages2_2018"), label = "Enter additional wages from W-2, box 1:", value = 0, min=0),
             actionButton(ns("hideWages2_2018"), label = "Delete additional wages",class="btn btn-danger")
      ),
      column(5,
             numericInput(ns("yourWages_2017"), label = "Enter your wages from W-2, box 1:", value = 0, min=0),
             numericInput(ns("spouseWages_2017"), label = "Enter spouse wages from W-2, box 1:", value = 0, min=0),
             numericInput(ns("addWages1_2017"), label = "Enter additional wages from W-2, box 1:", value = 0, min=0),
             actionButton(ns("hideWages1_2017"), label = "Delete additional wages",class="btn btn-danger"),
             numericInput(ns("addWages2_2017"), label = "Enter additional wages from W-2, box 1:", value = 0, min=0),
             actionButton(ns("hideWages2_2017"), label = "Delete additional wages", class="btn btn-danger")
      )
    ),
    hr(),
    # Interest
    fluidRow(
      column(5, 
             numericInput(ns("interest_2018"), label = "Enter your taxable interest income: (Form 1099-Int, Box 1)",
                          value = 0, min=0)
      ),
      column(5, 
             numericInput(ns("interest_2017"), label = "Enter your taxable interest income: (Form 1099-Int, Box 1)",
                          value = 0, min=0)
      )       
    ),
    hr(),
    # Ordinary and Qualified Dividends
    fluidRow(
      column(5, 
             numericInput(ns("ordinaryDividends_2018"), label = "Enter your ordinary dividends income: (Form 1099-DIV)",
                          value = 0, min=0),
             numericInput(ns("qualifiedDividends_2018"), label = "Enter your qualified dividends income: (Form 1099-DIV)",
                          value = 0, min=0)
      ),
      column(5, 
             numericInput(ns("ordinaryDividends_2017"), label = "Enter your ordinary dividends income: (Form 1099-DIV)",
                          value = 0, min=0),
             numericInput(ns("qualifiedDividends_2017"), label = "Enter your qualified dividends income: (Form 1099-DIV)",
                          value = 0, min=0)
      )      
    ),
    hr(),
    # Capital Gain
    fluidRow(
      column(4,
              numericInput(ns("LTGain_2018"), label = "Enter your long-term capital gains (loss):", value=0 ),
              numericInput(ns("STGain_2018"), label = "Enter your short-term capital gains (loss)", value = 0)
      ),
      column(4,
              numericInput(ns("LTGain_2017"), label = "Enter your long-term capital gains (loss):", value=0),
              numericInput(ns("STGain_2017"), label = "Enter your short-term capital gains (loss):", value = 0)
      ), 
      column(3,tags$strong("Help:"), a(href= "https://www.irs.gov/taxtopics/tc409", "Topic No. 409 - Capital Gains and Losses"))
    ),
    hr(),
    # IRA Distributions
    fluidRow(
      column(4, 
            numericInput(ns("IRADist_2018"), label = "Enter your taxable distribution IRA:", value = 0, min= 0),
            checkboxInput(ns("IRAException_2018"), label = "Check if you meet exception to 10% additional tax", value = FALSE)
      ),
      column(4, 
          numericInput(ns("IRADist_2017"), label = "Enter your taxable distribution IRA:", value = 0, min= 0),
          checkboxInput(ns("IRAException_2017"), label = "Check if you meet exception to 10% additional tax", value = FALSE)
      ),
      column(3, tags$strong("Help:"), 
             a(href="https://www.irs.gov/retirement-plans/plan-participant-employee/retirement-topics-tax-on-early-distributions",
              "Exception to Tax on Early Distributions")
      )
    )
  )
} # finish incomeUI function

income <- function (input, output, session ){
  observeEvent(input$hideWages1_2018,{
      # hide additional wages 1 and reset value to zero, and hide the button after it done
      #session$sendCustomMessage (type = "testmessage", message = "Testing")
      hide (id= "addWages1_2018")
      #updateNumericInput(session,inputId = input$addWages1_2018, value = 0)
      hide (id = "hideWages1_2018")
  })
  observeEvent(input$hideWages2_2018, {
      # hide additional wages 2 and reset value to zero, and hide the button after it done
      hide (id= "addWages2_2018")
      #updateNumericInput(session, inputId = input$addWages2_2018, value = 0)
      hide (id = "hideWages2_2018")
  })
  observeEvent(input$hideWages1_2017,{
      # hide additional wages 1 and reset value to zero, and hide the button after it done
      hide (id= "addWages1_2017")
      #updateNumericInput(session, inputId = input$addWages1_2017, value = 0)
      hide (id = "hideWages1_2017")
  })
  observeEvent (input$hideWages2_2017, {
      # hide additional wages 2 and reset value to zero, and hide the button after it done
      hide (id= "addWages2_2017")
      #updateNumericInput(session,inputId = input$addWages2_2017, value = 0)
      hide (id = "hideWages2_2017")
  })
  incomeDF <- reactive({
    rowNames <- c("Your_Wages", "Spouse_Wages", "Additional_Wages_1", "Additional_Wages_2",
                  "Interest", "Ordinary_Dividends","Qualified_Dividends", "Long_Term_Gains", "Short_Term_Gains",
                  "TaxableIRA", "Exception")
    income_2018 <- c(input$yourWages_2018,
                     input$spouseWages_2018,
                     input$addWages1_2018,
                     input$addWages2_2018,
                     input$interest_2018,
                     input$ordinaryDividends_2018,
                     input$qualifiedDividends_2018,
                     input$LTGain_2018,
                     input$STGain_2018,
                     input$IRADist_2018,
                     input$IRAException_2018)
    income_2017 <- c(input$yourWages_2017,
                     input$spouseWages_2017,
                     input$addWages1_2017,
                     input$addWages2_2017,
                     input$interest_2017,
                     input$ordinaryDividends_2017,
                     input$qualifiedDividends_2017,
                     input$LTGain_2017,
                     input$STGain_2017,
                     input$IRADist_2017,
                     input$IRAException_2017)
    return (data.frame(income_2018, income_2017,row.names = rowNames))
  })
}
