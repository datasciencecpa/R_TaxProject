incomeUI <- function (id){
  ns <- NS(id)
  tabPanel("Income",
    # Header

    fluidRow(
      column(6,checkboxInput(ns("same"), label = "Applied everything from 2018 to 2017", value = TRUE )),
      column(5, helpText("Check this box if you want to apply the same inputs from 2018 to 2017. Uncheck this box if you would like to make change to 2017"))
    ), hr(),
    # Wages
    fluidRow(  
      column(6, h4("2018"),
        column(6, 
           numericInput(ns("yourWages_2018"), label = "Enter your wages from W-2, box 1:", value = 0, min=0),
           numericInput(ns("spouseWages_2018"), label = "Enter spouse wages from W-2, box 1:", value = 0, min=0),
           numericInput(ns("addWages1_2018"), label = "Enter additional wages from W-2, box 1:", value = 0, min=0),
           actionButton(ns("hideWages1_2018"), label = "Delete", class="btn btn-danger btn-responsive"),
           numericInput(ns("addWages2_2018"), label = "Enter additional wages from W-2, box 1:", value = 0, min=0),
           actionButton(ns("hideWages2_2018"), label = "Delete",class="btn btn-danger btn-responsive")
        ),
        column(6, 
          numericInput(ns("yourW2Tax_2018"), label = "Enter your income tax withheld, box 2:", value = 0, min =0),
          numericInput(ns("spouseW2Tax_2018"), label = "Enter spouse income tax withheld, box 2:", value = 0, min= 0),
          numericInput(ns("addW2Tax1_2018"), label = "Enter income tax from W-2, box 2:", value = 0, min = 0),
          numericInput(ns("addW2Tax2_2018"), label = "Enter income tax from W-2, box 2:", value = 0, min = 0)
      )
             
      ),
      column(6,h4("2017"),
        column(6,
           numericInput(ns("yourWages_2017"), label = "Enter your wages from W-2, box 1:", value = 0, min=0),
           numericInput(ns("spouseWages_2017"), label = "Enter spouse wages from W-2, box 1:", value = 0, min=0),
           numericInput(ns("addWages1_2017"), label = "Enter additional wages from W-2, box 1:", value = 0, min=0),
           actionButton(ns("hideWages1_2017"), label = "Delete",class="btn btn-danger btn-responsive"),
           numericInput(ns("addWages2_2017"), label = "Enter additional wages from W-2, box 1:", value = 0, min=0),
           actionButton(ns("hideWages2_2017"), label = "Delete", class="btn btn-danger btn-responsive")
        ),
       column(6,
          numericInput(ns("yourW2Tax_2017"), label = "Enter your income tax withheld, box 2:", value = 0, min =0),
          numericInput(ns("spouseW2Tax_2017"), label = "Enter spouse income tax withheld, box 2:", value = 0, min= 0),
          numericInput(ns("addW2Tax1_2017"), label = "Enter income tax from W-2, box 2:", value = 0, min = 0),
          numericInput(ns("addW2Tax2_2017"), label = "Enter income tax from W-2, box 2:", value = 0, min = 0)    
        )
      )
    ),
    hr(),
    # Interest
    fluidRow(
      column(6, h4("2018"), 
        column(6,
            numericInput(ns("interest_2018"), label = "Enter your taxable interest income: (Form 1099-Int, Box 1)",
                            value = 0, min=0)
        ), 
        column(6,
            numericInput(ns("interestTax_2018"), label = "Income tax withheld from interest income:", value = 0, min=0)
        )
      ),
      column(6,h4("2017"), 
        column(6,
            numericInput(ns("interest_2017"), label = "Enter your taxable interest income: (Form 1099-Int, Box 1)",
                          value = 0, min=0)
        ),
        column(6,
            numericInput(ns("interestTax_2017"), label = "Income tax withheld from interest income:", value = 0, min=0)     
        )
      )       
    ),
    hr(),
    # Ordinary and Qualified Dividends
    fluidRow(
      column(6, h4("2018"),
           numericInput(ns("ordinaryDividends_2018"), label = "Enter your ordinary dividends income: (Form 1099-DIV)",
                        value = 0, min=0),
           numericInput(ns("qualifiedDividends_2018"), label = "Enter your qualified dividends income: (Form 1099-DIV)",
                        value = 0, min=0),
           
           numericInput(ns("dividendTax_2018"), label = "Enter your dividend income tax withheld: (Form 1099-DIV)",
                        value = 0, min=0)
      ),
      column(6, h4("2017"),
           numericInput(ns("ordinaryDividends_2017"), label = "Enter your ordinary dividends income: (Form 1099-DIV)",
                        value = 0, min=0),
           numericInput(ns("qualifiedDividends_2017"), label = "Enter your qualified dividends income: (Form 1099-DIV)",
                        value = 0, min=0),
           numericInput(ns("dividendTax_2017"), label = "Enter your dividend income tax withheld: (Form 1099-DIV)",
                        value = 0, min=0)
      )      
    ),
    hr(),
    # Capital Gain
    fluidRow(
      column(4, h4("2018"),
            numericInput(ns("LTGain_2018"), label = "Enter your long-term capital gains (loss):", value=0 ),
            numericInput(ns("STGain_2018"), label = "Enter your short-term capital gains (loss)", value = 0),
            numericInput(ns("capitalTax_2018"), label = "Enter your income tax withheld:", value = 0)
      ),
      column(4,h4("2017"),
            numericInput(ns("LTGain_2017"), label = "Enter your long-term capital gains (loss):", value=0),
            numericInput(ns("STGain_2017"), label = "Enter your short-term capital gains (loss):", value = 0),
            numericInput(ns("capitalTax_2017"), label = "Enter your income tax withheld:", value = 0)
      ), 
      column(4,tags$strong("Help:"), a(href= "https://www.irs.gov/taxtopics/tc409", "Topic No. 409 - Capital Gains and Losses"))
    ),
    hr(),
    # IRA Distributions
    fluidRow(
      column(4, h4("2018"),
            numericInput(ns("IRADist_2018"), label = "Enter your taxable distribution IRA:", value = 0, min= 0),
            numericInput(ns("IRATax_2018"), label = "Enter your income tax withheld:", value = 0),
            checkboxInput(ns("IRAException_2018"), label = "Check if you meet exception to 10% additional tax", value = FALSE)
      ),
      column(4, h4("2017"),
            numericInput(ns("IRADist_2017"), label = "Enter your taxable distribution IRA:", value = 0, min= 0),
            numericInput(ns("IRATax_2017"), label = "Enter your income tax withheld:", value = 0),
            checkboxInput(ns("IRAException_2017"), label = "Check if you meet exception to 10% additional tax", value = FALSE)
      ),
      column(4, tags$strong("Help:"), 
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
      hide (id= "addW2Tax1_2018")
      updateNumericInput(session, inputId = "addWages1_2018", value = 0)
      updateNumericInput(session, inputId = "addW2Tax1_2018", value = 0)
      hide (id = "hideWages1_2018")
  })
  observeEvent(input$hideWages2_2018, {
      # hide additional wages 2 and reset value to zero, and hide the button after it done
      hide (id= "addWages2_2018")
      hide (id= "addW2Tax2_2018")
      updateNumericInput(session, inputId ="addWages2_2018", value = 0)
      updateNumericInput(session, inputId = "addW2Tax2_2018", value = 0)
      hide (id = "hideWages2_2018")
  })
  observeEvent(input$hideWages1_2017,{
      # hide additional wages 1 and reset value to zero, and hide the button after it done
      hide (id= "addWages1_2017")
      hide (id= "addW2Tax1_2017")
      updateNumericInput(session, inputId = "addWages1_2017", value = 0)
      updateNumericInput(session, inputId = "addW2Tax1_2017", value = 0)
      hide (id = "hideWages1_2017")
  })
  observeEvent (input$hideWages2_2017, {
      # hide additional wages 2 and reset value to zero, and hide the button after it done
      hide (id= "addWages2_2017")
      hide (id= "addW2Tax2_2017")
      updateNumericInput(session,inputId = "addWages2_2017", value = 0)
      updateNumericInput(session, inputId = "addW2Tax2_2017", value = 0)
      hide (id = "hideWages2_2017")
  })
  observe({  # Use this to handle check box checked.
    if(input$same){

      updateNumericInput(session, "yourWages_2017",label = "Enter your wages from W-2, box 1:", value = input$yourWages_2018, min=0)
      updateNumericInput(session, "yourW2Tax_2017", label = "Enter your income tax withheld, box 2:", value = input$yourW2Tax_2018, min =0)
      updateNumericInput(session, "spouseWages_2017",label = "Enter spouse wages from W-2, box 1:", value = input$spouseWages_2018, min=0 )
      updateNumericInput(session, "spouseW2Tax_2017", label = "Enter spouse income tax withheld, box 2:", value = input$spouseW2Tax_2018, min =0)
      updateNumericInput(session, "addWages1_2017",label = "Enter additional wages from W-2, box 1:", value = input$addWages1_2018, min=0 )
      updateNumericInput(session, "addW2Tax1_2017", label = "Enter income tax from W-2, box 2:", value = input$addW2Tax1_2018, min =0)
      updateNumericInput(session, "addWages2_2017",label = "Enter additional wages from W-2, box 1:", value = input$addWages2_2018, min=0 )
      updateNumericInput(session, "addW2Tax2_2017", label = "Enter income tax from W-2, box 2:", value = input$addW2Tax2_2018, min =0)
      updateNumericInput(session, "interest_2017",label = "Enter your taxable interest income: (Form 1099-Int, Box 1)", 
                         value = input$interest_2018, min=0 )
      updateNumericInput(session, "interestTax_2017", label = "Income tax withheld from interest income", value = input$interestTax_2018)
      updateNumericInput(session, "ordinaryDividends_2017",label = "Enter your ordinary dividends income: (Form 1099-DIV)", 
                         value = input$ordinaryDividends_2018, min=0 )
      updateNumericInput(session, "dividendTax_2017", label = "Enter your dividend income tax withheld: (Form 1099-DIV)", value = input$dividendTax_2018)
      
      updateNumericInput(session, "qualifiedDividends_2017",label = "Enter your qualified dividends income: (Form 1099-DIV)", 
                         value = input$qualifiedDividends_2018, min=0)
      updateNumericInput(session, "LTGain_2017",label = "Enter your long-term capital gains (loss):", 
                        value = input$LTGain_2018, min=0)
      updateNumericInput(session, "STGain_2017",label = "Enter your long-term capital gains (loss):", 
                         value = input$STGain_2018, min=0)
      updateNumericInput(session, "capitalTax_2017", label = "Enter your income tax withheld:", value = input$capitalTax_2018)
      updateNumericInput(session, "IRADist_2017",label = "Enter your taxable distribution IRA:", 
                         value = input$IRADist_2018, min=0)
      updateNumericInput(session, "IRATax_2017", label = "Enter your income tax withheld:", value = input$IRATax_2018)
      updateCheckboxInput(session, "IRAException_2017", label = "Check if you meet exception to 10% additional tax", value = input$IRAException_2018 )
    }
  })
  incomeDF <- reactive({
    rowNames <- c("Your_Wages", "Spouse_Wages", "Additional_Wages_1", "Additional_Wages_2",
                  "Interest", "Ordinary_Dividends","Qualified_Dividends", "Long_Term_Gains", "Short_Term_Gains",
                  "TaxableIRA", "Exception", "Your_W2_Tax", "Spouse_W2_Tax", "Additional_W2_Tax_1", "Additional_W2_Tax_2",
                  "Interest_Tax", "Dividend_Tax", "Capital_Gain_Tax", "IRA_Tax")
    Income_Tax_2018 <- c(input$yourWages_2018,
                     input$spouseWages_2018,
                     input$addWages1_2018,
                     input$addWages2_2018,
                     input$interest_2018,
                     input$ordinaryDividends_2018,
                     input$qualifiedDividends_2018,
                     input$LTGain_2018,
                     input$STGain_2018,
                     input$IRADist_2018,
                     as.numeric(input$IRAException_2018),
                     input$yourW2Tax_2018,
                     input$spouseW2Tax_2018,
                     input$addW2Tax1_2018,
                     input$addW2Tax2_2018,
                     input$interestTax_2018, 
                     input$dividendTax_2018,
                     input$capitalTax_2018,
                     input$IRATax_2018
                     )
    Income_Tax_2017 <- c(input$yourWages_2017,
                     input$spouseWages_2017,
                     input$addWages1_2017,
                     input$addWages2_2017,
                     input$interest_2017,
                     input$ordinaryDividends_2017,
                     input$qualifiedDividends_2017,
                     input$LTGain_2017,
                     input$STGain_2017,
                     input$IRADist_2017,
                     as.numeric(input$IRAException_2017),
                     input$yourW2Tax_2017,
                     input$spouseW2Tax_2017,
                     input$addW2Tax1_2017,
                     input$addW2Tax2_2017,
                     input$interestTax_2017, 
                     input$dividendTax_2017,
                     input$capitalTax_2017,
                     input$IRATax_2017)

    return (data.frame(Income_Tax_2018, Income_Tax_2017,row.names = rowNames))
  })
}
