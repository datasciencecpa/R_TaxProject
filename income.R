incomeUI <- function (id){
  ns <- NS(id)
  tabPanel("Incomes",

    # --Wages section------------------------------------------------------------------------------------------------------------
    h3("Your W-2 Wages"),
    fluidRow(
      column(6,numericInput(ns("yourWages"), label = "Enter your wages from W-2, box 1:", value = 0)),
      column(6,numericInput(ns("yourW2Tax"), label = "Enter your income tax withheld, box 2:", value = 0))
    ),# Your Wages row
    fluidRow(
      column(6,numericInput(ns("yourMedicareW2"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
      column(6,numericInput(ns("yourMedicareTax"), label = "Enter your medicare tax withheld, box 6:", value = 0))
    ), # Medicare Wage & Tax Row
    fluidRow(
      column(6,numericInput(ns("spouseWages"), label = "Enter spouse wages from W-2, box 1:", value = 0)),
      column(6,numericInput(ns("spouseW2Tax"), label = "Enter spouse income tax withheld, box 2:", value = 0))
    ), # Spouse Wages
    fluidRow(
      column(6,numericInput(ns("spouseMedicareW2"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
      column(6,numericInput(ns("spouseMedicareTax"), label = "Enter your medicare tax withheld, box 6:", value = 0))
    ),# Spouse Medicare 
    hr(),
    h3("Your Additional W-2:"),
    fluidRow(
      column(6,numericInput(ns("addWages1"), label = "Enter additional wages from W-2, box 1:", value = 0)),
      column(6,numericInput(ns("addW2Tax1"), label = "Enter income tax from W-2, box 2:", value = 0))
    ), # Additional wages 1
    fluidRow(
      column(6,numericInput(ns("addMedicare1"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
      column(6,numericInput(ns("addMedicareTax1"), label = "Enter your medicare tax withheld, box 6:", value = 0))
    ), # Additional medicare wages and taxes
    fluidRow(
      actionButton(ns("hideWages1"), label = "Delete this W-2", class="btn btn-danger btn-responsive")
    ), # Action Button
    hr(),
    h3("Spouse Additional W-2:"),
    fluidRow(
      column(6,numericInput(ns("addWages2"), label = "Enter additional wages from W-2, box 1:", value = 0)),
      column(6,numericInput(ns("addW2Tax2"), label = "Enter income tax from W-2, box 2:", value = 0))
    ), # Additional Wages 2
    fluidRow(
      column(6,numericInput(ns("addMedicare2"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
      column(6,numericInput(ns("addMedicareTax2"), label = "Enter your medicare tax withheld, box 6:", value = 0))
    ), # Additional Medicare wages and tax 2
    fluidRow(
      actionButton(ns("hideWages2"), label = "Delete this W-2",class="btn btn-danger btn-responsive")
    ),   
    #-- End Wages Section ------------------------------------------------------------------------------------------------------------
    # Interest Section --------------------------------------------------------------------------------------------------------------------
    hr(),
    h3("Taxable Interest Income"),
    fluidRow(
        column(6,
            numericInput(ns("interest"), label = "Enter your taxable interest income: (Form 1099-Int, Box 1)",
                            value = 0, min=0)), 
        column(6,
            numericInput(ns("interestTax"), label = "Income tax withheld from interest income:", value = 0, min=0))
    ),
    # End Interest Section -----------------------------------------------------------------------------------------------------------------
   
    # Ordinary and Qualified Dividends------------------------------------------------------------------------
    hr(),h3("Dividends Income"),
    fluidRow(
      column(4, 
           numericInput(ns("ordinaryDividends"), label = "Enter your ordinary dividends income: (Form 1099-DIV)",
                        value = 0, min=0)),
      column(4,
           numericInput(ns("qualifiedDividends"), label = "Enter your qualified dividends income: (Form 1099-DIV)",
                        value = 0, min=0)),
      column(4,
           numericInput(ns("dividendTax"), label = "Enter your dividend income tax withheld: (Form 1099-DIV)",
                        value = 0, min=0))
    ),
    # - End Dividends section-------------------------------------------------------------------------------------------------------------
    hr(), h3("Taxable Refunds"), #Taxable Refunds
    helpText("Report taxable refunds, credits, or offsets of State and Local Income Taxes only if they are taxable.
             If you did not itemize deductions, or elected to deduct state and local general sales taxes instead of state and local income taxes -
             your refund is not taxable. Enter zero below if this applied to you."),
    fluidRow(
       numericInput(ns("taxRefund"), label = "Taxable refunds, credits, or offsets of state and local income taxes", value = 0)
    ),
    # - End Taxable Refunds section ------------------------------------------------------------------------------------------------------------
    hr(), h3("Alimony Income"), #Alimony received
    helpText("Your alimony income may not be taxable in 2018. If this is the case, enter zero for 2018 below. See ", 
             a(href="https://www.irs.gov/forms-pubs/about-publication-504", "IRS Publication 504"), " for detail."),
    fluidRow(numericInput(ns("alimony"), label = "Alimony received", value = 0)
    ),
    #-- End Alimony section ---------------------------------------------------------------------------------------------------------------------
    hr(), h3("Capital Gains/Losses"),
    # Capital Gains
    helpText("Enter amount of net short-term and/or long-term capital gains/losses as showed on Schedule D to boxes below.
             This app won't calculate special capital gains such as section 1250 gain or collectible gain."),
    fluidRow(
      column(9, 
            numericInput(ns("LTGain"), label = "Enter your long-term capital gains (loss):", value=0 ),
            numericInput(ns("STGain"), label = "Enter your short-term capital gains (loss)", value = 0),
            numericInput(ns("capitalTax"), label = "Enter your income tax withheld:", value = 0)
      ),
      column(3,tags$strong("Help:"), a(href= "https://www.irs.gov/taxtopics/tc409", "Topic No. 409 - Capital Gains and Losses"))
    ),
    # --- End Capital Gains section------------------------------------------------------------------------------------------------------------
    hr(), h3("IRA Distribution Income"),
    # IRA Distributions
    fluidRow(
      column(9,
            numericInput(ns("IRADist"), label = "Enter your taxable distribution IRA:", value = 0, min= 0),
            numericInput(ns("IRATax"), label = "Enter your income tax withheld:", value = 0),
            checkboxInput(ns("IRAException"), label = "Check if you meet exception to 10% additional tax", value = FALSE)
      ),
      column(3, tags$strong("Help:"), 
             a(href="https://www.irs.gov/retirement-plans/plan-participant-employee/retirement-topics-tax-on-early-distributions",
              "Exception to Tax on Early Distributions")
      )
    ), 
    # End IRA Distributions -------------------------------------------------------------------------------------------------------------------
    hr(),h3("Unemployment Income"),# Unemployment income 
    fluidRow(
        column(6, numericInput(ns("unemployment"), label= "Unemployment income received", value = 0)),
        column(6, numericInput(ns("unemploymentTax"), label= "Unemployment income tax withheld", value = 0))
    )
  )
} # finish incomeUI function

income <- function (input, output, session ){
  
  
  observeEvent(input$hideWages1,{
      # hide additional wages 1 and reset value to zero, and hide the button after it done
      hideshow(c("addWages1", "addW2Tax1", "addMedicare1", "addMedicareTax1","hideWages1"), hide = TRUE)
      updateNumInput_2(session, c("addWages1", "addW2Tax1","addMedicare1", "addMedicareTax1"),
                     values = c(0,0,0,0))
  })
  observeEvent(input$hideWages2, {
      # hide additional wages 2 and reset value to zero, and hide the button after it done

      hideshow(c("addWages2", "addW2Tax2", "addMedicare2","addMedicareTax2","hideWages2"), hide = TRUE)
      updateNumInput_2(session, c("addWages2","addW2Tax2","addMedicare2","addMedicareTax2"),
                     c(0,0,0,0))
  })

  incomeDF <- reactive({
    rowNames <- c("Your_Wages", "Your_Medicare_Wage", "Spouse_Wages", "Spouse_Medicare_Wage", "Additional_Wages_1", "Add_Medicare_Wage1", "Additional_Wages_2",
                  "Add_Medicare_Wage2", "Interest", "Ordinary_Dividends","Qualified_Dividends","Tax_Refunds", "Alimony", "Long_Term_Gains", "Short_Term_Gains",
                  "TaxableIRA", "Exception", "Unemployment_Income", "Your_W2_Tax", "Your_Medicare_Tax", "Spouse_W2_Tax","Spouse_Medicare_Tax", 
                  "Additional_W2_Tax_1","Add_Medicare_Tax1", "Additional_W2_Tax_2", "Add_Medicare_Tax2",
                  "Interest_Tax", "Dividend_Tax", "Capital_Gain_Tax", "IRA_Tax", "Unemployment_Tax")
    Income_Tax_2018 <- c(input$yourWages,
                         input$yourMedicareW2,
                         input$spouseWages,
                         input$spouseMedicareW2,
                         input$addWages1,
                         input$addMedicare1,
                         input$addWages2,
                         input$addMedicare2,
                         input$interest,
                         input$ordinaryDividends,
                         input$qualifiedDividends,
                         input$taxRefund,
                         input$alimony,
                         input$LTGain,
                         input$STGain,
                         input$IRADist,
                         as.numeric(input$IRAException),
                         input$unemployment,
                         input$yourW2Tax,
                         input$yourMedicareTax,
                         input$spouseW2Tax,
                         input$spouseMedicareTax,
                         input$addW2Tax1,
                         input$addMedicareTax1,
                         input$addW2Tax2,
                         input$addMedicareTax2,
                         input$interestTax, 
                         input$dividendTax,
                         input$capitalTax,
                         input$IRATax,
                         input$unemploymentTax
                         )
  
    dfIncome <- data.frame(Income_Tax_2018,row.names = rowNames, stringsAsFactors = FALSE)
    dfIncome[is.na.data.frame(dfIncome)] <- 0
    return (dfIncome)
  })
}
