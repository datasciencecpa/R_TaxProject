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
        fluidRow(
          column(6,numericInput(ns("yourWages_2018"), label = "Enter your wages from W-2, box 1:", value = 0)),
          column(6,numericInput(ns("yourW2Tax_2018"), label = "Enter your income tax withheld, box 2:", value = 0))
        ),# Wages row
        fluidRow(
          column(6,numericInput(ns("yourMedicareW2_2018"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
          column(6,numericInput(ns("yourMedicareTax_2018"), label = "Enter your medicare tax withheld, box 6:", value = 0))
          
        ), # Medicare Wage & Tax Row
        fluidRow(
          column(6,numericInput(ns("spouseWages_2018"), label = "Enter spouse wages from W-2, box 1:", value = 0)),
          column(6,numericInput(ns("spouseW2Tax_2018"), label = "Enter spouse income tax withheld, box 2:", value = 0))
        ), # Spouse Wages
        fluidRow(
          column(6,numericInput(ns("spouseMedicareW2_2018"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
          column(6,numericInput(ns("spouseMedicareTax_2018"), label = "Enter your medicare tax withheld, box 6:", value = 0))
        ),# Spouse Medicare 
        fluidRow(
          column(6,numericInput(ns("addWages1_2018"), label = "Enter additional wages from W-2, box 1:", value = 0)),
          column(6,numericInput(ns("addW2Tax1_2018"), label = "Enter income tax from W-2, box 2:", value = 0))
        ), # Additional wages 1
        fluidRow(
          column(6,numericInput(ns("addMedicare1_2018"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
          column(6,numericInput(ns("addMedicareTax1_2018"), label = "Enter your medicare tax withheld, box 6:", value = 0))
                 
        ), # Additional medicare wages and taxes
        fluidRow(
          actionButton(ns("hideWages1_2018"), label = "Delete", class="btn btn-danger btn-responsive")
        ), # Action Button
        fluidRow(
          column(6,numericInput(ns("addWages2_2018"), label = "Enter additional wages from W-2, box 1:", value = 0)),
          column(6,numericInput(ns("addW2Tax2_2018"), label = "Enter income tax from W-2, box 2:", value = 0))
        ), # Additional Wages 2
        fluidRow(
          column(6,numericInput(ns("addMedicare2_2018"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
          column(6,numericInput(ns("addMedicareTax2_2018"), label = "Enter your medicare tax withheld, box 6:", value = 0))
        ), # Additional Medicare wages and tax 2
        fluidRow(
          actionButton(ns("hideWages2_2018"), label = "Delete",class="btn btn-danger btn-responsive")
        )   
      ),
      column(6,h4("2017"),
        fluidRow(
          column(6,numericInput(ns("yourWages_2017"), label = "Enter your wages from W-2, box 1:", value = 0)),
          column(6,numericInput(ns("yourW2Tax_2017"), label = "Enter your income tax withheld, box 2:", value = 0))
        ), # Your Wages
        fluidRow(
          column(6,numericInput(ns("yourMedicareW2_2017"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
          column(6,numericInput(ns("yourMedicareTax_2017"), label = "Enter your medicare tax withheld, box 6:", value = 0))
        ), # Your medicare wages & tax
        fluidRow(
          column(6,numericInput(ns("spouseWages_2017"), label = "Enter spouse wages from W-2, box 1:", value = 0)),
          column(6,numericInput(ns("spouseW2Tax_2017"), label = "Enter spouse income tax withheld, box 2:", value = 0))
        ), # Spouse Wages
        fluidRow(
          column(6,numericInput(ns("spouseMedicareW2_2017"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
          column(6,numericInput(ns("spouseMedicareTax_2017"), label = "Enter your medicare tax withheld, box 6:", value = 0))
        ), # Spouse medicare wages and tax
        fluidRow(
          column(6,numericInput(ns("addWages1_2017"), label = "Enter additional wages from W-2, box 1:", value = 0)),
          column(6,numericInput(ns("addW2Tax1_2017"), label = "Enter income tax from W-2, box 2:", value = 0))
        ), # Additional wages 1
        fluidRow(
          column(6,numericInput(ns("addMedicare1_2017"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
          column(6,numericInput(ns("addMedicareTax1_2017"), label = "Enter your medicare tax withheld, box 6:", value = 0))
        ), # Additional medicare wages and tax
        fluidRow(
          actionButton(ns("hideWages1_2017"), label = "Delete",class="btn btn-danger btn-responsive")
        ), # Action button 1
        fluidRow(
          column(6, numericInput(ns("addWages2_2017"), label = "Enter additional wages from W-2, box 1:", value = 0)),
          column(6,numericInput(ns("addW2Tax2_2017"), label = "Enter income tax from W-2, box 2:", value = 0))
        ), # Additional Wages 2
        fluidRow(
          column(6,numericInput(ns("addMedicare2_2017"), label = "Enter your medicare wages from W-2, box 5:", value = 0)),
          column(6,numericInput(ns("addMedicareTax2_2017"), label = "Enter your medicare tax withheld, box 6:", value = 0))
        ), # Additional medicare 2 wages and tax
        fluidRow(
          actionButton(ns("hideWages2_2017"), label = "Delete", class="btn btn-danger btn-responsive")
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
    hr(), #Taxable Refunds
    helpText("Report taxable refunds, credits, or offsets of State and Local Income Taxes only if they are taxable.
             If you did not itemize deductions, or elected to deduct state and local general sales taxes instead of state and local income taxes -
             your refund is not taxable. Enter zero below if this applied to you."),
    fluidRow(
      column(6, h4("2018"),
             numericInput(ns("taxRefund_2018"), label = "Taxable refunds, credits, or offsets of state and local income taxes", value = 0)),
      column(6, h4("2017"),
             numericInput(ns("taxRefund_2017"), label = "Taxable refunds, credits, or offsets of state and local income taxes", value = 0))
    ), hr(), #Alimony received
    helpText("Your alimony income may not be taxable in 2018. If this is the case, enter zero for 2018 below. See ", 
             a(href="https://www.irs.gov/forms-pubs/about-publication-504", "IRS Publication 504"), " for detail."),
    fluidRow(
      column(6, h4("2018"),
             numericInput(ns("alimony_2018"), label = "Alimony received", value = 0)),
      column(6, h4("2017"),
             numericInput(ns("alimony_2017"), label = "Alimony received", value = 0))
    ), hr(),
    # Capital Gain
    helpText("Enter amount of net short-term and/or long-term capital gain/loss as showed on Schedule D to box below.
             This app won't calculate special capital gain such as section 1250 gain or collectible gain."),
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
    ), hr(), # Unemployment income 
    fluidRow(
      column(6, h4("2018"),
        column(6, numericInput(ns("unemployment_2018"), label= "Unemployment income received", value = 0)),
        column(6, numericInput(ns("unemploymentTax_2018"), label= "Unemployment income tax withheld", value = 0))
      ),
      column(6, h4("2017"),
             column(6, numericInput(ns("unemployment_2017"), label= "Unemployment income received", value = 0)),
             column(6, numericInput(ns("unemploymentTax_2017"), label= "Unemployment income tax withheld", value = 0))
      )
    )
  )
} # finish incomeUI function

income <- function (input, output, session ){
  
  
  observeEvent(input$hideWages1_2018,{
      # hide additional wages 1 and reset value to zero, and hide the button after it done
      hideshow(c("addWages1_2018", "addW2Tax1_2018", "addMedicare1_2018", "addMedicareTax1_2018","hideWages1_2018"), hide = TRUE)
      updateNumInput(session, c("addWages1_2018", "addW2Tax1_2018","addMedicare1_2018", "addMedicareTax1_2018"),
                     values = c(0,0,0,0))
  })
  observeEvent(input$hideWages2_2018, {
      # hide additional wages 2 and reset value to zero, and hide the button after it done

      hideshow(c("addWages2_2018", "addW2Tax2_2018", "addMedicare2_2018","addMedicareTax2_2018","hideWages2_2018"), hide = TRUE)
      updateNumInput(session, c("addWages2_2018","addW2Tax2_2018","addMedicare2_2018","addMedicareTax2_2018"),
                     c(0,0,0,0))
  })
  observeEvent(input$hideWages1_2017,{
      # hide additional wages 1 and reset value to zero, and hide the button after it done
  
      hideshow(c("addWages1_2017", "addW2Tax1_2017", "addMedicare1_2017", "addMedicareTax1_2017","hideWages1_2017"), hide = TRUE)
      updateNumInput(session, c("addWages1_2017","addW2Tax1_2017","addMedicare1_2017","addMedicareTax1_2017"),
                     c(0,0,0,0))
  })
  observeEvent (input$hideWages2_2017, {
      # hide additional wages 2 and reset value to zero, and hide the button after it done
      hideshow(c("addWages2_2017", "addW2Tax2_2017","addMedicare2_2017", "addMedicareTax2_2017","hideWages2_2017"), hide = TRUE)
      updateNumInput(session, c("addWages2_2017","addW2Tax2_2017","addMedicare2_2017","addMedicareTax2_2017"),
                     c(0,0,0,0))
  })
  observe({  # Use this to handle check box checked.
    if(input$same){
      updateNumericInput(session, "yourMedicareW2_2017", label = "Enter your medicare wages from W-2, box 5:", value = input$yourMedicareW2_2018)
      updateNumericInput(session, "yourMedicareTax_2017", label ="Enter your medicare tax withheld, box 6:", value = input$yourMedicareTax_2018)
      updateNumericInput(session, "spouseMedicareW2_2017", label = "Enter your medicare wages from W-2, box 5:", value = input$spouseMedicareW2_2018)
      updateNumericInput(session, "spouseMedicareTax_2017", label = "Enter your medicare tax withheld, box 6:", value = input$spouseMedicareTax_2018)
      updateNumericInput(session, "addMedicare1_2017", label = "Enter your medicare wages from W-2, box 5:", value = input$addMedicare1_2018)
      updateNumericInput(session, "addMedicareTax1_2017", label = "Enter your medicare tax withheld, box 6::", value = input$addMedicareTax1_2018)
      updateNumericInput(session, "addWages1_2017",label = "Enter additional wages from W-2, box 1:", value = input$addWages1_2018 )
      updateNumericInput(session, "addW2Tax1_2017", label = "Enter income tax from W-2, box 2:", value = input$addW2Tax1_2018)
      updateNumericInput(session, "addWages2_2017",label = "Enter additional wages from W-2, box 1:", value = input$addWages2_2018 )
      updateNumericInput(session, "addW2Tax2_2017", label = "Enter income tax from W-2, box 2:", value = input$addW2Tax2_2018)
      updateNumericInput(session, "addMedicare2_2017", label = "Enter your medicare wages from W-2, box 5:", value = input$addMedicare2_2018)
      updateNumericInput(session, "addMedicareTax2_2017", label = "Enter your medicare tax withheld, box 6::", value = input$addMedicareTax2_2018)
      updateNumericInput(session, "interest_2017",label = "Enter your taxable interest income: (Form 1099-Int, Box 1)", 
                         value = input$interest_2018)
      updateNumericInput(session, "interestTax_2017", label = "Income tax withheld from interest income", value = input$interestTax_2018)
      updateNumericInput(session, "ordinaryDividends_2017",label = "Enter your ordinary dividends income: (Form 1099-DIV)", 
                         value = input$ordinaryDividends_2018)
      updateNumericInput(session, "dividendTax_2017", label = "Enter your dividend income tax withheld: (Form 1099-DIV)", value = input$dividendTax_2018)
      
      updateNumericInput(session, "qualifiedDividends_2017",label = "Enter your qualified dividends income: (Form 1099-DIV)", 
                         value = input$qualifiedDividends_2018)
      updateNumericInput(session, "LTGain_2017",label = "Enter your long-term capital gains (loss):", 
                        value = input$LTGain_2018)
      updateNumericInput(session, "STGain_2017",label = "Enter your long-term capital gains (loss):", 
                         value = input$STGain_2018)
      updateNumericInput(session, "capitalTax_2017", label = "Enter your income tax withheld:", value = input$capitalTax_2018)
      updateNumericInput(session, "IRADist_2017",label = "Enter your taxable distribution IRA:", 
                         value = input$IRADist_2018)
      updateNumericInput(session, "IRATax_2017", label = "Enter your income tax withheld:", value = input$IRATax_2018)
      updateCheckboxInput(session, "IRAException_2017", label = "Check if you meet exception to 10% additional tax", value = input$IRAException_2018 )
      updateNumericInput(session, "taxRefund_2017", label = "Taxable refunds, credits, or offsets of state and local income taxes",
                         value = input$taxRefund_2018)
      updateNumericInput(session, "alimony_2017", label = "Alimony received", value = input$alimony_2018)
      updateNumericInput(session, "unemployment_2017", label = "Unemployment income received", value = input$unemployment_2018)
      updateNumericInput(session, "unemploymentTax_2017", label = "Unemployment income tax withheld", value = input$unemploymentTax_2018)
      updateNumInput(session, c("yourWages_2017","yourW2Tax_2017","spouseWages_2017","spouseW2Tax_2017"),
                     values = c(input$yourWages_2018,input$yourW2Tax_2018,input$spouseWages_2018,input$spouseW2Tax_2018),
                     labels = c("Enter your wages from W-2, box 1:", "Enter your income tax withheld, box 2:",
                                "Enter spouse wages from W-2, box 1:","Enter spouse income tax withheld, box 2:")
      )
    }
  })
  incomeDF <- reactive({
    rowNames <- c("Your_Wages", "Your_Medicare_Wage", "Spouse_Wages", "Spouse_Medicare_Wage", "Additional_Wages_1", "Add_Medicare_Wage1", "Additional_Wages_2",
                  "Add_Medicare_Wage2", "Interest", "Ordinary_Dividends","Qualified_Dividends","Tax_Refunds", "Alimony", "Long_Term_Gains", "Short_Term_Gains",
                  "TaxableIRA", "Exception", "Unemployment_Income", "Your_W2_Tax", "Your_Medicare_Tax", "Spouse_W2_Tax","Spouse_Medicare_Tax", 
                  "Additional_W2_Tax_1","Add_Medicare_Tax1", "Additional_W2_Tax_2", "Add_Medicare_Tax2",
                  "Interest_Tax", "Dividend_Tax", "Capital_Gain_Tax", "IRA_Tax", "Unemployment_Tax")
    Income_Tax_2018 <- c(input$yourWages_2018,
                         input$yourMedicareW2_2018,
                         input$spouseWages_2018,
                         input$spouseMedicareW2_2018,
                         input$addWages1_2018,
                         input$addMedicare1_2018,
                         input$addWages2_2018,
                         input$addMedicare2_2018,
                         input$interest_2018,
                         input$ordinaryDividends_2018,
                         input$qualifiedDividends_2018,
                         input$taxRefund_2018,
                         input$alimony_2018,
                         input$LTGain_2018,
                         input$STGain_2018,
                         input$IRADist_2018,
                         as.numeric(input$IRAException_2018),
                         input$unemployment_2018,
                         input$yourW2Tax_2018,
                         input$yourMedicareTax_2018,
                         input$spouseW2Tax_2018,
                         input$spouseMedicareTax_2018,
                         input$addW2Tax1_2018,
                         input$addMedicareTax1_2018,
                         input$addW2Tax2_2018,
                         input$addMedicareTax2_2018,
                         input$interestTax_2018, 
                         input$dividendTax_2018,
                         input$capitalTax_2018,
                         input$IRATax_2018,
                         input$unemploymentTax_2018
                         )
    Income_Tax_2017 <- c(input$yourWages_2017,
                         input$yourMedicareW2_2017,
                         input$spouseWages_2017,
                         input$spouseMedicareW2_2017,
                         input$addWages1_2017,
                         input$addMedicare1_2017,
                         input$addWages2_2017,
                         input$addMedicare2_2017,
                         input$interest_2017,
                         input$ordinaryDividends_2017,
                         input$qualifiedDividends_2017,
                         input$taxRefund_2017,
                         input$alimony_2018,
                         input$LTGain_2017,
                         input$STGain_2017,
                         input$IRADist_2017,
                         as.numeric(input$IRAException_2017),
                         input$unemployment_2017,
                         input$yourW2Tax_2017,
                         input$yourMedicareTax_2017,
                         input$spouseW2Tax_2017,
                         input$spouseMedicareTax_2017,
                         input$addW2Tax1_2017,
                         input$addMedicareTax1_2017,
                         input$addW2Tax2_2017,
                         input$addMedicareTax2_2017,
                         input$interestTax_2017, 
                         input$dividendTax_2017,
                         input$capitalTax_2017,
                         input$IRATax_2017, 
                         input$unemploymentTax_2017)
    dfIncome <- data.frame(Income_Tax_2018, Income_Tax_2017,row.names = rowNames, stringsAsFactors = FALSE)
    dfIncome[is.na.data.frame(dfIncome)] <- 0
    return (dfIncome)
  })
}
