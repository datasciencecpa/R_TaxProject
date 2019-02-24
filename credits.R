creditsUI <- function (id){
  ns <- NS(id)
  tabPanel("Credits",
     fluidRow(
       column(6,checkboxInput(ns("same"), label = "Applied everything from 2018 to 2017", value = TRUE )),
       column(5, helpText("Check this box if you want to apply the same inputs from 2018 to 2017. Uncheck this box if you would like to make change to 2017"))
     ), 
     hr(),
     h3("Child and Dependent Care Expenses - See ", a(href = "https://www.irs.gov/pub/irs-pdf/i2441.pdf", "IRS link")," for more information"),
     helpText("Currently, this form does not support calculation if you received dependent care benefits from employers. If you don't have dependent care benefits,\n
              please skip to the next section."),
     fluidRow(
       column(6, h4("2018"),
          checkboxInput(ns("MFSException_2018"), label = "Check this box if you meet the MFS exception to claim this credit", value = TRUE),
          numericInput(ns("numQualifying_2018"), label = "Enter number of qualifying person", value = 0, min= 0),
          numericInput(ns("qualifiedExp_2018"), label = "Enter total of qualified expenses (Max $6000)", value = 0, min = 0),
          column(6,checkboxInput(ns("youFTStudent_2018"), label = "Check this box if you were a full-time student or disabled"),
            numericInput(ns("yourMonths_2018"), label = "Enter number of months you were full-time student or disabled", value = 0)
          ),
          column(6,checkboxInput(ns("spouseFTStudent_2018"), label = "Check this box if your spouse were a FT student or disabled"),
            numericInput(ns("spouseMonths_2018"), label = "Enter number of months your spouse were FT student or disabled", value = 0))
       ),
       column(6, h4("2017"),
          checkboxInput(ns("MFSException_2017"), label = "Check this box if you meet the MFS exception to claim this credit", value = TRUE),
          numericInput(ns("numQualifying_2017"), label = "Enter number of qualifying person", value = 0, min= 0),
          numericInput(ns("qualifiedExp_2017"), label = "Enter total of qualified expenses (Max $6000)", value = 0, min = 0),
          column(6,checkboxInput(ns("youFTStudent_2017"), label = "Check this box if you were a full-time student or disabled"),
                 numericInput(ns("yourMonths_2017"), label = "Enter number of months you were full-time student or disabled", value = 0)
          ),
          column(6,checkboxInput(ns("spouseFTStudent_2017"), label = "Check this box if your spouse were a FT student or disabled"),
                 numericInput(ns("spouseMonths_2017"), label = "Enter number of months your spouse were FT student or disabled", value = 0))
       )
     ), hr(),
     h3("Education Credits - See ", a(href= "https://www.irs.gov/pub/irs-pdf/p970.pdf", "IRS Publication 970"), " for more information")
  )
}
credits <- function(input, output, session){
  
}
