# Project: Income Tax Project
# Author: Long Nguyen
# Date: 01/19/2019
library (ggplot2)
library (shiny)
library (shinyjs)    #loading addional package to enable more UI experience
library (DT)
library (gdata)      # Use to read Excel file TaxRates.xls
library(scales)
source ("filingStatus.R")
source ("income.R")
source ("deductions.R")
source ("credits.R")
source ("Instructions.R")
source ("helper.R")
source ("incometaxCalculation.R")
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Federal Income Tax 2018 & 2017 Analysis"),
  navbarPage(tags$img(src = "navImg.jpg", class = "img-responsive", width = "50", height = "60"),
    tabPanel("Home",
       navlistPanel("Enter Your Information:",
         instructionUI("instruction"),
         filingInformationUI("filingInformation"),
         incomeUI("income"),
         deductionsUI("deductions"),
         creditsUI("credits"),
         tabPanel("Results",
                  fluidRow(
                    fluidRow(column(8,h3("Your Tax Summary")),
                        column(4, checkboxInput("hideTaxSummary", label= "Hide Tax Summary", value = FALSE))
                    ),
                    fluidRow(column(8,checkboxInput("showTaxSummaryTbl", label = "Show Tax Summary Table", value = TRUE)),
                        column(4,checkboxInput("add2017", label = "See Results Under 2017 Rules", value = FALSE))
                    ),
                    dataTableOutput("taxSummaryTbl"),
                    checkboxInput("displaySummaryGraph", label = "Display Graph", value = FALSE),
                    plotOutput("summaryGraph"),
                    checkboxInput("viewCreditChb", label = "Detail Credits", value = FALSE),
                    selectInput("creditsSelect", label = "Select Credit:", choices = list()),
                    dataTableOutput("CreditTbl"),
                    
                    hr(),
                    fluidRow( column(4,h3("Tax Planning")),
                              column(4, checkboxInput("taxPlanning", label = "Tax Planning", value = FALSE)),
                              column(4, checkboxInput("hideTaxPlanning", label = "Hide Tax Planning", value = TRUE)
                              )
                    ),
                    h4("Change in Filing Status"),
                    selectInput("filingStatus", label = "Select filing status",
                                choices = c("Single", "MFJ", "MFS", "QW",
                                            "HOH"), selected = "Single"),
                    hr(),
                    h4("Change in Dependent"),
                    sliderInput("qualifyingChildU17", label = "Number of Qualifying Child Under 17:", min= 0, max=10, value = 0, 
                                round = TRUE, width = "500px", step=1),
                    sliderInput("qualifyingChildO17", label = "Number of Qualifying Child Over 17:", min= 0, max=10, value = 0, 
                                round = TRUE, width = "500px", step=1),
                    sliderInput("qualifyingRelatives", label = "Number of Qualifying Relative", min = 0, max = 10, 
                                value = 0, round = TRUE,step = 1,width = "500px"),
                    hr(),
                    h4("Change in IRA Contribution"),
                    sliderInput("YourIRAAmountSld", label = "Your IRA Contribution", min=0, max=6500, value=0, step = 50, 
                                round = TRUE, width = "500px"),
                    sliderInput("SpouseIRAAmountSld", label = "Spouse IRA Contribution", min=0, max=6500, value=0, step = 50, 
                                round = TRUE, width = "500px"),
                    hr(),
                    h4("Change in HSA Contribution"),
                    sliderInput("HSAAmountSld", label = "HSA Contribution", min = 0, max = 6850, value = 0, step=50, width = "500px")
                    
                  ),
                  hr(),
                  fluidRow(
                    fluidRow(column(8,h3("Other Details Summary")),
                        column(4, checkboxInput("hideDetailSummary", label = "Hide Detail Summary", value= TRUE))
                    ),
                    selectInput("otherDetailSummary",label = "Select Other Taxes Detail Summary", choices = c("NONE"),selected = "NONE"),
                    dataTableOutput("otherDetailTbl")
                    # checkboxInput("displayOtherDetailGrh", label = "Display Graph", value= FALSE),
                    # plotOutput("detailGraph")
                  ) # End Other Detail Section
         ) 
       )
    ),
    tabPanel("Information Summary",
      navlistPanel("Information you Enter:",
        tabPanel("Filing Status & Dependent Infor You Entered:",
                 dataTableOutput("FS_Summary")),
        tabPanel("Income Summary",
                 dataTableOutput("Income_Summary")),
        tabPanel("Deduction Summary",
                 dataTableOutput("Deduction_Summary"))
      )
    ),
    tabPanel("About",
         p("Hi, my name is Long Nguyen. Currently, I am a graduate MSSD student at ", a(href = "http://www.bu.edu/csmet/academic-programs/mssd/", "Boston University."),
            "This app is a part of my self-learning of R/Shiny and Data-Science."),
         p(strong("How this app comes about:"), "I deal with tax every day in my day job. Therefore, my first app would have something to do with the thing that everyone love to hate:Taxxxx!
           Though, my goals are to make this process less painful and fun while exercising my programming skills."),
         p(),
         h5("This is my wife and I taking a walk at Pier 39 in San Francisco"),
         tags$img(src = "img1.JPG",height = "1000", width = "700")
         

    )
  )
)

server <- function(input, output, session) {
  # local variable for entire server
  
  # get statusInformation, incomeInformation,deductions, and credits information entered by user.
  
  statusInformation <- callModule(filingInformation, "filingInformation", session = session)
  income <- callModule(income,"income", session = session)
  deductions <- callModule(deductions, "deductions", session = session)
  credits <- callModule(credits, "credits", session = session)
  
  #--------------------------------------------------------------------------------
  # Display user entered information to information summary tab
  output$FS_Summary <- renderDataTable(datatable({statusInformation()},
                                                 extensions = "Buttons", 
                                                 options = list(pageLength = 25, dom ="Bfrtip",buttons= c("excel", "print"),
                                                                fixedColumns = TRUE, autoWidth = FALSE, paging= TRUE), class= "display")
                                        )
  output$Income_Summary <- renderDataTable(datatable({income()}, 
                                                 extensions = "Buttons", 
                                                 options = list(pageLength = 25, dom ="Bfrtip",buttons= c("excel", "print"),
                                                                fixedColumns = TRUE, autoWidth = FALSE, paging= TRUE), class= "display")
                                          )
  output$Deduction_Summary <- renderDataTable(datatable({deductions()}, 
                                                  extensions = "Buttons", 
                                                  options = list(pageLength = 25, dom ="Bfrtip",buttons= c("excel", "print"),
                                                                 fixedColumns = TRUE, autoWidth = FALSE, paging= TRUE), class= "display")           
                                             )
  
  # -- Observe when user check on display graph---
  observe({
    if (input$hideTaxSummary){ # Hide Tax Summary  Section
      updateCheckboxInput(session,"showTaxSummaryTbl",value = FALSE )
      updateCheckboxInput(session, "displaySummaryGraph",value = FALSE)
      updateCheckboxInput(session, "viewCreditChb", value= FALSE)
      hideshow(c("showTaxSummaryTbl", "add2017","taxSummaryTbl","viewCreditChb",
                 "displaySummaryGraph"), TRUE)
    } else {
      hideshow(c("showTaxSummaryTbl", "add2017","taxSummaryTbl","viewCreditChb",
                 "displaySummaryGraph"), FALSE)
    } # End Hide Tax Summary Section----------------------------------------------
  })  
  observe({
    if (input$hideTaxPlanning){ # Hide Tax Planning
      hideshow(c("filingStatus","qualifyingChildU17", "qualifyingChildO17","qualifyingRelatives","YourIRAAmountSld",
                 "SpouseIRAAmountSld","HSAAmountSld"), TRUE)
    } 
    else {
      hideshow(c("filingStatus","qualifyingChildU17", "qualifyingChildO17","qualifyingRelatives","YourIRAAmountSld",
                 "SpouseIRAAmountSld","HSAAmountSld"), FALSE)
    }# End Hide Tax Planning. Uncheck this box will be handled separately within function below.
  })
  observe({
    if (input$hideDetailSummary){
      # updateCheckboxInput(session, "displayOtherDetailGrh",value = FALSE)
      hideshow(c("otherDetailSummary","otherDetailTbl"), TRUE)
    } else {
      hideshow(c("otherDetailSummary","otherDetailTbl"), FALSE)
    }# End hide Other Detail Summary Section.
    if (input$displaySummaryGraph) { #Show summary Graph
      show("summaryGraph")
    } else {
      hide("summaryGraph")
    } # End Summary Graph -----------------------
    
    if (input$showTaxSummaryTbl){
      show("taxSummaryTbl")
    } else {
      hide ("taxSummaryTbl")
    }
  })
  observe ({
    if (input$viewCreditChb){# Credit checkbox under tax summary
      show ("creditsSelect")
      show ("CreditTbl")
    } else {
      hide ("creditsSelect")
      hide ("CreditTbl")
    } # End credit checkbox -----------
  })
  observe({
    if (input$taxPlanning){# Update values in planning section
      # Update variables for taxPlanning section
      updateSelectInput(session,"filingStatus", selected = statusInformation()["Filing_Status",1])
      updateSliderInput(session, "qualifyingChildU17", value = as.numeric(statusInformation()[c("Qualifying_Child_Under_17"),1]))
      updateSliderInput(session, "qualifyingChildO17", value = as.numeric(statusInformation()[c("Qualifying_Child_Over_17"),1]))
      updateSliderInput(session, "qualifyingRelatives", value = as.numeric(statusInformation()["Qualifying_Relative",1]))
      updateSliderInput(session, "YourIRAAmountSld", value = deductions()["Your_IRA_Contribution",1])
      updateSliderInput(session, "SpouseIRAAmountSld", value = deductions()["Spouse_IRA_Cover",1])
      updateSliderInput(session, "HSAAmountSld", value = deductions()["HSA_Contribution",1])
      updateCheckboxInput(session,"hideTaxPlanning", value = FALSE )
    }
  })
    
  #-------------------------------------------------------------------
  output$taxSummaryTbl <- renderDataTable({
      #------------------------------------------------------------------+
      # Step 1: Calculate TotalIncome, TotalDeductions Above AGI, SD or Itemized Deductions, Exemption, taxes, and credits
      # local variables for this function ------------------------------------
      statusDF <- statusInformation()
      filingStatus <- toupper(statusDF["Filing_Status",1]) # commonly use variable
      incomeDF <- income() # getting dataframe from income.           # commonly use variable
      IRAContribution <- as.numeric(deductions()[c("Your_IRA_Contribution","Spouse_IRA_Contribution"),1])
      taxes2017 <- NULL                # Vector variable used to store taxes 2017
      taxPlanning <- NULL              # Vector variable used to store taxPlanning
      credits18 <- NULL
      deductionsDF <- deductions()
      
      # Start main function
      taxes2018 <- taxSummary(2018,statusDF, incomeDF, deductionsDF, credits())
      credits18 <- taxes2018[[5]]
      summaryDF <- data.frame(taxes2018[[1]], row.names = names(taxes2018[[1]]))
      colnames(summaryDF) <- "Tax_2018"
      currentColNames <- colnames(summaryDF)
      # Adding eligible credits to selectInput
      updateSelectInput(session,"creditsSelect", choices = names(credits18) )
      # Finish Step 1- Calculate Taxes_2018 --------------------------------------------------------------------------------
      if (input$add2017){
        # Step 1: Calculate taxes2017
        taxes2017 <- taxSummary(2017,statusDF, incomeDF, deductionsDF, credits())

        # Step 2: Add to summaryDF
        summaryDF <- cbind(summaryDF, taxes2017[[1]])
        colnames(summaryDF) <- append(currentColNames, "Tax_2017")
      } 
      else { # unchecked, remove tax 2017 column
        summaryDF$Tax_2017 <- NULL
      }
      currentColNames <- colnames(summaryDF)
      if (input$taxPlanning){
        # Step 1: Update max amount for both HSA Contribution sliders
        updateSliderInput(session,"HSAAmountSld", max = taxes2018[[3]]["Maximum_Contribution",1] )
        # Step 2: Updates values in both statusDF and deductionsDF
        statusDF["Filing_Status",1] <- input$filingStatus
        statusDF["Qualifying_Child_Under_17",1] <- input$qualifyingChildU17
        statusDF["Qualifying_Child_Over_17",1] <- input$qualifyingChildO17
        statusDF["Qualifying_Relative",1] <- input$qualifyingRelatives
        deductionsDF["Your_IRA_Contribution",1] <- input$YourIRAAmountSld
        deductionsDF["Spouse_IRA_Cover",1] <- input$SpouseIRAAmountSld
        deductionsDF["HSA_Contribution",1] <- input$HSAAmountSld
        # Step 3: calculate taxPlan
        taxPlanning <- taxSummary(2018, statusDF, incomeDF, deductionsDF, credits())

        # Step 4: add to summaryDF
        summaryDF <- cbind(summaryDF, taxPlanning[[1]])
        colnames(summaryDF) <- append(currentColNames, "Tax_Planning")
      } 
      else {
        summaryDF$Tax_Planning <- NULL
      } 
       
      output$CreditTbl <- renderDataTable({
        creditName <- input$creditsSelect
        return (datatable({credits18[[creditName]]},
                  extensions = "Buttons", 
                  options = list(pageLength = 25, dom ="Bfrtip",buttons= c("excel", "print"),
                                 fixedColumns = TRUE, autoWidth = FALSE, paging= TRUE), class= "display" 
                )
        )
      })
      # Finish Step 5 ------------------------------------------------------------------------
      # Step 6: Adding to Other Details Summary
      detailLabel <- "Above_AGI_Deduction_Summary"
      rowValues <- taxes2018[[2]][,1] !=0
      if (sum(rowValues)>0) detailLabel <- append(c("Income_Summary"), detailLabel)
      updateSelectInput(session, "otherDetailSummary", choices= detailLabel )

      output$otherDetailTbl <- renderDataTable({
        detailSummaryDF <- 0
        if (input$otherDetailSummary == "Income_Summary"){
          rowNames <- rownames(taxes2018[[2]])
          rowValues <- taxes2018[[2]][,1] !=0
          rowNames <- rowNames[rowValues]
          totalIncomeDF <- as.data.frame(taxes2018[[2]][rowValues,1], row.names = rowNames)
          colnames(totalIncomeDF) <- "Tax_2018"
          detailSummaryDF <- totalIncomeDF
        } 
        else {  # return Deductions To AGI
          detailSummaryDF <- taxes2018[[3]]
        }
        return (datatable({detailSummaryDF},
                          extensions = "Buttons", 
                          options = list(pageLength = 25, dom ="Bfrtip",buttons= c("excel", "print"),
                                         fixedColumns = TRUE, autoWidth = TRUE, paging= TRUE), class= "display"        
                ))
      })
      # Finish Step 6------------------------------------------------------------------------
      # Graph section -------------------------------------------------------------------------
      # Step 1: cut row with zero value
      Income_Credit <- row.names(summaryDF)
      valueRows <- apply(summaryDF, 1, function(row) all(row!=0))
      Income_Credit <- Income_Credit[valueRows]
      
      graphDF <- data.frame(Income_Credit,summaryDF[valueRows,], row.names = NULL)
      colnames(graphDF) <- append("Income_Credit", colnames(summaryDF))
      point <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)
      output$summaryGraph <- renderPlot({
        ggplot(data = DFConverter(graphDF), aes(x= Income_Credit, y =Amount, fill = TaxYear))+
          geom_bar (stat="identity", position = position_dodge())+
          geom_text(aes(label=Amount), vjust=-0.5, color="black", size=3.5, position = position_dodge(width = 0.9))+
          scale_y_continuous(labels =point)
        
      })
      
      # Final Step: return data to output$taxSummaryTbl
      return (
        datatable({summaryDF},extensions = "Buttons", 
                  options = list(pageLength = 25,dom ="Bfrtip",buttons= c("excel", "print"),
                                fixedColumns = FALSE, autoWidth = FALSE, paging= TRUE), class= "display")
      )
  } ) # End Tax Summary
}

shinyApp(ui, server)