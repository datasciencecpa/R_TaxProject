# This will be the main helper module that will interact directly with app.R
# This module will use the help of other modules:
# -deductionCalculation
# -creditCalculation
# to figure out total income, total deductions above AGI, AGI, standard/itemized deductions, credits, and final tax amount

# Author: Long Nguyen
# Date Created: 01/28/2019
source ("deductionCalculation.R")
source ("creditCalculation.R")
taxTbl <- read.xls(xls = "TaxRates.xls", sheet = 1)
LTCapTbl <- read.xls(xls = "TaxRates.xls", sheet = 2)
QDCGWorksheet <- function(taxYear, taxableIncome, incomeDF, filingStatus){
  # Determine if Qualified Dividends and CG Worksheet need to be used
  # When there are Qualified Div, or LT CapGain, this worksheet needs to be used
  # To figure out amounts that may be eligible for lower rate

  # Main function below

  div <- incomeDF[1]
  LTGain <- incomeDF[2]
  netGain <- sum(incomeDF[2:3])

  tax_15 <- 0
  tax_20 <- 0
  if ((div>0) | (LTGain>0 & netGain>0)){
    # Perform this calculation only when either conditions above are met
    rowValuesDF <- LTCapTbl[LTCapTbl$YEAR == taxYear & grepl(filingStatus, LTCapTbl$FILING_STATUS),]
    # print (rowValuesDF)
    line_3 <- ifelse(LTGain<netGain, LTGain, netGain)
    # print (paste("Line 3 amount: ", line_3))
    line_4 <- div + line_3
    # print (paste("Line 4 amount: ", line_4))
    line_7 <- ifelse ((taxableIncome - line_4)>0, taxableIncome - line_4,0)
    line_8 <- rowValuesDF[1,"UPPER_AMT"]
    line_15 <- rowValuesDF[2, "UPPER_AMT"]
    # print (paste("Line 8 amount: ", line_8))
    # print (paste("Line 15 amount: ", line_15))
    # If amount from line_7 above is lower than the 0% limit, some or all of the line_4 amount may be taxed at 0%
    if (line_7< line_8){ # Some amount of line 4 will be taxed at 0%
      if (taxableIncome>line_8) { # Some amount of line 4 will be taxed at 0%
        line_14 <- line_4 -(line_8 - line_7)
        # print (paste("Line 14 to be tax at 15%", line_14))
        tax_15 <- line_14*0.15
      }  
    } else if ((line_7 < line_15) & (line_7>=line_8)){ # some or all of line 4 will be taxed at 15%
      if (taxableIncome > line_15){
        line_22 <- line_4 -(line_15 - line_7)
        # print (paste("Line 22 to be tax at 20%%", line_22))
        tax_15 <- (line_15 - line_7) *0.15
        tax_20 <- line_22 * 0.2
      } else { # Entire line 4 will be taxed at 15%
        tax_15 <- line_4 *0.15
      }
    } else { # Entire amount of line 4 wil be taxed at 20%
      tax_20 <- line_4 *0.2
      # print (paste("Entire amount taxed at 20%", tax_20))
    }
    taxableIncome <- line_7
  }
  # print (paste("Net taxable income: ", taxableIncome))
  taxRow <- taxTbl[taxTbl$YEAR == taxYear & grepl(filingStatus, taxTbl$FILING_STATUS),]
  ind <- which (taxRow$LOWER_AMT< taxableIncome & taxRow$UPPER_AMT>  taxableIncome)
  taxRow <- taxRow[ind,]
  # print (taxRow)
  taxAmount <- (taxableIncome - taxRow$LOWER_AMT) * taxRow$TAX_RATE + taxRow$ADD_ON + tax_15 + tax_20
  # print (paste("Tax Amount base on rate:", (taxableIncome - taxRow$LOWER_AMT) * taxRow$TAX_RATE))
  # print (paste("Addon: ", taxRow$ADD_ON))
  return (taxAmount)
}
totalIncomeCalculation <- function (incomeDF){
   # This function will sum up total income user entered from the income section. incoe
   # Special handle for net capital gain/loss so that maximum of -$3000 are allowed. 
   Income_Type <- c("Total_W2_Wages", "Interest", "Ord_Dividends", "Qualified_Dividends","Taxable_Refunds","Alimony", "Net_Capital_Gain_Loss",
                 "IRA_Distribution", "Unemployment Income", "Total_Taxes_Withheld","Total_Medicare_Tax_Withheld")

   TotalIncome <- c(
     sum(incomeDF[c("Your_Wages", "Spouse_Wages", "Additional_Wages_1", "Additional_Wages_2"),"Income_Tax_2018"]),
     incomeDF["Interest", 1],
     incomeDF["Ordinary_Dividends", 1],
     incomeDF["Qualified_Dividends",1],
     incomeDF["Tax_Refunds", 1],
     incomeDF["Alimony",1],
     ifelse (sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),1])< -3000,-3000,sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),1])),
     incomeDF["TaxableIRA",1],
     incomeDF["Unemployment_Income", 1],
     sum(incomeDF[c("Your_W2_Tax","Spouse_W2_Tax","Additional_W2_Tax_1","Additional_W2_Tax_2","Interest_Tax","Dividend_Tax",
                    "Capital_Gain_Tax","IRA_Tax","Unemployment_Tax"),1]),
     sum(incomeDF[c("Your_Medicare_Tax","Spouse_Medicare_Tax","Add_Medicare_Tax1","Add_Medicare_Tax2"),1])
   )
   return (data.frame(TotalIncome, row.names = Income_Type))
}
totalDeductionToAGI <- function (deductionsDF, statusDF, AGIIncome, colName, taxYear) {
  # This function will calculate above AGI deductions.
  # Parameters: DeductionsDF: Dataframe that contains all deductions user entered.
  # statusDF: Vector that contains all user information: Ages, Filing status.
  # Income, use to check student loan interest and IRA deductions.
  # colName: use to assign colName for returnDF
  # Begin function --------------------------------------------------------
  returnDF <- data.frame(c(0,0,0,0,
                           0,0,0,
                           0,0,0,
                           0,0,0,
                           0,0), 
                         row.names = c("Educator_Expense", "HSA_Contribution", "HSA_Contribution_Per_W2","HSA_Plan_Type",
                                             "Catchup_Contribution","Maximum_Contribution","HSA_Deduction_Amt",
                                             "Your_IRA_Contribution","Your_IRA_Cover","Your_IRA_Deduction",
                                             "Spouse_IRA_Contribution","Spouse_IRA_Cover","Your_Spouse_IRA_Deduction",
                                       "Student_Loan_Interest", "Student_Loan_Deduction"))
  colnames(returnDF) <- colName
  filingStatus <- toupper(statusDF[1]) # vector of filing status
  ages <- as.numeric(statusDF[2:3]) # vector that contains ages
  # Starting with Educator_Expense
  if (filingStatus !="MFJ"){
    returnDF["Educator_Expense",1] <- min(deductionsDF["Educator_Expense",1], 250)
  } else {
    returnDF["Educator_Expense",1] <- deductionsDF["Educator_Expense",1]
  }
  
    # Finish checking educator expense ------------------------------------------
  # Calculate HSA Deduction if applicable.------------------------------------------
  returnDF[c("HSA_Contribution","HSA_Contribution_Per_W2","HSA_Plan_Type"),1] <- deductionsDF[c("HSA_Contribution","HSA_Contribution_Per_W2","HSA_Plan_Type"),1]  

  HSA_Deduction <- HSADeduction(ages, as.numeric(returnDF[c("HSA_Contribution","HSA_Contribution_Per_W2"),1]),
                            hsaPlan = returnDF["HSA_Plan_Type",1], taxYear)

  returnDF[c("HSA_Deduction_Amt","Catchup_Contribution","Maximum_Contribution"),1] <- HSA_Deduction

  # Finish calculating HSA Deduction ------------------------------------------------------------------------------------
  # Start calculating IRA Deduction
  returnDF[c("Your_IRA_Contribution","Spouse_IRA_Contribution",
             "Your_IRA_Cover","Spouse_IRA_Cover"),1] <-  deductionsDF[c("Your_IRA_Contribution","Spouse_IRA_Contribution",
                                                                        "Your_IRA_Cover","Spouse_IRA_Cover"),1]

  # Checking if user has enough earned income (Total Wages + Alimony) to contribute to IRA
  earnedIncome <- sum(as.numeric(AGIIncome[c(1,6)])) # Include total W2 Wages and Alimony
  AGI <- sum(as.numeric(AGIIncome[-c(4,10,11)]))
  MAGI <- AGI - sum(as.numeric(returnDF[c("Educator_Expense", "HSA_Deduction_Amt"),1]))
  IRA_Deduction <- IRADeduction(taxYear, IRAcover = as.character(deductionsDF[c("Your_IRA_Cover", "Spouse_IRA_Cover"),1]),
                                          filingStatus = filingStatus,
                                          ages = ages,
                                          MAGI = MAGI, 
                                          earnedIncome = earnedIncome, 
                                          IRAAmount = as.numeric(deductionsDF[c("Your_IRA_Contribution", "Spouse_IRA_Contribution"), 1]))

  returnDF[c("Your_IRA_Deduction","Your_Spouse_IRA_Deduction"),1] <- IRA_Deduction
    
  # End IRA deduction calculation ------------------------------------------------------------------------------------
  # STARTING CALCULATION OF STUDENT LOAN INTEREST DEDUCTION
  returnDF["Student_Loan_Interest",1] <- as.numeric(deductionsDF["Student_Loan_Interest",])
  AGI_Deduction <- sum(as.numeric(returnDF[c("Educator_Expense", "HSA_Deduction_Amt",
                                              "Your_IRA_Deduction", "Your_Spouse_IRA_Deduction"), 1]))
  
  MAGI <- AGI - AGI_Deduction  # Calculate new MAGI for student loan deduction

  SLDeduction <- studentLoan(interest = returnDF["Student_Loan_Interest",1], MAGI, filingStatus, 2018)
  print (paste("Student loan deduction: ", SLDeduction))
  returnDF["Student_Loan_Deduction",] <- SLDeduction
   # End Student loan deduction calculation ----------------------------------------------------------------------
  returnDF["Adjusted_Gross_Income", ] <- AGI - sum(as.numeric(returnDF[c("Educator_Expense", "HSA_Deduction_Amt","Your_IRA_Deduction", 
                                                        "Your_Spouse_IRA_Deduction","Student_Loan_Deduction"), 1]))
  returnDF["Adjusted_Gross_Income",] <- max(returnDF["Adjusted_Gross_Income",], 0)                                      
  print("Final Return DF Value")
  print (returnDF)
  return (returnDF) 
} # End totalDeductionToAGI
belowAGIDeduction <- function(deductionDF, statusDF, AGI){
  # This function will determine whether itemized deduction or SD will apply
  statusDF["Filing_Status", ] <- toupper(as.character(statusDF["Filing_Status", ]))
  deductionDF$Deduction_2018 <- as.numeric(deductionDF$Deduction_2018)
  deductionDF$Deduction_2017 <- as.numeric(deductionDF$Deduction_2017)
  deductions <- SDExemptionDeduction (deductionDF, statusDF, AGI)
  return (deductions)
}
DFConverter <- function (df){
  # This function is used to convert given dataframe into different format
  df_col_names <- colnames(df)
  df_1 <- df[c(df_col_names[1], df_col_names[3])]  # get dataframe that contain the first and last column
  colnames(df_1)[2] <- "Amount"
  df_1$TaxYear <- "2017"  #adding new column and assigned value 2017
  df[,3] <- NULL # Delete column with amount from 2017
  colnames(df)[2] <- "Amount"
  df$TaxYear <- "2018"
  # print (rbind(df, df_1))
  return (rbind(df, df_1))
}
taxCalculation <- function (taxableIncome, incomeDF, filingStatus){
  # This function will calculate tax based on taxable income
  # Parameter: taxableIncome is a vector, not a dataframe.
  if (taxableIncome[1]>0){
    tax_2018 <- QDCGWorksheet(2018, taxableIncome = taxableIncome[1], 
                              incomeDF[c("Qualified_Dividends", "Long_Term_Gains", "Short_Term_Gains"),"Income_Tax_2018"],
                              filingStatus[1])
  } else {
    tax_2018 <- 0
  }
  if (taxableIncome[2]>0){
    tax_2017 <- QDCGWorksheet(2017, taxableIncome = taxableIncome[2],
                              incomeDF[c("Qualified_Dividends", "Long_Term_Gains", "Short_Term_Gains"),"Income_Tax_2017"],
                              filingStatus[2])
  } else{
    tax_2017 <- 0
  }
  return (c(tax_2018, tax_2017))
  
}
creditCalculation <- function (summaryDF, incomeDF, filingStatus, creditDF){
  # This function will calculate available credits:
  # Child and Dependent Care Credit
  # Education Credit
  # Child Tax Credit/ Additional CTC
  # Saver Credit
  # It will return a list of dataframe with all available credits
  
  # Calcualte Child and DC Expenses Credit
  print (creditDF)
  credit_18 <-creditDF$Credit_18 # Vector that contains information user enter on Credit tab
  credit_17 <- creditDF$Credit_17 # Vector that contains information user enter on Credit tab
  names(credit_18) <- rownames(creditDF)
  names(credit_17) <- rownames(creditDF)
  otherCredits <- data.frame(c(0,0),c(0,0), row.names = c("CDC", "Education")) # Use to store other Nonrefundable credits for quick access
  returnList <- list() # Use to store list of dataframe that will return to App.R
  #----------Calculate CDC Credit ------------------------------------------------------
  returnList[["CDC"]] <- 0 # initialize list that will contain CDC dataframe.
  if (credit_18["Qualifying_Person"]>0 & credit_18["Expense"]>0){ # Calculate CDC for tax year 2018
    CDC_18 <- dependentCareCrd("2018",summaryDF$summary_2018, filingStatus[1],incomeDF$Income_Tax_2018, credit_18)
    if (CDC_18["Child_Dependent_Care_Credit",]>0){
      returnList[["CDC"]] <- CDC_18
      otherCredits["CDC",1] <- CDC_18["Child_Dependent_Care_Credit",]
    }
  }
  if (credit_17["Qualifying_Person"]>0 & credit_17["Expense"]>0){ # Calculate CDC for tax year 2017
    CDC_17 <- dependentCareCrd("2017",summaryDF$summary_2017, filingStatus[2],incomeDF$Income_Tax_2017, credit_17 )
    if (CDC_17["Child_Dependent_Care_Credit",]>0){
      if (!is.data.frame(returnList[["CDC"]])){
        returnList[["CDC"]] <- CDC_17
      } else {
        returnList[["CDC"]] <- cbind(returnList[["CDC"]], CDC_17)
      }
      otherCredits["CDC",2] <- CDC_17["Child_Dependent_Care_Credit",]
    }
  }
  print(otherCredits)
  # print ("Testing CDC_DF")
  # print (returnList[["CDC"]])
  
  #---------- Testing if Educational Credit need to be calculated-----------------------
  returnList[["Education"]] <- 0
  if (credit_18["Expense_1"]>0 | credit_18["Expense_2"]>0) {# Calculate only when expenses are greater than zero
    print ("Calculate Educational credit for 2018")
    CDC_18 
    EDC_18 <- educationalCrd("2018",summaryDF$summary_2018, filingStatus[1], credit_18, otherCredits["CDC",1])
    if (EDC_18["Line_19",]>0 | EDC_18["Refundable_AOC",]>0){
      otherCredits["Education",1] <- EDC_18["Line_19",]
      returnList[["Education"]] <- EDC_18
    }
  }
  if (credit_17["Expense_1"]>0 | credit_17["Expense_2"]>0) {# Calculate only when expenses are greater than zero
    print ("Calculate Educational credit for 2017")
    EDC_17 <- educationalCrd("2017",summaryDF$summary_2017, filingStatus[2], credit_17, otherCredits["CDC",2])
    if (EDC_17["Line_19",]>0 | EDC_17["Refundable_AOC",]>0){
      otherCredits["Education",2] <- EDC_17["Line_19",]
      if (!is.data.frame(returnList[["Education"]])) returnList[["Education"]] <- EDC_17
      else returnList[["Education"]] <- cbind(returnList[["Education"]], EDC_17)
    }
  }
  print(otherCredits)
  # print ("Testing Education")
  # print (returnList[["Education"]])
  
  #-----------netstep - calculate child tax credit
  
  return (returnList)
}