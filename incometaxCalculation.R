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
  # filingStatus <- toupper(filingStatus)
  # print (paste("IncomeDF:"))
  # print (incomeDF)
  # print (paste("Original taxable income: ", taxableIncome))
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
   # This function will sum up total income user entered from the income section.
   # This function will ignore income with value = 0, assuming that user did not have that type of income in both year.
   # Special handle for net capital gain/loss so that maximum of -$3000 are allowed. 
   Income_Type <- c("Total_W2_Wages", "Interest", "Dividends", "Taxable_Refunds","Alimony", "Net_Capital_Gain_Loss",
                 "IRA_Distribution", "Unemployment Income")

   AGI_2018 <- c(
     sum(incomeDF[c("Your_Wages", "Spouse_Wages", "Additional_Wages_1", "Additional_Wages_2"),"Income_Tax_2018"]),
     incomeDF["Interest", 1],
     incomeDF["Ordinary_Dividends", 1],
     incomeDF["Tax_Refunds", 1],
     incomeDF["Alimony",1],
     ifelse (sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),1])< -3000,-3000,sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),1])),
     incomeDF["TaxableIRA",1],
     incomeDF["Unemployment_Income", 1]
   )

   AGI_2017 <- c(
     sum(incomeDF[c("Your_Wages", "Spouse_Wages", "Additional_Wages_1", "Additional_Wages_2"), "Income_Tax_2017"]),
     incomeDF["Interest", 2],
     incomeDF["Ordinary_Dividends", 2],
     incomeDF["Tax_Refunds", 2],
     incomeDF["Alimony",2],
     ifelse (sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),2])< -3000,-3000,sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),2])),
     incomeDF["TaxableIRA",2],
     incomeDF["Unemployment_Income", 2]
   )
   #AGI_2017 <- c (AGI_2017, sum(AGI_2017))
   return (data.frame(AGI_2018, AGI_2017, row.names = Income_Type))
}
totalDeductionToAGI <- function (deductionsDF, statusDF, AGIIncome) {
  valueRow <- deductionsDF$Deduction_2018 !=0 | deductionsDF$Deduction_2017 !=0
  
  Deduction_Type <- c ("Educator_Expense", "HSA_Contribution","Your_IRA_Contribution", "IRA_Excess_Amount", "Student_Loan_Deduction")
  rowNames <- rownames(deductionsDF[valueRow,]) # Get name of rows in DeductionDF that have values.
  filingStatus <- as.character(statusDF["Filing_Status",])
  # Iterate through rowNames to see which above AGI deductions need to verify
  # Starting with Educator_Expense
  returnDF <- data.frame (Deduction_2018 = 0, 
                          Deduction_2017 = 0, row.names = "No Deduction") # This is the dataframe that use to hold return values for this function
  returnDF["Educator_Expense",] <- c(0,0)
  if (any(rowNames == Deduction_Type[1])){ # Check if Educator_Expense still in the deductonDF after elimination
      expense_2018 <- ifelse (filingStatus[1] != "MFJ",
                        ifelse (deductionsDF["Educator_Expense", "Deduction_2018"]<=250,deductionsDF["Educator_Expense", "Deduction_2018"], 250 ),
                          deductionsDF["Educator_Expense", "Deduction_2018"])
      expense_2017 <- ifelse(filingStatus[2] != "MFJ",
                        ifelse (deductionsDF["Educator_Expense", "Deduction_2017"]<=250,deductionsDF["Educator_Expense", "Deduction_2017"], 250 ),
                          deductionsDF["Educator_Expense", "Deduction_2017"])
      returnDF["Educator_Expense",] <- c(expense_2018, expense_2017)
      # print (c(expense_2018, expense_2017))
  } # Finish checking educator expense
  returnDF["HSA_Deduction_Amt",] <- c(0,0)
  if (any(rowNames == Deduction_Type[2])){ # checking HSA contribution, if no HSA contributon, skip this step
    # check if user enter amount or HSA Contribution per W2
    HSA_Per_W2 <- c(0,0)
    returnDF["HSA_Contribution",] <- deductionsDF["HSA_Contribution",]
    if (any(rowNames == "HSA_Contribution_Per_W2")) {
      HSA_Per_W2 <- as.numeric(deductionsDF["HSA_Contribution_Per_W2",])
    }
    returnDF["HSA_Contribution_Per_W2",] <- HSA_Per_W2
    returnDF["HSA_Plan_Type",] <- deductionsDF["HSA_Plan_Type",]
    deduction_2018 <- HSADeduction(ages = as.numeric(statusDF[c("Your_Age", "Spouse_Age"), "Status_2018"]), 
                                   contributionAmt = c(as.numeric(deductionsDF["HSA_Contribution", "Deduction_2018"]), HSA_Per_W2[1]),
                                   hsaPlan = deductionsDF["HSA_Plan_Type", "Deduction_2018"], taxYear = 2018)
    deduction_2017 <- HSADeduction(ages = as.numeric(statusDF[c("Your_Age", "Spouse_Age"), "Status_2017"]),
                                   contributionAmt = c(as.numeric(deductionsDF["HSA_Contribution", "Deduction_2017"]), HSA_Per_W2[2]),
                                   hsaPlan = deductionsDF["HSA_Plan_Type", "Deduction_2017"], taxYear = 2017)
    returnDF["Catchup_Contribution", ] <- c(deduction_2018[2], deduction_2017[2])
    returnDF["Maximum_Contribution", ] <- c(deduction_2018[3], deduction_2017[3])
    returnDF["HSA_Deduction_Amt",] <- c(deduction_2018[1], deduction_2017[1])
    if (any(returnDF["HSA_Contribution", ] > returnDF["HSA_Deduction_Amt",])) {
      returnDF["Excess_Contribution",] <- as.numeric(returnDF["HSA_Contribution", ]) - as.numeric(returnDF["HSA_Deduction_Amt",])
    }
    if (returnDF["HSA_Deduction_Amt", 1]<0) returnDF["HSA_Deduction_Amt", 1] <- 0
    if (returnDF["HSA_Deduction_Amt",2]<0) returnDF["HSA_Deduction_Amt",2] <- 0
  } # Finish checking HSA contribution
  returnDF["Your_IRA_Deduction",] <- c(0,0)
  returnDF["Your_Spouse_IRA_Deduction", ] <- c(0,0)
  if (any(grepl("IRA_Contribution", rowNames))){ # Checking if user entered any IRA contribution
    # Checking if user has enough earned income (Total Wages + Alimony) to contribute to IRA
    earnedIncome_2018 <- 0
    earnedIncome_2017 <- 0
    MAGI_2018 <- sum(as.numeric(AGIIncome$AGI_2018)) - sum(as.numeric(returnDF[c("Educator_Expense", "HSA_Deduction_Amt"), "Deduction_2018"]))
    MAGI_2017 <- sum(as.numeric(AGIIncome$AGI_2017)) - sum(as.numeric(returnDF[c("Educator_Expense", "HSA_Deduction_Amt"), "Deduction_2017"]))

    returnDF["IRA_Filing_Status",] <- filingStatus
    earnedIncome_2018 <- sum(as.numeric(AGIIncome[c(1,5),"AGI_2018"]))
    earnedIncome_2017 <- sum(as.numeric(AGIIncome[c(1,5), "AGI_2017"]))
    returnDF["Your_IRA_Contribution", ] <- deductionsDF["Your_IRA_Contribution",]
    returnDF["Your_IRA_Cover",] <- deductionsDF["Your_IRA_Cover",]
    if (any(filingStatus == "MFJ")) {
      returnDF["Spouse_IRA_Contribution",] <- deductionsDF["Spouse_IRA_Contribution",]
      returnDF["Spouse_IRA_Cover",] <- deductionsDF["Spouse_IRA_Cover", ]
    }
    
    IRA_Deduction_2018 <- IRADeduction(2018, IRAcover = as.character(deductionsDF[c("Your_IRA_Cover", "Spouse_IRA_Cover"),"Deduction_2018"]),
                                            filingStatus = as.character(filingStatus[1]),
                                            ages = as.numeric(statusDF[c("Your_Age", "Spouse_Age"), "Status_2018"]),
                                            MAGI = MAGI_2018, earnedIncome = earnedIncome_2018, 
                                            IRAAmount = as.numeric(deductionsDF[c("Your_IRA_Contribution", "Spouse_IRA_Contribution"), "Deduction_2018"]))
    print (paste("IRA Deduction 2018:", IRA_Deduction_2018))
    IRA_Deduction_2017 <- IRADeduction(2017, IRAcover = as.character(deductionsDF[c("Your_IRA_Cover", "Spouse_IRA_Cover"),"Deduction_2017"]),
                                       filingStatus = as.character(filingStatus[2]),
                                       ages = as.numeric(statusDF[c("Your_Age", "Spouse_Age"), "Status_2017"]),
                                       MAGI = MAGI_2018, earnedIncome = earnedIncome_2017, 
                                       IRAAmount = as.numeric(deductionsDF[c("Your_IRA_Contribution", "Spouse_IRA_Contribution"), "Deduction_2017"]))
    print (paste("IRA Deduction 2017:", IRA_Deduction_2017))
    returnDF["Your_IRA_Deduction",] <- c(IRA_Deduction_2018[1], IRA_Deduction_2017[1])
    returnDF["Your_Spouse_IRA_Deduction", ] <- c(IRA_Deduction_2018[2], IRA_Deduction_2017[2])
    
  } # End IRA deduction calculation
  returnDF["Student_Loan_Interest",] <- c(0,0)
  returnDF["Student_Loan_Deduction",] <- c(0,0)
  if (any(grepl("Student_Loan", rowNames))){

    SLInterest <- as.numeric(deductionsDF["Student_Loan_Interest",])
    returnDF["Student_Loan_Interest",] <- SLInterest
    deduction_2018 <- sum(as.numeric(returnDF[c("Educator_Expense", "HSA_Deduction_Amt",
                                                "Your_IRA_Deduction", "Your_Spouse_IRA_Deduction"), "Deduction_2018"]))
    deduction_2017 <- sum(as.numeric(returnDF[c("Educator_Expense", "HSA_Deduction_Amt",
                                                "Your_IRA_Deduction", "Your_Spouse_IRA_Deduction"), "Deduction_2017"]))
    MAGI_2018 <- sum(as.numeric(AGIIncome$AGI_2018)) - deduction_2018 
    MAGI_2017 <- sum(as.numeric(AGIIncome$AGI_2017)) - deduction_2017
    SLDeduction_2018 <- studentLoan(interest = SLInterest[1], MAGI_2018, filingStatus[1], 2018)
    print (paste("Student loan deduction: ", SLDeduction_2018))
    SLDeduction_2017 <- studentLoan(interest = SLInterest[2], MAGI_2017, filingStatus[1], 2017)
    print (paste("Student loan deduction: ", SLDeduction_2017))
    returnDF["Student_Loan_Deduction",] <- c(SLDeduction_2018, SLDeduction_2017)
  } # End Student loan deduction calculation
  returnDF["Adjusted_Gross_Income", ] <- c(sum(AGIIncome$AGI_2018) -
                                             sum(as.numeric(returnDF[c("Educator_Expense", "HSA_Deduction_Amt","Your_IRA_Deduction", 
                                                        "Your_Spouse_IRA_Deduction","Student_Loan_Deduction"), "Deduction_2018"])),
                                           sum(AGIIncome$AGI_2017) -
                                             sum(as.numeric(returnDF[c("Educator_Expense", "HSA_Deduction_Amt","Your_IRA_Deduction", 
                                                         "Your_Spouse_IRA_Deduction","Student_Loan_Deduction"), "Deduction_2017"])))
  if (nrow(returnDF)>1) returnDF <- returnDF[-1,]
  return (returnDF) 
}
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
  # It will return a dataframe with all available credits
  
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
    CDC_18 <- dependentCareCrd("2018",summaryDF$summary_2018, filingStatus[1],incomeDF$Income_Tax_2018, credit_18 )
    returnList[["CDC"]] <- CDC_18
    otherCredits["CDC",1] <- CDC_18["Child_Dependent_Care_Credit",]
    
  }
  if (credit_17["Qualifying_Person"]>0 & credit_17["Expense"]>0){ # Calculate CDC for tax year 2017
    CDC_17 <- dependentCareCrd("2017",summaryDF$summary_2017, filingStatus[2],incomeDF$Income_Tax_2017, credit_17 )
    otherCredits["CDC",2] <- CDC_17["Child_Dependent_Care_Credit",]
    print (otherCredits)
    if (!is.data.frame(returnList[["CDC"]])){
      returnList[["CDC"]] <- CDC_17
    } else {
      returnList[["CDC"]] <- cbind(returnList[["CDC"]], CDC_17)
    }
  }
  print(otherCredits)
  print ("Testing CDC_DF")
  print (returnList[["CDC"]])
  
  #---------- Testing if Educational Credit need to be calculated-----------------------
  returnList[["Education"]] <- 0
  if (credit_18["Expense_1"]>0 | credit_18["Expense_2"]>0) {# Calculate only when expenses are greater than zero
    print ("Calculate Educational credit for 2018")
    CDC_18 
    EDC_18 <- educationalCrd("2018",summaryDF$summary_2018, filingStatus[1], credit_18, otherCredits["CDC",1])
    otherCredits["Education",1] <- EDC_18["Line_19",]
    returnList[["Education"]] <- EDC_18
  }
  if (credit_17["Expense_1"]>0 | credit_17["Expense_2"]>0) {# Calculate only when expenses are greater than zero
    print ("Calculate Educational credit for 2017")
    EDC_17 <- educationalCrd("2017",summaryDF$summary_2017, filingStatus[2], credit_17, otherCredits["CDC",2])
    otherCredits["Education",2] <- EDC_17["Line_19",]
    if (!is.data.frame(returnList[["Education"]])) returnList[["Education"]] <- EDC_17
    else returnList[["Education"]] <- cbind(returnList[["Education"]], EDC_17)
  }
  print(otherCredits)
  print ("Testing Education")
  print (returnList[["Education"]])
  return (returnList)
}