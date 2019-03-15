# Author: Long Nguyen
# Date Created: 01/28/2019
# This will be the main helper module that will interact directly with app.R
# This module will use the helps of other modules:
# -deductionCalculation
# -creditCalculation
# to figure out total income, total deductions above AGI, AGI, standard/itemized deductions, credits, and final tax amount


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
  taxAmount <- round(taxAmount,digits = 2)
  # print (paste("Tax Amount base on rate:", (taxableIncome - taxRow$LOWER_AMT) * taxRow$TAX_RATE))
  # print (paste("Addon: ", taxRow$ADD_ON))
  return (taxAmount)
}
totalIncomeCalculation <- function (taxYear,incomeDF){
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
     max (sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),1]), -3000),
     incomeDF["TaxableIRA",1],
     incomeDF["Unemployment_Income", 1],
     sum(incomeDF[c("Your_W2_Tax","Spouse_W2_Tax","Additional_W2_Tax_1","Additional_W2_Tax_2","Interest_Tax","Dividend_Tax",
                    "Capital_Gain_Tax","IRA_Tax","Unemployment_Tax"),1]),
     sum(incomeDF[c("Your_Medicare_Tax","Spouse_Medicare_Tax","Add_Medicare_Tax1","Add_Medicare_Tax2"),1])
   )
   totalIncomeDF <- data.frame(TotalIncome, row.names = Income_Type)
   colnames(totalIncomeDF) <- taxYear
   return (totalIncomeDF)
}
totalDeductionToAGI <- function (taxYear,status,deductionsDF,AGIIncome) {
  # This function will calculate above AGI deductions.
  # Parameters: DeductionsDF: Dataframe that contains all deductions user entered.
  # status: Vector that contains all user information: Ages, Filing status.
  # AGIIncome: Vector uses to check student loan interest and IRA deductions.
  # deductionsDF <- contains all deductions usered entered.
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
  colnames(returnDF) <- taxYear
  filingStatus <- toupper(status[1]) # vector of filing status
  ages <- as.numeric(status[2:3]) # vector that contains ages
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
  earnedIncome <- sum(as.numeric(AGIIncome[c(1,6)]))    # Include total W2 Wages and Alimony
  AGI <- sum(as.numeric(AGIIncome[-c(4,10,11)]))        # Exclude qualified div, and taxes rows
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

  SLDeduction <- studentLoan(interest = returnDF["Student_Loan_Interest",1], MAGI, filingStatus, taxYear)
  # print (paste("Student loan deduction: ", SLDeduction))
  returnDF["Student_Loan_Deduction",] <- SLDeduction
   # End Student loan deduction calculation ----------------------------------------------------------------------
  returnDF["Adjusted_Gross_Income", ] <- AGI - sum(as.numeric(returnDF[c("Educator_Expense", "HSA_Deduction_Amt","Your_IRA_Deduction", 
                                                        "Your_Spouse_IRA_Deduction","Student_Loan_Deduction"), 1]))
  returnDF["Adjusted_Gross_Income",] <- max(returnDF["Adjusted_Gross_Income",], 0)                                      
  # print("Final Return DF Value")
  # print (returnDF)
  return (returnDF) 
} # End totalDeductionToAGI
belowAGIDeduction <- function(deductionDF, statusDF, AGI, taxYear){
  # This function will determine whether itemized deduction or SD will apply
  # Parameters: deductionDF: vector that contains itemmized expenses
  # statusDF: dataframe that contains status information such as filing status, ages
  statusDF["Filing_Status", ] <- toupper(as.character(statusDF["Filing_Status", ]))
  deductionDF <- as.numeric(deductionDF)

  deductions <- SDExemptionDeduction (deductionDF, statusDF, AGI, taxYear)

  return (deductions)
}
DFConverter <- function (df){
  # This function is used to convert given dataframe into different format
  print(head(df,5))
  df_col_names <- colnames(df)
  df_1 <- df[c(df_col_names[1], df_col_names[3])]  # get dataframe that contain the first and last column
  colnames(df_1)[2] <- "Amount"
  df_1$TaxYear <- "2017"  #adding new column and assigned value 2017
  df[,3] <- NULL # Delete column with amount from 2017
  colnames(df)[2] <- "Amount"
  df$TaxYear <- "2018"
  print (head(rbind(df, df_1),5))
  return (rbind(df, df_1))
}
taxCalculation <- function (taxableIncome, incomeDF, filingStatus, taxYear){
  # This function will calculate tax based on taxable income
  # Parameter: taxableIncome is a vector, not a dataframe.
  taxes <- 0
  if (taxableIncome>0){
    taxes <- QDCGWorksheet(taxYear, taxableIncome = taxableIncome, 
                              incomeDF[c("Qualified_Dividends", "Long_Term_Gains", "Short_Term_Gains"),1],
                              filingStatus)
  } else {
    taxes <- 0
  }
  return (c(taxes))
}
creditCalculation <- function (AGI, taxes, incomeDF,statusDF, creditDF,IRAContribution, taxYear){
  # This function will calculate available credits:
  # Child and Dependent Care Credit
  # Education Credit
  # Child Tax Credit/ Additional CTC
  # Saver Credit
  # It will return a list of dataframe with all available credits
  # IRAContribution is a vector that contains contributions to IRA on Deductions Tab that user entered.Used for saver's credit
  
  # Calcualte Child and DC Expenses Credit
  # print (creditDF)
  filingStatus <- toupper(statusDF["Filing_Status",1])
  earnedIncome <- sum(as.numeric(incomeDF[c(1,3,5,7),1])) # Sum of all wages
  otherCredits <- data.frame (c(0,0,0,0,0,0,0), row.names = c("CDC", "Education", "Saver", "CTC", "AOC","ACTC", "EIC")) # Use to store credits
  colnames(otherCredits) <- taxYear
  returnList <- list() # Use to store list of dataframe that will return to App.R
  #----------Calculate CDC Credit ------------------------------------------------------

  if (creditDF["Qualifying_Person",]>0 & creditDF["Expense",]>0){ # Calculate CDC for current tax year
    CDC <- dependentCareCrd(taxYear,AGI, taxes, filingStatus,incomeDF$Income_Tax_2018, creditDF)
    if (CDC["Child_Dependent_Care_Credit",]>0){
      returnList[["Child and Dependent Care Credit"]] <- CDC # Store dataframe
      otherCredits["CDC",1] <- CDC["Child_Dependent_Care_Credit",]
    }
  }
  #---------- Testing if Educational Credit need to be calculated-----------------------

  if (creditDF["Expense_1",1]>0 | creditDF["Expense_2",1]>0) {# Calculate only when expenses are greater than zero
    # print ("Calculate Educational credit for 2018")

    EDC <- educationalCrd(taxYear,AGI, taxes, filingStatus, creditDF, otherCredits["CDC",1])
    if (EDC["Line_9_AOC_Nonrefundable_Amount ",]>0 | EDC["Refundable_AOC",]>0){
      otherCredits["Education",1] <- EDC["Line_19:Nonrefundable Education Credits",]
      otherCredits["AOC",1] <- EDC["Refundable_AOC",]
      returnList[["Education"]] <- EDC
    }
  }
  #-----------nextstep - calculate saver's credit--------------------------------------------
  retirementContribution <- creditDF[c("Your_Retirement_Contribution","Spouse_Retirement_Contribution"),1]
  if (any(IRAContribution>0) | any(retirementContribution>0)){
    # Calculate credit, starting with identify amount of earnedIncome
    # print ("Calculate saver's credit")
    if (filingStatus!="MFJ") { # Eliminate any amounts entered on the spouse contribution boxes and IRA boxes
      # print ("Not MFJ")
      earnedIncome <- earnedIncome - sum(as.numeric(incomeDF[c(3,7),1])) # Eliminate spouse wages out from earned income
      IRAContribution[2] <- 0
      creditDF["Spouse_Retirement_Contribution",1] <- 0
    }
    saverDF <- saverCrd(taxYear, filingStatus, AGI, taxes, earnedIncome, IRAContribution, 
                      retirementContribution, sum(otherCredits[c("CDC", "Education"),1]))
    if (saverDF["Credit_Amount",1]>0){
      otherCredits["Saver",1] <- saverDF["Saver_Credit",1]
      returnList[["Saver's Credit"]] <- saverDF
    }
  }
  # _______________nextstep -- Calculate Child Tax and Other Dependent Credits--------------------------------
  sumOtherCredits <- sum(otherCredits[c("CDC","Education","Saver"),1])
  ACTC <- FALSE
  isCTCEligible <- FALSE
  if (taxYear ==2018 & sum(as.numeric(statusDF[c("Qualifying_Child_Under_17","Qualifying_Relative"),1]))>0) {
    CTC <- childTaxCrd(taxYear, AGI, taxes, statusDF,sumOtherCredits )
    isCTCEligible <- TRUE
  } else if (sum(as.numeric(statusDF[c("Qualifying_Child_Under_17"),1]))>0){
    CTC <- childTaxCrd(taxYear, AGI, taxes, statusDF,sumOtherCredits )
    isCTCEligible <- TRUE
  }
  if (isCTCEligible){
    if (CTC["Line_10",]>0){
      otherCredits["CTC",1] <- CTC["Line_16:Child_Tax_Credit",]
      if (CTC["Possible_Additional_CTC",1] == 1) ACTC <- TRUE
      returnList[["Child Tax Credit"]] <- CTC
      
    }
  }
  
  #_______________nextstep -- Calculate Additional Child Tax Credits--------------------------------
  if (ACTC) { # Calculate ACTC for the user
    ACTC <- additionalChildTaxCrd(taxYear,returnList[["Child Tax Credit"]], statusDF, earnedIncome)
    if (ACTC["ACTC_Amount",1]>0){
      returnList[["Additional Child Tax Credit"]] <- ACTC
      otherCredits["ACTC",1] <- ACTC["ACTC_Amount",1]
    }
  }
  #_______________nextstep -- Calculate EIC Credits--------------------------------
  investmentIncome <- sum(as.numeric(incomeDF[c("Interest","Ordinary_Dividends"),1]))
  investmentIncome <- investmentIncome + max(sum(as.numeric(incomeDF[c("Short_Term_Gains","Long_Term_Gains"),1])),0)
  # Calculate EIC only when filingStatus != MFJ, and investmentIncome is lower than amount allowed
  maxInvestmentIncome <- 3500  # Amount applied for tax year 2018
  if (taxYear == 2017){
    maxInvestmentIncome <- 3450
  }
  if (filingStatus!= "MFS" & investmentIncome<=maxInvestmentIncome){ #Calculate EIC Credit
    EICdf <- EIC(taxYear, earnedIncome,AGI,statusDF)
    if (EICdf["EIC_Credit_Limit",1]>0){
      returnList[["Earned Income Credit"]] <- EICdf
      otherCredits["EIC",1] <- EICdf["EIC_Amount",1]
    }
  } else {
    print ("Not Qualified for EIC")
  }
  returnList[["Credits Summary"]] <- otherCredits
  
  return (returnList)
}
additionalTaxes <- function (statusDF, incomeDF, AGI,taxYear){
  # This function will calculate the additional taxes from 3 sources:
  # 1. Additional Medicare Taxes
  # 2. Net Investment Income Taxes
  # 3. 10% additional taxes on IRA distribution
  addMedicareTaxes <- function (statusDF, incomeDF,taxYear){
    # Helper function. 
    # This function will calculate medicare taxes on from W-2 wages only. 
    # https://www.irs.gov/pub/irs-pdf/i8959.pdf
    
    thresholdAmount <- 0
    if (statusDF["Filing_Status",1]=="MFJ"){
      thresholdAmount <- 250000
    } else if (statusDF["Filing_Status",1]=="MFS"){
      thresholdAmount <- 125000
    } else thresholdAmount <- 200000
    returnDF <- data.frame(c(0), row.names = c("Total medicare wages from W2s"))
    returnDF["Total medicare wages from W2s",] <- sum(as.numeric(incomeDF[c(2,4,6,8),1]))
    returnDF["Unreported Tips: Skip",1] <- 0
    returnDF["Wages from form 8919: Skip",1] <- 0
    returnDF["Total Above",1] <- returnDF["Total medicare wages from W2s",] +returnDF["Unreported Tips: Skip",1]+
                                  returnDF["Wages from form 8919: Skip",1]
    returnDF["Threshold Amount for your filing status",1] <- thresholdAmount
    returnDF["Difference from above",1] <- returnDF["Total Above",1] -returnDF["Threshold Amount for your filing status",1]
    returnDF["Difference from above",1] <- max(returnDF["Difference from above",1],0)
    returnDF["Additional Medicare Taxes (0.9%)",1] <- round(returnDF["Difference from above",1]*0.009,digits = 4)
    # End Part 1 of form 8959 - Starting part 5 Withholding Reconciliation
    returnDF["Total Medicare tax withheld",1] <- sum(as.numeric(incomeDF[c(20,22,24,26),1]))
    returnDF["Total Medicare wages above",1] <- returnDF["Total medicare wages from W2s",]
    returnDF["Regular Medicare Taxes (1.45%)",1] <- round(returnDF["Total Medicare wages above",1]*0.0145,digits = 4)
    returnDF["Additional Medicare tax withheld by employer",1] <- returnDF["Total Medicare tax withheld",1]-returnDF["Regular Medicare Taxes (1.45%)",1]
    returnDF["Additional Medicare tax withheld by employer",1] <- max(returnDF["Additional Medicare tax withheld by employer",1],0)
    returnDF["Line 23: SKip",1] <- 0
    returnDF["Additional Medicare tax withholding",1] <- returnDF["Additional Medicare tax withheld by employer",1] 
    colnames(returnDF) <- taxYear
    return (returnDF)
  } # End addMedicareTaxes function
  netInvestmentTaxes <- function (statusDF, incomeDF,AGI, taxYear){
    # https://www.irs.gov/pub/irs-pdf/i8960.pdf
    # This function will calculate NIT on interest, Or.Div, and net capital gains only.
    thresholdAmount <- 0
    if (statusDF["Filing_Status",1]=="MFJ" | statusDF["Filing_Status",1]=="QW"){
      thresholdAmount <- 250000
    } else if (statusDF["Filing_Status",1]=="MFS"){
      thresholdAmount <- 125000
    } else thresholdAmount <- 200000
    returnDF <- NULL
    returnDF[c("Taxable Interest","Ordinary Dividends")] <- as.numeric(incomeDF[c("Interest","Ordinary_Dividends"),1])
    returnDF["Net Capital Gain or Loss"] <- sum(as.numeric(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),1]))
    returnDF["Net Capital Gain or Loss"] <- max(returnDF["Net Capital Gain or Loss"], -3000)
    returnDF["Total Investment Income"] <- sum(returnDF)
    returnDF["AGI"] <- AGI
    returnDF["Threshold Amount"] <- thresholdAmount
    returnDF["Difference from above"] <- returnDF["AGI"]- returnDF["Threshold Amount"]
    returnDF["Difference from above"] <- max (returnDF["Difference from above"],0)
    returnDF["Amount Subject to NIT"] <- min(returnDF["Difference from above"], returnDF["Total Investment Income"])
    returnDF["Net Investment Income Tax"] <- round(returnDF["Amount Subject to NIT"]*0.038, digits = 4)
    returnDF <- data.frame(returnDF, row.names = names(returnDF))
    colnames(returnDF) <- taxYear
    return (returnDF)
  }# End netInvestmentTaxes function
  additionalTaxesIRA <- function (incomeDF, taxYear){
    # 10% taxes on IRA distribution reported, if exception was not checked.
    returnDF <- NULL
    returnDF["IRAs Distribution"] <- as.numeric(incomeDF["TaxableIRA",1])
    returnDF["Additional IRA Taxes"] <- 0 # assuming that exception was applied
    if (as.numeric(incomeDF["Exception",1])==0){
      returnDF["Additional IRA Taxes"] <- round (returnDF["IRAs Distribution"]*0.1, digits = 4)
    }
    returnDF <- data.frame(returnDF, row.names = names(returnDF))
    colnames(returnDF) <- taxYear
    return (returnDF)
  } # End additionalTaxesIRA function
  #-----------------Main function ----------------------------------------------
  medicareTaxes <- addMedicareTaxes(statusDF, incomeDF, taxYear)
  NIT <- netInvestmentTaxes(statusDF, incomeDF, AGI,taxYear)
  IRATaxes <- additionalTaxesIRA(incomeDF, taxYear)
  returnList <- list()
  addTaxesAmount <- 0
  returnList[["Add_Medicare_Withholding"]] <- 0
  if (medicareTaxes["Additional Medicare Taxes (0.9%)",1]>0){
    returnList[["MedicareTaxes"]] <- medicareTaxes
    addTaxesAmount <- addTaxesAmount+ medicareTaxes["Additional Medicare Taxes (0.9%)",1]
    returnList[["Add_Medicare_Withholding"]] <- medicareTaxes["Additional Medicare tax withholding",1]
  }
  if (NIT["Net Investment Income Tax",1]>0){
    returnList[["NIT"]] <- NIT
    addTaxesAmount <- addTaxesAmount +NIT["Net Investment Income Tax",1]
  }
  if (IRATaxes["Additional IRA Taxes",1]>0){
    returnList[["IRATaxes"]] <- IRATaxes
    addTaxesAmount <- addTaxesAmount + IRATaxes["Additional IRA Taxes",1]
  }
  returnList[["AddTaxesAmount"]] <- addTaxesAmount
  return (returnList)
}
taxSummary <- function (taxYear, statusDF, incomeDF, deductionDF, creditDF){
  # This will be the main helper function, return a vector with all necessary information
  
  # Commonly use variables
  filingStatus <- toupper(statusDF["Filing_Status",1])
  IRAContribution <- as.numeric(deductionDF[c("Your_IRA_Contribution","Spouse_IRA_Contribution"),1])
  itemizedItems <- c("Medical_Exp","State_Local_Taxes", "Real_Estate_Taxes","Personal_Property_Tax",
                     "Mortgage_Interest","Premium_Mortage_Interest","Charitable_Contribution")
  # End Commonly use variables
  # Begin main function
  totalIncomeDF <- totalIncomeCalculation(taxYear = taxYear, incomeDF) # Step 1:Summary Income and taxes
  deductionsAboveAGI <- totalDeductionToAGI(taxYear, statusDF[c("Filing_Status", "Your_Age","Spouse_Age"),1],
                                            deductionDF, totalIncomeDF[,1]) # Step 2: Calculate above AGI deductions
  totalIncomeDF["Total_Income",] <- sum(as.numeric(totalIncomeDF[-c(4,10,11),1])) # Step 3: Calculate total income  
  AGI <- as.numeric(deductionsAboveAGI["Adjusted_Gross_Income",1])
  deductionBelowAGI <- belowAGIDeduction(deductionDF[itemizedItems,1],statusDF = statusDF,
                                         AGI, taxYear) # Step 3: Calculate SD, Itemized, and Exemption
  taxableIncome <- AGI - sum(deductionBelowAGI[[1]][c("Your_Deduction","Exemption_Deduction"),])
  taxableIncome <- max(taxableIncome, 0)
  taxes <- taxCalculation(taxableIncome, incomeDF, filingStatus, taxYear)
  aboveAGIDeduction <- sum(as.numeric(deductionsAboveAGI[c("Educator_Expense","HSA_Deduction_Amt",
                            "Your_IRA_Deduction","Your_Spouse_IRA_Deduction","Student_Loan_Deduction"),1]))
  taxCredits <- creditCalculation(AGI, taxes, incomeDF, statusDF, creditDF,IRAContribution, taxYear)
  addTaxes <- additionalTaxes(statusDF, incomeDF, AGI, taxYear)
  # Finish main function
  # Prepare information to return to caller
  
  nonrefundableAmount <- sum(taxCredits[["Credits Summary"]][1:4,1])
  netTaxesAfterRefundableCrd <- max(taxes -nonrefundableAmount,0)
  totalTaxes <- netTaxesAfterRefundableCrd + addTaxes[["AddTaxesAmount"]]
  totalWitholding <- totalIncomeDF["Total_Taxes_Withheld",1] + addTaxes[["Add_Medicare_Withholding"]]
  summaryV <- c(totalIncomeDF["Total_Income",],aboveAGIDeduction,AGI,
                deductionBelowAGI[[1]]["Your_Deduction",1],
                deductionBelowAGI[[1]]["Exemption_Deduction",1],
                taxableIncome,
                taxes,
                nonrefundableAmount, # this is the total nonrefundable credits
                netTaxesAfterRefundableCrd,
                addTaxes[["AddTaxesAmount"]],
                totalTaxes,
                totalWitholding,
                sum(taxCredits[["Credits Summary"]][5:7,1]),
                totalWitholding + sum(taxCredits[["Credits Summary"]][5:7,1]) - totalTaxes
                )
  
  names(summaryV) <- c("Total_Income", "Above_AGI_Deduction", "AGI", "Standard_Deduction_Or_Itemized",
                       "Exemption_Amount","Taxable_Income", "Taxes_Before_Credits", "Non_refundable_credit",
                       "Net_Taxes_After_Nonrefundable_Credits","Additional_Taxes","Total_Taxes",
                       "Total_Withholding_From_All_Sources","Refundable_Credits", "Your_Refund/Payment")
  
  returnList <- list(summaryV, totalIncomeDF, deductionsAboveAGI, deductionBelowAGI, taxCredits,addTaxes)
}