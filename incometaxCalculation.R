# This will be the main helper module that will interact directly with app.R
# This module will use the help of other modules:
# -deductionCalculation
# -creditCalculation
# to figure out total income, total deductions above AGI, AGI, standard/itemized deductions, credits, and final tax amount

# Author: Long Nguyen
# Date Created: 01/28/2019
source ("deductionCalculation.R")
totalIncomeCalculation <- function (incomeDF){
   # This function will sum up total income user entered from the income section.
   # This function will ignore income with value = 0, assuming that user did not have that type of income in both year.
   # Special handle for net capital gain/loss so that maximum of -$3000 are allowed. 
   Income_Type <- c("Total_W2_Wages", "Interest", "Dividends", "Taxable_Refunds","Alimony", "Net_Capital_Gain_Loss",
                 "IRA_Distribution", "Unemployment Income")

   AGI_2018 <- c(
     sum(incomeDF[1:4,"Income_Tax_2018"]),
     incomeDF["Interest", 1],
     incomeDF["Ordinary_Dividends", 1],
     incomeDF["Tax_Refunds", 1],
     incomeDF["Alimony",1],
     ifelse (sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),1])<0,-3000,sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),1])),
     incomeDF["TaxableIRA",1],
     incomeDF["Unemployment_Income", 1]
   )

   AGI_2017 <- c(
     sum(incomeDF[1:4, "Income_Tax_2017"]),
     incomeDF["Interest", 2],
     incomeDF["Ordinary_Dividends", 2],
     incomeDF["Tax_Refunds", 2],
     incomeDF["Alimony",2],
     ifelse (sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),2])<0,-3000,sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),2])),
     incomeDF["TaxableIRA",2],
     incomeDF["Unemployment_Income", 2]
   )
   valueRow <- AGI_2018 !=0 | AGI_2017 !=0  # Interested in non-zero value income type.
   AGI_2018 <- AGI_2018[valueRow]
   AGI_2017 <- AGI_2017[valueRow]
   Income_Type <- Income_Type[valueRow]
   #AGI_2017 <- c (AGI_2017, sum(AGI_2017))
   return (data.frame(Income_Type,AGI_2018, AGI_2017))
}
totalDeductionToAGI <- function (deductionsDF, statusDF) {
  valueRow <- deductionsDF$Deduction_2018 !=0 | deductionsDF$Deduction_2017 !=0
  deductionsDF <- deductionsDF[valueRow,]
  Deduction_Type <- c ("Educator_Expense", "HSA_Contribution", "HSA_Excess_Amount","IRA_Contribution", "IRA_Excess_Amount", "Student_Loan_Deduction")
  rowNames <- rownames(deductionsDF)
  # Iterate through rowNames to see which above AGI deductions need to verify
  # Starting with Educator_Expense
  returnDF <- data.frame (Deduction_2018 = 0, Deduction_2017 = 0, row.names = "No_Deduction") # This is the dataframe that use to hold return values for this function
  if (any(rowNames == Deduction_Type[1])){ # Check if Educator_Expense still in the deductonDF after elimination
      expense_2018 <- ifelse (statusDF["Filing_Status", "Status_2018"] != "MFJ",
                        ifelse (deductionsDF["Educator_Expense", "Deduction_2018"]<=250,deductionsDF["Educator_Expense", "Deduction_2018"], 250 ),
                          deductionsDF["Educator_Expense", "Deduction_2018"])
      expense_2017 <- ifelse(statusDF["Filing_Status", "Status_2017"] != "MFJ",
                        ifelse (deductionsDF["Educator_Expense", "Deduction_2017"]<=250,deductionsDF["Educator_Expense", "Deduction_2017"], 250 ),
                          deductionsDF["Educator_Expense", "Deduction_2017"])
      returnDF["Educator_Expense",] <- c(expense_2018, expense_2017)
      # print (c(expense_2018, expense_2017))
  } # Finish checking educator expense
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
      print ("Testing")
      returnDF["Excess_Contribution",] <- as.numeric(returnDF["HSA_Contribution", ]) - as.numeric(returnDF["HSA_Deduction_Amt",])
      print (returnDF)
    }
  }
  if (returnDF["HSA_Deduction_Amt", 1]<0) returnDF["HSA_Deduction_Amt", 1] <- 0
  if (returnDF["HSA_Deduction_Amt",2]<0) returnDF["HSA_Deduction_Amt",2] <- 0
  if (nrow(returnDF)>1) returnDF <- returnDF[-1,]
  return (returnDF) 

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
  return (rbind(df, df_1))
}

