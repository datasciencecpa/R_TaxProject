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
  print (deductionsDF)
  hsaAmt <- HSADeduction(ages = c(statusDF[c("Your_Age", "Spouse_Age"), "Status_2018"]), 
                         contributionAmt = c(deductionsDF["HSA_Contribution", "Deduction_2018"], 
                                             deductionsDF["HSA_Contribution_Per_W2", "Deduction_2018"]),
                         hsaPlan = deductionsDF["HSA_Plan_Type", "Deduction_2018"],
                         taxYear = 2018
                         )
  print (hsaAmt)
  # This function will calcualte taxpayer eligible deductions before AGI
  #Deduction_Type <- c ("Educatior_Expense", "HSA_Contribution", "HSA_Excess_Amount","IRA_Contribution", "IRA_Excess_Amount", "Student_Loan_Deduction")
  # Deductions_2018 <- c(
  #   ifelse (statusDF["Filing_Status", "Status_2018"] == "MFJ", deductionsDF["Educator_Expense", "Deduction_2018"], 
  #           ifelse(deductionsDF["Educator_Expense", "Deduction_2018"]<=250,deductionsDF["Educator_Expense", "Deduction_2018"], 250))
  # )
  # Deduction_2017 <- c(
  #   ifelse (statusDF["Filing_Status", "Status_2017"] == "MFJ", deductionsDF["Educator_Expense", "Deduction_2017"], 
  #           ifelse(deductionsDF["Educator_Expense", "Deduction_2017"]<=250,deductionsDF["Educator_Expense", "Deduction_2017"], 250))
  # )
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

