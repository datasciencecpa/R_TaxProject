# This will be the main helper module that will interact directly with app.R
# This module will use the help of other modules:
# -deductionCalculation
# -creditCalculation
# to figure out the tax and other things

# Author: Long Nguyen
# Date Created: 01/28/2019

AGICalculation <- function (income){
   rowNames <- c("Total_W2_Wages", "Interest", "Dividends", "Net_Capital_Gain_Loss", "IRA_Distribution", "Total_Income")
   incomeDF <- income()
   AGI_2018 <- c(
     sum(incomeDF[1:4,"Income_Tax_2018"]),
     incomeDF["Interest", 1],
     incomeDF["Ordinary_Dividends", 1],
     sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),1]),
     incomeDF["TaxableIRA",1]
   )
   AGI_2018 <- c(AGI_2018, sum(AGI_2018))
   AGI_2017 <- c(
     sum(incomeDF[1:4, "Income_Tax_2017"]),
     incomeDF["Interest", 2],
     incomeDF["Ordinary_Dividends", 2],
     sum(incomeDF[c("Long_Term_Gains","Short_Term_Gains"),2]),
     incomeDF["TaxableIRA",2]
   )
   AGI_2017 <- c (AGI_2017, sum(AGI_2017))
   
   return (data.frame(AGI_2018, AGI_2017, row.names = rowNames))
}

