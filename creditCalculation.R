# Credit calculation module
# Author: Long Nguyen
# Date Created: 01/26/2019

## This module will use to calcualte credits.
library(gdata)
library(plyr)
childTaxCrd <- function (AGI, filingStatus, taxYear, numQualifyingChild, creditDF,taxBeforeCredit, 
                         nonRefundableCredits = c(childDependentCareCredit = 0, educationCredit = 0, saverCredit = 0), numQualifyingRelative = 0){
  # Link to IRS website, pub 972: https://www.irs.gov/publications/p972
  # numQualifyingRelative is a variable that will be used for tax year 2018 and later only.
  # tax credit of this is $500 per qualified person, nonrefundable.
  credit_row <- creditDF[creditDF$YEAR == taxYear & grepl(filingStatus, creditDF$FILING_STATUS),]
  phase_out_agi <- as.numeric(credit_row$PHASE_OUT_AGI)
 
  credit_per_child <- as.numeric(credit_row$CREDIT_PER_CHILD)
  credit_per_otherDep <- as.numeric(credit_row$CREDIT_PER_OTHER_DEP)
  numQualifyingChild <- as.numeric(numQualifyingChild)
  numQualifyingRelative <- as.numeric(numQualifyingRelative)

  line_1 <- numQualifyingChild * credit_per_child
  line_1b <- numQualifyingRelative * credit_per_otherDep
  line_1c <- line_1 + line_1b
  line_2 <- AGI
  line_3 <- phase_out_agi
  line_4 <- ifelse(line_2<line_3, 0, round_any(line_2 - line_3, 1000, f= ceiling))
  line_5 <- line_4 *0.05
  line_6 <- ifelse(line_5>line_1c,0, line_1c - line_5)  # this is the net credit after the 5% reduction
  line_7 <- as.numeric(taxBeforeCredit)
  line_8 <- sum(nonRefundableCredits)
  line_9 <- line_7 - line_8                             # This is the net tax after subtraction of other nonrefundable credit
  line_10 <- ifelse (line_6>line_9, line_9, line_6)     # Smaller of net tax above, or net credit. If net credit is higher, additional credit may apply
  line_11 <- ifelse (line_6>line_9, line_6 - line_9, 0)
  resultDF <- c(line_1, line_1b, line_1c, line_2, line_3, line_4, line_5, line_6, line_7, line_8, line_9, line_10, line_11)
  line_1_name <-  paste("Tax_Credit_Per_Qualifying_Child_", numQualifyingChild, sep ="")
  line_1b_name <- paste("Tax_Credit_Per_Other_Dependent_", numQualifyingRelative, sep = "")
  names(resultDF) <- c(line_1_name, line_1b_name,
                       "Total_Tax_Credit", "Your_AGI","Phase_Out_AGI","Amt_Over_AGI", "Reduce_Credit_Amt",
                       "Net_Tax_Credit","Your_Tax","Sum_Other_Nonrefundable_Credit", "Net_Tax_After_Credit_Above", 
                       "Your_Child_Tax_Credit", "Qualify_For_Additional_Credit")
  
  return (data.frame(resultDF)) 
}

dependentCareCrd <- function (summaryDF, filingStatus, incomeDF, creditDF ){
  #https://www.irs.gov/pub/irs-pdf/i2441.pdf
  # Rules: Qualifying child must be under 13 year old/ Or Disabled
  # Rules: Expense can't be more than $3000 for one child, or 6000 for 2 or more child
  LOWER_LIMIT <- 15000
  UPPER_LIMIT <- 43000
  MAX_RATE <- 0.35
  MIN_RATE <- 0.2
  INCREMENT <- 2000
  if (creditDF["Qualifying_Person"]==1){
    creditDF["Expense"] <- ifelse(creditDF["Expense"]>3000, 3000, creditDF["Expense"])
  } else {
    creditDF["Expense"] <- ifelse(creditDF["Expense"]>6000, 6000, creditDF["Expense"])
    creditDF["Qualifying_Person"] <- 2
  }
  print (creditDF)

  # Rules: Caclualte earning for FT Students or Disable Spouse
  # Rules: Earned $250/Child for each month being FT Student or Disabled
  # Rules: If both are ST or disabled, sum of month can't be more than 12
  # Assumption: If user has both earned income, and some months as FTStudent, add both together
  line_4 <- incomeDF[1] + incomeDF[5] # Line 4 equal wages of user in the vector at position 1 and 5
  if (creditDF["You_FT_Student"]==1){ # if FT student checkbox was checked, add additional income for months that were student
    line_4 <- line_4 + 250* creditDF["FT_Student_Month"]* creditDF["Qualifying_Person"]
  }
  line_5 <- line_4
  if (filingStatus == "MFJ"){
    line_5 <-  incomeDF[3] + incomeDF[7] # W-2 income of spouse
    if (creditDF["Spouse_FT_Student"]==1){
      line_5 <- line_5 + 250 * creditDF["Spouse_FT_Student_Month"]* creditDF["Qualifying_Person"]
    }
    if (creditDF["You_FT_Student"]== 1 & creditDF["Spouse_FT_Student"]==1){ # if both are full-time student, can allow additional income for the same month
      # I am making assumption that if the total of months being student are more than 12
      # I will deduct the excess out from income of the higher one.
      # Assuming that husband and wife are not FT student in the same month. Which allowed the max 12 months calculation.
      if ((creditDF["FT_Student_Month"] + creditDF["Spouse_FT_Student_Month"])>12){
        # this is the case when both are students and both sum of months is more than 12
        # Reduce income of the higher one, eithe line 4 or 5 by the extra months above 12
        reductionAmt <- 250*creditDF["Qualifying_Person"]*(creditDF["FT_Student_Month"] + creditDF["Spouse_FT_Student_Month"]-12)
        # print (paste("Reduction Amount: ", reductionAmt))
        if (line_4> line_5){
          line_4 <- line_4 - reductionAmt
          # print (paste("Line_4 afte reduction:", line_4))
        } else {
          line_5 <- line_5 - reductionAmt
          # print (paste("Line_5 after reduction:", line_5))
        }
      } # else: no need to do anything
    }
  }
  # print ("Line 4:")
  # print(line_4)
  # print ("Line 5:")
  # print(line_5)
  line_6 <- min(c(line_4, line_5, creditDF["Expense"])) # getting the smallest
  # print (paste("Smallest: ", line_6))
  # print (paste("Summary DF:", summaryDF))
  AGI <- summaryDF[1] # AGI
  # print (paste("AGI:",AGI))
  if (AGI<LOWER_LIMIT) rate <- MAX_RATE
  else if (AGI>UPPER_LIMIT) rate <- MIN_RATE
  else {
    rate <- MAX_RATE -(round_any((AGI - LOWER_LIMIT)/2000,1, f= ceiling)/100)
  }
  # print(paste("Rate:", rate))
  line_9 <- round(line_6 * rate, digits = 4)
  # print(paste("Eligible credit:", line_9))
  line_10 <- summaryDF[5] # Tax_Amount
  return (min(line_9, line_10))
}

educationalCrd <- function (){
  
}

saverCrd <- function(){
  
}

elderlyDisableCrd <- function (){
  # Schedule R Instruction: https://www.irs.gov/pub/irs-pdf/i1040sr.pdf
  
}
additionalChildTaxCrd <- function (){
   # Form 8812
}



