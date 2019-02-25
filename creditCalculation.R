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
  if (creditDF[2]==1){
    creditDF[3] <- ifelse(creditDF[3]>3000, 3000, creditDF[3])
  } else {
    creditDF[3] <- ifelse(creditDF[3]>6000, 6000, creditDF[3])
    creditDF[2] <- 2
  }
  print (creditDF)

  # Rules: Caclualte earning for FT Students or Disable Spouse
  # Rules: Earned $250/Child for each month being FT Student or Disabled
  # Rules: If both are ST or disabled, sum of month can't be more than 12
  # Assumption: If user has both earned income, and some months as FTStudent, add both together
  line_4 <- incomeDF[1]
  if (creditDF[4]==1){
    line_4 <- line_4 + 250* creditDF[5]* creditDF[2]
    print (line_4)
  }
  line_5 <- line_4
  if (filingStatus == "MFJ"){
    line_5 <-  incomeDF[3]
    if (creditDF[6]==1){
      line_5 <- line_5 + 250 * creditDF[7]* creditDF[2]
      print (paste("Line_5:", line_5))
    }
    if (creditDF[4]== 1 & creditDF[6]==1){
      if ((creditDF[5] + creditDF[7])>12){
        # this is the case when both are students and both sum of months is more than 12
        # Reduce income of the higher one, eithe line 4 or 5 by the extra months above 12
        reductionAmt <- 250*creditDF[2]*(creditDF[5] + creditDF[7]-12)
        if (line_4> line_5){
          line_4 <- line_4 - reductionAmt
          print (paste("Line_4 afte reduction:", line_4))
        } else {
          line_5 <- line_5 - reductionAmt
          print (paste("Line_5 after reduction:", line_5))
        }
      } # else: no need to do anything
    }
  }
  print ("Line 4:")
  print(line_4)
  print ("Line 5:")
  print(line_5)
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



