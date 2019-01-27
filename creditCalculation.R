# Credit calculation module
# Author: Long Nguyen
# Date Created: 01/26/2019

## This module will use to calcualte credits.
library(gdata)
childTaxCrd <- function (AGI, filingStatus, taxYear, numQualifyingChild, creditDF,taxBeforeCredit, 
                         nonRefundableCredits = c(childDependentCareCredit = 0, educationCredit = 0, saverCredit = 0), numQualifyingRelative = 0){
  # Link to IRS website, pub 972: https://www.irs.gov/publications/p972
  # numQualifyingRelative is a variable that will be used for tax year 2018 and later only.
  # tax credit of this is $500 per qualified person, nonrefundable.
  credit_row <- creditDF[creditDF$YEAR == taxYear & grepl(filingStatus, creditDF$FILING.STATUS),]
  phase_out_agi <- as.numeric(credit_row$PHASE_OUT_AGI)
  credit_per_child <- as.numeric(credit_row$CREDIT_PER_CHILD)
  numQualifyingChild <- as.numeric(numQualifyingChild)
  paste ("Num Qualifying child:",numQualifyingChild)
  paste ("Credit per child:",credit_per_child)
  line_1 <- numQualifyingChild * credit_per_child
  paste ("Line 1:", line_1)
}

dependentCareCrd <- function (){
  
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

testing <- function() {
  childTaxCrd_DF <- read.xls(xls = "TaxRates.xls")
  childTaxCrd(100000, "MFJ", 2018, 2, childTaxCrd_DF, 10000)
}
testing()

