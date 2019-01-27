# Credit calculation module
# Author: Long Nguyen
# Date Created: 01/26/2019

## This module will use to calcualte credits.

childTaxCrd <- function (AGI, filingStatus, taxYear, numQualifyingChild, creditDF, numQualifyingRelative = 0){
  # Link to IRS website, pub 972: https://www.irs.gov/publications/p972
  # numQualifyingRelative is a variable that will be used for tax year 2018 and later only.
  # tax credit of this is $500 per qualified person, nonrefundable.
  a <- creditDF$FILING.STATUS
  t <- grepl(filingStatus, a)
  b <- c(TRUE, FALSE, FALSE, FALSE, FALSE)
  print (t)
  print (creditDF[t,])
  print (creditDF[b,])
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