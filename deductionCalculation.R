# This module was used to calculate above the line deductions, and figure out
# amount of standard deduction or itemized deduction, which ever is larger.

# Author: Long Nguyen
# Date Created: 01/28/2019

# HSA Deductions, see form 8889 Instruction
# https://www.irs.gov/pub/irs-pdf/i8889.pdf

library (gdata)
library(plyr)
SLTbl <- read.xls(xls="TaxRates.xls", sheet=9)
hsaTbl <- read.xls(xls="TaxRates.xls", sheet= 10)
IRATbl <- read.xls(xls = "TaxRates.xls", sheet = 11)
SDTbl <- read.xls(xls = "TaxRates.xls", sheet = 4)
IRATbl$LOWER_AGI <- as.numeric(IRATbl$LOWER_AGI)
IRATbl$UPPER_AGI <- as.numeric(IRATbl$UPPER_AGI)
HSADeduction <- function (ages, contributionAmt, hsaPlan, taxYear) {
  # In order to calculate HSA deduction, this function will need the following information.
  # * Ages - this will be the ages of individual and/or spouses if both contribute to HSA either as Single or Family.
  # * Individual with ages >55 can eligible for HSA catch-up contribution of $1000. Assuming that they are eligible for entire year
  # * contributionAmt - This is a vector contains individual contribution to HSA (outsite of employee contribution), and employer contribution
  # * hsaPlan - which has either Single/Family value
  # * taxYear
  maxContribution <- as.numeric(hsaTbl[hsaTbl$YEAR == taxYear & hsaTbl$TYPE == toupper(hsaPlan), "MAX_CONTRIBUTION"])
  if (sum(ages>=55) ==2 ) {
    if (hsaPlan == "Family") catchupContribution = 2000
    else catchupContribution = 1000
  } else if (sum(ages>=55) == 1) {
    catchupContribution = 1000
  } else catchupContribution = 0
  maxContribution <- maxContribution + catchupContribution
  employeeContribution <- contributionAmt[1]
  employerContribution <- contributionAmt[2]
  eligibleAmount <- maxContribution - employerContribution # this is the maximum amount of additional HSA contribution employee can contribute
  return (c(ifelse (eligibleAmount>employeeContribution, employeeContribution, eligibleAmount), catchupContribution, maxContribution))
}
IRADeduction <- function (taxYear, IRAcover, filingStatus, ages, MAGI, earnedIncome, IRAAmount){
  # For IRA, the contribution limit is $5500 for age <50. Catchup contribution is $1000. This information is the same for both 2017 & 2018
  # IRA contribution rules: Must have earned income, include alimony for purpose of this project. Other earned incomes are not considered
  # since this app used limited income types.
  # IRA deduction will be calculated as follow:
  # 1. If user was not covered by retirement plan --> Full deduction amount up to limit
  # 2a. If user was covered, Single, HOH, or MFS - check with limit.
  # 2b. If user was covered, MFJ - Both, or only one. Using appropriate AGI limit
  filingStatus <- toupper(filingStatus)
  IRAMultiply <- function (upperAGI, MAGI, rate){
    #This function will calculate the eligible IRA deduction as stated in step 7 of the Worksheet

    eligibleAmt <- round_any((upperAGI - MAGI)*rate, 10,f = ceiling)
    # print (paste("Eligible amount from IRA mul: ", eligibleAmt))
    return (ifelse (eligibleAmt>200, eligibleAmt, 200))
  }
  # print(paste("Ages:", ages, "Earned Income: ", earnedIncome))
  # print (paste("MAGI: ", MAGI))
  if (all (ages>70) | earnedIncome<=0) return (c(0,0)) # User can't contribute to IRA with age greater than 70 or zero earned income
  IRAAmount[which(ages>70)] <- 0
  rowValues <- IRATbl[IRATbl$YEAR == taxYear & grepl(filingStatus, IRATbl$FILING_STATUS) & IRATbl$COVERED == "YES",]
  limit <- 0
  if (all(IRAcover == "NO") | MAGI< rowValues$LOWER_AGI ){
    # print ("Meeting condition of no-one cover or lower than lowerAGI")
    if (grepl(filingStatus, "SINGLE/HOH/MFS/QW")) {
      if (ages[1]>=50){
        limit <- ifelse(earnedIncome <=6500,earnedIncome, 6500)
      } else {
        limit <- ifelse(earnedIncome <=5500,earnedIncome, 5500)
      }
      IRAAmount[1] <- ifelse (IRAAmount[1]<=limit, IRAAmount[1], limit)
      IRAAmount[2] <- 0
    } else { # MFJ
      if (ages[1]>=50){
        limit <- ifelse(earnedIncome <=6500,earnedIncome, 6500)
      } else {
        limit <- ifelse(earnedIncome <=5500,earnedIncome, 5500)
      }
      IRAAmount[1] <- ifelse (IRAAmount[1]<=limit, IRAAmount[1], limit)
      earnedIncome <- earnedIncome - limit # Reduce earnedIncome for amount of limit that already apply for IRAAmount[1]
      # Apply for spouse
      if (ages[2]>=50){
        limit <- ifelse(earnedIncome <=6500,earnedIncome, 6500)
      } else {
        limit <- ifelse(earnedIncome <=5500,earnedIncome, 5500)
      }
      IRAAmount[2] <- ifelse (IRAAmount[2]<=limit, IRAAmount[2], limit)
    }
  } else if ( (MAGI>rowValues$UPPER_AGI & all(IRAcover =="YES"))| (MAGI>rowValues$UPPER_AGI &grepl(filingStatus, "SINGLE/HOH/MFS/QW"))) {
    # print (paste("Above AGI Limit: ", rowValues$UPPER_AGI))
    IRAAmount <- c(0,0)
  } 
  else {
    if (grepl(filingStatus, "SINGLE/HOH/MFS/QW")) {
      if (ages[1]>=50){
        limit <- ifelse(earnedIncome <= (IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.65)), earnedIncome, IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.65))
      } else {
        limit <- ifelse(earnedIncome <= (IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.55)), earnedIncome, IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.55))
      }
      IRAAmount[1]<- ifelse (IRAAmount[1]<=limit, IRAAmount[1], limit)
      IRAAmount[2] <- 0
    } 
    else { # filingStatus = MFJ, two situation can occur: Either users have retirement coverage, or one of them have coverage.
      rowValues_2 <- IRATbl[IRATbl$YEAR == taxYear & grepl(filingStatus, IRATbl$FILING_STATUS) & IRATbl$COVERED == "ONE",]
      if (MAGI > rowValues_2$UPPER_AGI) { # None of the IRA Contribution are eligible
        IRAAmount <- c(0,0)
      } 
      else{
        if (any(IRAcover =="NO")){ # Need to update rowValues to get value with higher AGI limit
          ind_N <- which(IRAcover =="NO")
          ind_Y <- which(IRAcover =="YES")
          if (MAGI<rowValues$UPPER_AGI){
            # This situation is when MAGI is between the lowerAGI and upperAGI of the covered person
            # Which, non-covered person would be allowed full deduction
            # print ("MAGI is lower than the covered person UpperAGI")
            if (ages[ind_N]>=50){
              limit <- ifelse(earnedIncome <=6500,earnedIncome, 6500)
            } else {
              limit <- ifelse(earnedIncome <=5500,earnedIncome, 5500)
            }
            IRAAmount[ind_N] <-  ifelse (IRAAmount[ind_N]<=limit, IRAAmount[ind_N], limit)
            earnedIncome <- earnedIncome -limit
             if (ages[ind_Y]>=50) {
               limit <- ifelse(earnedIncome<=(IRAMultiply(rowValues$UPPER_AGI,MAGI,0.325)), earnedIncome, IRAMultiply(rowValues$UPPER_AGI,MAGI,0.325))
             } else {
               limit <- ifelse(earnedIncome<=(IRAMultiply(rowValues$UPPER_AGI,MAGI,0.275)), earnedIncome, IRAMultiply(rowValues$UPPER_AGI,MAGI,0.275))
             }
            IRAAmount[ind_Y]<-  ifelse (IRAAmount[ind_Y]<=limit, IRAAmount[ind_Y], limit)
          } 
          else {
            print ("MAGI is higher than the covered person upperAGI. zero deduction for covered person")
            IRAAmount[ind_Y] <- 0
            if (MAGI<rowValues_2$LOWER_AGI) { #Full deduction for non-cover user
              # print (paste("MAGI is lower than the non-cover person lowerAGI: ", MAGI))
              if (ages[ind_N]>=50){
                limit <- ifelse(earnedIncome <=6500,earnedIncome, 6500)
              } else {
                limit <- ifelse(earnedIncome <=5500,earnedIncome, 5500)
              }
            }
            else {
              # print ("MAGI is between lower AGI and upperAGI")
              if (ages[ind_N]>=50){
                limit <- ifelse (earnedIncome<=(IRAMultiply(rowValues_2$UPPER_AGI,MAGI,0.65)), earnedIncome, 
                                                IRAMultiply(rowValues_2$UPPER_AGI,MAGI,0.65))
              } else {
                limit <- ifelse (earnedIncome<=(IRAMultiply(rowValues_2$UPPER_AGI,MAGI,0.55)), earnedIncome, 
                                 IRAMultiply(rowValues_2$UPPER_AGI,MAGI,0.55))
              }
            }

            IRAAmount[ind_N] <-  ifelse (IRAAmount[ind_N]<=limit, IRAAmount[ind_N], limit)
            IRAAmount[ind_Y] <- 0
          }
        }
        else { # Both were covered and less than the non-cover upper_agi
          # print ("MAGI is between lowerAGI and upperAGI of cover person, both covered")
          if (ages[1]>=50) {
            limit <- ifelse(earnedIncome<=IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.325), earnedIncome, IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.325))
          } else {
            limit <- ifelse(earnedIncome<=IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.275), earnedIncome, IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.275))
          }
          IRAAmount[1] <- ifelse(IRAAmount[1]<=limit, IRAAmount[1], limit)
          earnedIncome <- earnedIncome - limit
          if (ages[2]>=50) {
            limit <- ifelse(earnedIncome<=IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.325), earnedIncome, IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.325))
          } else {
            limit <- ifelse(earnedIncome<=IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.275), earnedIncome, IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.275))
          }
          IRAAmount[2] <- ifelse(IRAAmount[2]<=limit, IRAAmount[2], limit)
        }
      } 
    } 
  }# End MFJ above the LowerAGI of covered person
  return (IRAAmount)
}
studentLoan <- function (interest, MAGI, filingStatus, taxYear) {
  filingStatus <- toupper(filingStatus)
  if (filingStatus == "MFS") return (0)
  rowValues <- SLTbl[SLTbl$YEAR == taxYear & grepl(filingStatus, SLTbl$FILING_STATUS),]
  print (rowValues)
  if (MAGI<=rowValues$LOWER_AGI) { # Full student loan deduction
    return (interest)
  } else if (MAGI>rowValues$LOWER_AGI & MAGI <=rowValues$UPPER_AGI){
    print ("Within the range limit")
    incomeRange <- rowValues$UPPER_AGI - rowValues$LOWER_AGI
    multiplier <- round((rowValues$UPPER_AGI - MAGI)/incomeRange, digits = 3)
    return (interest*(1-multiplier))
  } else {
    return (0)
  }
}
itemizedDeduction <- function (deductionDF, statusDF, AGI){
  statusDF["Filing_Status", ] <- toupper(as.character(statusDF["Filing_Status", ]))
  deductionDF$Deduction_2018 <- as.numeric(deductionDF$Deduction_2018)
  deductionDF$Deduction_2017 <- as.numeric(deductionDF$Deduction_2017)
  row_2017 <- SDTbl[SDTbl$YEAR == 2017 & grepl(statusDF["Filing_Status", "Status_2017"], SDTbl$FILING_STATUS),]
  row_2018 <- SDTbl[SDTbl$YEAR == 2018 & grepl(statusDF["Filing_Status", "Status_2018"], SDTbl$FILING_STATUS),]

  num_2017 <- sum(as.numeric(statusDF[c("Your_Age", "Spouse_Age"),"Status_2017"])>=65) +
              sum(as.logical(statusDF[c("You_Blind", "Spouse_Blind"), "Status_2017"]))
  num_2018  <-  sum(as.numeric(statusDF[c("Your_Age", "Spouse_Age"),"Status_2018"])>=65) +
                sum(as.logical(statusDF[c("You_Blind", "Spouse_Blind"), "Status_2018"]))

  SD_2017 <- row_2017$AMOUNT + num_2017*row_2017$ADDITIONAL_PER_CONDITION
  SD_2018 <- row_2018$AMOUNT + num_2018*row_2018$ADDITIONAL_PER_CONDITION
  deductionDF["Medical_Exp", ] <- ifelse ((deductionDF["Medical_Exp",] - AGI *0.075)>0,(deductionDF["Medical_Exp",] - AGI *0.075),0 )
  print (deductionDF)
  
}