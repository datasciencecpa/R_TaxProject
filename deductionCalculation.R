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
ExempTbl <- read.xls(xls = "TaxRates.xls", sheet = 3)
ExempTbl$LOWER_AMT <- as.numeric(ExempTbl$LOWER_AMT)
ExempTbl$UPPER_AMT <- as.numeric(ExempTbl$UPPER_AMT)
ExempTbl$AMOUNT <- as.numeric(ExempTbl$AMOUNT)
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
  eligibleAmount <- maxContribution - employerContribution # this is the maximum amount of additional HSA contribution employee can contribution
  eligibleAmount <- max(eligibleAmount, 0)
  HSA_Deduction <- min(eligibleAmount, employeeContribution)
  return (c(HSA_Deduction, catchupContribution, maxContribution))
}
IRADeduction <- function (taxYear, IRAcover, filingStatus, ages, MAGI, earnedIncome, IRAAmount){
  # For IRA, the contribution limit is $5500 for age <50. Catchup contribution is $1000. This information is the same for both 2017 & 2018
  # IRA contribution rules: Must have earned income, include alimony for purpose of this project. Other earned incomes are not considered
  # since this app used limited income types.
  # IRA deduction will be calculated as follow:
  # 1. If user was not covered by retirement plan --> Full deduction amount up to limit
  # 2a. If user was covered, Single, HOH, or MFS - check with limitation amount.
  # 2b. If user was covered, MFJ - Both, or only one. Using appropriate AGI limit
  # Parameters: taxYear: use to identify row in Excel worksheet
  # IRAcover: vector indicates whether user/spouse were covered
  # filingStatus: user filing status
  # ages: vector that contains user ages
  # MAGI: Modified AGI
  # earnedIncome: earned income calculated.
  # Begin function ----------------------------------------------------------------
  IRAMultiply <- function (upperAGI, MAGI, rate){
    #This function will calculate the eligible IRA deduction as stated in step 7 of the Worksheet
    eligibleAmt <- round_any((upperAGI - MAGI)*rate, 10,f = ceiling)
    # print (paste("Eligible amount from IRA mul: ", eligibleAmt))
    return (ifelse (eligibleAmt>200, eligibleAmt, 200))
  }
  #----------------------------------------------------------------
  # print(paste("Ages:", ages, "Earned Income: ", earnedIncome))
  # print (paste("MAGI: ", MAGI))
  if (all (ages>70) | earnedIncome<=0) return (c(0,0)) # User can't contribute to IRA with age greater than 70 or with zero earned income
  IRAAmount[which(ages>70)] <- 0 
  rowValues <- IRATbl[IRATbl$YEAR == taxYear & grepl(filingStatus, IRATbl$FILING_STATUS) & IRATbl$COVERED == "YES",]
  # Calculate limit that would be applied. Either the lower of Earned Income or maxContribution would be allowed
  limit_1 <- 0
  limit_2 <- 0
  if (ages[1]>=50){
    limit_1<- min(earnedIncome, 6500)
  } else {
    limit_1<- min(earnedIncome, 5500)
  }
  earnedIncome <- earnedIncome -limit_1
  if (ages[2]>=50){
    limit_2<- min(earnedIncome, 6500)
  } else {
    limit_2<- min(earnedIncome, 5500)
  }  
  # End calculation limits -----------------------------------------------------------
  if (all(IRAcover == "NO") | MAGI< rowValues$LOWER_AGI ){
    # print ("Meeting condition of no-one cover or lower than lowerAGI")
    if (grepl(filingStatus, "SINGLE/HOH/MFS/QW")) {
      IRAAmount[1] <- min (IRAAmount[1],limit_1)
      IRAAmount[2] <- 0
    } 
    else { # MFJ
      IRAAmount[1] <- min (IRAAmount[1], limit_1)
      IRAAmount[2] <- min (IRAAmount[2],limit_2)
    } # End MFJ and first condition.
  } # End first condition
  else if ( (MAGI>rowValues$UPPER_AGI & all(IRAcover =="YES"))| (MAGI>rowValues$UPPER_AGI &grepl(filingStatus, "SINGLE/HOH/MFS/QW"))) {
    # print (paste("Above AGI Limit: ", rowValues$UPPER_AGI))
    IRAAmount <- c(0,0)
  } 
  else { # This situation is either MAGI between the range, or one spouse was not covered
    if (grepl(filingStatus, "SINGLE/HOH/MFS/QW")) { # Not MFJ Situation
      if (ages[1]>=50){
        limit <- min(earnedIncome, (IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.65)))
      } else {
        limit <- min(earnedIncome , (IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.55)))
      }
      IRAAmount[1]<- min (IRAAmount[1],limit)
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
          if (ages[ind_N]>=50){
            limit_1<- min(earnedIncome, 6500)
          } else {
            limit_1<- min(earnedIncome, 5500)
          }
          if (MAGI<rowValues$UPPER_AGI){
            # This situation is when MAGI is between the lowerAGI and upperAGI of the covered person
            # Which, non-covered person would be allowed full deduction
            # print ("MAGI is lower than the covered person UpperAGI")
            IRAAmount[ind_N] <-  min (IRAAmount[ind_N],limit_1)
            earnedIncome <- earnedIncome -IRAAmount[ind_N]
             if (ages[ind_Y]>=50) {
               limit <- min(earnedIncome,(IRAMultiply(rowValues$UPPER_AGI,MAGI,0.325)))
             } else {
               limit <- min(earnedIncome,(IRAMultiply(rowValues$UPPER_AGI,MAGI,0.275)))
             }
            IRAAmount[ind_Y]<-  min (IRAAmount[ind_Y],limit)
          } 
          else {
            # print ("MAGI is higher than the covered person upperAGI. zero deduction for covered person")
            IRAAmount[ind_Y] <- 0
            if (MAGI<rowValues_2$LOWER_AGI) { #Full deduction for non-cover user
              # print (paste("MAGI is lower than the non-cover person lowerAGI: ", MAGI))
              limit <- limit_1
            }
            else {
              # print ("MAGI is between lower AGI and upperAGI")
              if (ages[ind_N]>=50){
                limit <- min (earnedIncome,(IRAMultiply(rowValues_2$UPPER_AGI,MAGI,0.65)))
              } else {
                limit <- min (earnedIncome,(IRAMultiply(rowValues_2$UPPER_AGI,MAGI,0.55)))
              }
            }

            IRAAmount[ind_N] <-  min (IRAAmount[ind_N],limit)
          }
      }
        else { # Both were covered and less than the covered upper_agi
          # print ("MAGI is between lowerAGI and upperAGI of cover person, both covered")
          if (ages[1]>=50) {
            limit <- min(earnedIncome,IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.325))
          } else {
            limit <- min(earnedIncome,IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.275))
          }
          IRAAmount[1] <- min(IRAAmount[1],limit)
          earnedIncome <- earnedIncome - limit
          if (ages[2]>=50) {
            limit <- min(earnedIncome,IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.325))
          } else {
            limit <- min(earnedIncome,IRAMultiply(rowValues$UPPER_AGI, MAGI, 0.275))
          }
          IRAAmount[2] <- min(IRAAmount[2],limit)
        }
      } 
    } 
  }# End MFJ above the LowerAGI of covered person
  return (IRAAmount)
}
studentLoan <- function (interest, MAGI, filingStatus, taxYear) {

  interest <- as.numeric(interest)
  MAGI <- as.numeric(MAGI)
  if (filingStatus == "MFS") return (0)
  rowValues <- SLTbl[SLTbl$YEAR == taxYear & grepl(filingStatus, SLTbl$FILING_STATUS),]
  # print (rowValues)
  if (MAGI<=rowValues$LOWER_AGI) { # Full student loan deduction
    # print ("lower AGI, full deduction")
    return (interest)
  } else if (MAGI>rowValues$LOWER_AGI & MAGI <rowValues$UPPER_AGI){
    # print ("In between AGI")
    incomeRange <- rowValues$UPPER_AGI - rowValues$LOWER_AGI
    multiplier <- round((MAGI - rowValues$LOWER_AGI)/incomeRange, digits = 3)
    # print (multiplier)
    return (interest*(1-multiplier))
  } else {
    return (0)
  }
}
SDExemptionDeduction <- function (deductionDF, statusDF, AGI, taxYear){
  # This function will calculate both itemized and SD for a single tax year
  # Return a vector
  # Parameters: deductionDF: Vector that contains information about itemized expenses
  # statusDF: dataframe
  # AGI: vector of AGI income
  # taxYear: Contains tax year
  PMICalculation <- function (AGI, PMIAmount, filingStatus){
    # This function is used to calculate eligible PMI deduction for tax year 2017
    # Currently, PMI was not extended for tax year 2018
    # Starting phase-out amount was $100,000
    # Max phase-out amount was $109,000 for MFJ
    min_phase_out <- ifelse (filingStatus =="MFS", 50000,100000)
    percentage <- 0 
    if  (AGI > min_phase_out) { # calculate limitation
      if (filingStatus == "MFS"){
        round_to <- 500
        denominator <- 5000
      } else {
        round_to <- 1000
        denominator <- 10000
      }  
      excess_amt <- round_any(AGI - min_phase_out, round_to,f=ceiling)
      percentage <- round(excess_amt/denominator, digits = 4)
      percentage <- ifelse (percentage>=1, 1, percentage)
    }
    return (PMIAmount - PMIAmount*percentage)
  } # End Internal function
  ExemptionAmount <- function (AGI_2017, statusDF) { 
    # This function applied only for tax year 2017
    rowValue <- ExempTbl[grepl(statusDF["Filing_Status", "Status_2017"], ExempTbl$FILING_STATUS),]
    num <- 1 # Variable to store number of exemptions
    if (statusDF["Filing_Status", "Status_2017"] == "MFJ") num <- 2
    num <- num + sum(as.numeric(statusDF$Status_2017[2:4]))
    ExemptionAmt <- num * rowValue$AMOUNT
    if ((AGI_2017 > rowValue$LOWER_AMT) & (AGI_2017<rowValue$UPPER_AMT)) {
      line_5 <- AGI_2017 - rowValue$LOWER_AMT
      line_6 <- round_any(line_5/2500, 1, f=ceiling)
      print (paste("Line 6:", line_6))
      line_7 <- line_6* 0.02
      ExemptionAmt <- ExemptionAmt - (ExemptionAmt* line_7)
    } else if (AGI_2017>rowValue$UPPER_AMT) {
      ExemptionAmt <- 0
    }
    return (ExemptionAmt)
  }
  # ------------ Function begin ----------------------#
  # Step 1: Calculate amount of SD based on filing status, conditions (ages, blinds)
  # ---- Create variables to store important values for this function
  standardDeduction <- 0
  totalItemizeAmount <- 0
  exemptionAmt <- 0
  itemizedItems <- c("Medical_Exp","State_Local_Taxes", "Real_Estate_Taxes","Personal_Property_Tax",
                     "Mortgage_Interest","Premium_Mortage_Interest","Charitable_Contribution", "Total_Itemized_Amount")
  # ----------------------------------------------------------------------------------------
  rowValues <- SDTbl[SDTbl$YEAR == taxYear & grepl(statusDF["Filing_Status", 1], SDTbl$FILING_STATUS),]
  # print (rowValues)

  num_condition <- sum(as.numeric(statusDF[c("Your_Age", "Spouse_Age"),1])>=65) +
              sum(as.logical(statusDF[c("You_Blind", "Spouse_Blind"), 1]))
  

  standardDeduction <- rowValues$AMOUNT + num_condition*rowValues$ADDITIONAL_PER_CONDITION
  # print (paste("Standard Deduction Amount:", standardDeduction))
  # End Step 1 ------------------------------------------------------------------------------------------
  # Step 2: Calculate Itemized Deductions
  deductionDF[1] <- max((deductionDF[1] - AGI*0.075),0) # Eligible medical expense
  if (taxYear=="2018") {
    deductionDF[6] <- 0 # PMI was not extended for tax year 2018
    deductionDF[7] <- min (deductionDF[7], AGI*0.6) # Calculate donation limitation
    totalSALTDDeductions <- sum(deductionDF[2:4])
    excess_SALT_2018 <- 0
    if (totalSALTDDeductions>10000) { # New SALT limitation for tax year 2018
      excess_SALT_2018 <- totalSALTDDeductions -10000
      deductionDF[2] <- deductionDF[2] - (deductionDF[2]/totalSALTDDeductions * excess_SALT_2018)
      deductionDF[3] <- deductionDF[3] - (deductionDF[3]/totalSALTDDeductions * excess_SALT_2018)
      deductionDF[4] <- deductionDF[4] - (deductionDF[4]/totalSALTDDeductions * excess_SALT_2018)
    }
    totalItemizeAmount <- sum(deductionDF)
    deductionDF[8] <- totalItemizeAmount
  } 
  else if (taxYear =="2017"){
    # Special situation:
    # Exemption was allowed for tax year 2017
    # PMI was allowed
    # Charitable Donation was based on 50% maximum of AGI
    # Itemized amount and exemption were subjected to phase-out based on income
    #---------------------------------------------------------------------------------------------------------------------
    deductionDF[6] <- PMICalculation(AGI, deductionDF[6], (statusDF["Filing_Status", 1]))
    deductionDF[7] <- min (deductionDF[7], AGI*0.5) # Calculate donation limitation
    totalItemizeAmount <- sum(deductionDF)
    # print (totalItemizeAmount)
    # Figure out the itemized deduction amount for AGI above phase-out limit.
    if (AGI>rowValues$PHASE_OUT) {
      # Follow instructions from Itemized Deductions Worksheet
      
      if (sum(deductionDF[c(1,7)]) < totalItemizeAmount) { # Step 3, Box Yes checked
        line_3 <- totalItemizeAmount - sum(deductionDF[c(1,7)])
        line_4 <- line_3*0.8
        line_8 <- (AGI - rowValues$PHASE_OUT)*0.03
        line_9 <- min(line_4,line_8)
        totalItemizeAmount <- totalItemizeAmount - line_9
      }
    }
    deductionDF[8] <- totalItemizeAmount
    exemptionAmt <- ExemptionAmount(AGI_2017 = AGI, statusDF = statusDF)
  }

  maxDeduction <- max (totalItemizeAmount, standardDeduction)
  Below_AGI_Deduction <- c(standardDeduction, totalItemizeAmount, maxDeduction,exemptionAmt)
  rowNames <- c("Standard_Deduction", "Total_Itemized_Deduction", "Your_Deduction", "Exemption_Deduction")
  return (list(data.frame(Below_AGI_Deduction, row.names = rowNames),
            data.frame(deductionDF, row.names = itemizedItems)))
}