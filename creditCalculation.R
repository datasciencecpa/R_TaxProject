# Credit calculation module
# Author: Long Nguyen
# Date Created: 01/26/2019

## This module will use to calcualte credits.
library(gdata)
library(plyr)
LLTbl <- read.xls("TaxRates.xls", sheet = 8)
SaverTbl <- read.xls("TaxRates.xls", sheet = 12)
CTCTbl <- read.xls("TaxRates.xls", sheet = 5)
EICTbl <- read.xls("TaxRates.xls", sheet = 13)
childTaxCrd <- function (taxYear, AGI, taxes, statusDF,sumOtherCredits){
  # Link to IRS website, pub 972: https://www.irs.gov/publications/p972
  # Parameters:
  # * taxYear: Numeric
  
  # * sumOtherCredits : Numeric that contains total of other non-refundable credits
  
  credit_row <- CTCTbl[CTCTbl$YEAR == taxYear & grepl(toupper(statusDF["Filing_Status",1]), CTCTbl$FILING_STATUS),]
  # print (credit_row)
  phase_out_agi <- as.numeric(credit_row$PHASE_OUT_AGI)
 
  credit_per_child <- as.numeric(credit_row$CREDIT_PER_CHILD)
  credit_per_otherDep <- as.numeric(credit_row$CREDIT_PER_OTHER_DEP)
  numQualifyingChild <- as.numeric(statusDF["Qualifying_Child_Under_17",1])
  numQualifyingRelative <- as.numeric(statusDF["Qualifying_Relative",1])
  returnDF <- data.frame(c(0), row.names = c("Credit_Per_Qualifying_Child"))
  colnames(returnDF) <- taxYear
  returnDF["Credit_Per_Qualifying_Child",] <- numQualifyingChild * credit_per_child
  returnDF["Credit_Per_Other_Dependent",] <- numQualifyingRelative * credit_per_otherDep
  returnDF["Total_Above",] <- returnDF["Credit_Per_Qualifying_Child",] + returnDF["Credit_Per_Other_Dependent",]
  returnDF["AGI",] <- AGI
  returnDF["Line_5:SKIP",] <- 0
  returnDF["Line_6:AGI",] <- AGI + returnDF["Line_5:SKIP",]
  returnDF["Line_7:Phase_Out_Amount",] <- phase_out_agi
  returnDF["Line_8",] <- ifelse(returnDF["Line_6:AGI",]<returnDF["Line_7:Phase_Out_Amount",], 0, 
                               round_any(returnDF["Line_6:AGI",] - returnDF["Line_7:Phase_Out_Amount",], 1000, f= ceiling))
  returnDF["Line_9: 5% of Line 8",] <- round(returnDF["Line_8",] *0.05, digits = 4)
  returnDF["Line_10",] <- ifelse(returnDF["Line_9: 5% of Line 8",]>returnDF["Total_Above",],0,
                                 returnDF["Total_Above",] - returnDF["Line_9: 5% of Line 8",])  # this is the net credit after the 5% reduction
  returnDF["Line_11_Taxes",] <- taxes
  returnDF["Line_12:Other_Nonrefundable_Credits",] <- sumOtherCredits
  returnDF["Line_13_Net_Taxes",] <- returnDF["Line_11_Taxes",] - returnDF["Line_12:Other_Nonrefundable_Credits",] # This is the net tax after subtraction of other nonrefundable credit
  returnDF["Line_14:SKIP",] <- 0
  returnDF["Line_15:Net_Taxes",] <- returnDF["Line_13_Net_Taxes",]
  returnDF["Line_16:Child_Tax_Credit",] <- round(min(returnDF["Line_15:Net_Taxes",],returnDF["Line_10",]), digits = 2)
  if (returnDF["Line_10",]> returnDF["Line_15:Net_Taxes",]) {
    returnDF["Possible_Additional_CTC",1] <- 1 # May eligible for additional CTC
  } else {
    returnDF["Possible_Additional_CTC",1] <- 0 # Not eligible for additional CTC
  }
  return (returnDF) 
}
dependentCareCrd <- function (taxYear,AGI, taxes, filingStatus, incomeDF, creditDF ){
  #https://www.irs.gov/pub/irs-pdf/i2441.pdf
  # Rules: Qualifying child must be under 13 year old/ Or Disabled
  # Rules: Expense can't be more than $3000 for one child, or 6000 for 2 or more child

  LOWER_LIMIT <- 15000
  UPPER_LIMIT <- 43000
  MAX_RATE <- 0.35
  MIN_RATE <- 0.2
  INCREMENT <- 2000
  returnDF <- data.frame(creditDF["Qualifying_Person",], row.names = c("Number_Of_Qualifying_Person"))
  colnames(returnDF) <- taxYear
  # returnDF["Number_Of_Qualifying_Person",] <- creditDF["Qualifying_Person",]
  if (creditDF["Qualifying_Person",]==1){
    creditDF["Expense",] <- min(creditDF["Expense",],3000)
  } else {
    creditDF["Expense",] <- min(creditDF["Expense",],6000)
    creditDF["Qualifying_Person",] <- 2
  }
  returnDF["Maximum Expense",] <- creditDF["Expense",] # Step 2, this equal to line 2, part 2 of form 2441

  # Rules: Caclualte earning for FT Students or Disable Spouse
  # Rules: Earned $250/Child for each month being FT Student or Disabled
  # Rules: If both are ST or disabled, sum of month can't be more than 12
  # Assumption: If user has both earned income, and some months as FTStudent, add both together
  line_4 <- incomeDF[1] + incomeDF[5] # Line 4 equal wages of user in the vector at position 1 and 5
  if (creditDF["You_FT_Student",]==1){ # if FT student checkbox was checked, add additional income for months that were student
    line_4 <- line_4 + 250* creditDF["FT_Student_Month",]* creditDF["Qualifying_Person",]
  }
  line_5 <- line_4
  returnDF["Line 4:Your Earned Income", ] <- line_4
  returnDF["Line 5: Spouse Earned Income (MFJ)", ] <- line_5
  if (filingStatus == "MFJ"){
    line_5 <-  incomeDF[3] + incomeDF[7] # W-2 income of spouse
    if (creditDF["Spouse_FT_Student",]==1){
      line_5 <- line_5 + 250 * creditDF["Spouse_FT_Student_Month",]* creditDF["Qualifying_Person",]
    }
    returnDF["Line 5: Spouse Earned Income (MFJ)", ] <- line_5 # Change line 5 to spouse income
    if (creditDF["You_FT_Student",]== 1 & creditDF["Spouse_FT_Student",]==1){ # if both are full-time student, can allow additional income for the same month
      # I am making assumption that if the total of months being student are more than 12
      # I will deduct the excess out from income of the higher one.
      # Assuming that husband and wife are not FT student in the same month. Which allowed the max 12 months calculation.
      if ((creditDF["FT_Student_Month",] + creditDF["Spouse_FT_Student_Month",])>12){
        # this is the case when both are students and both sum of months is more than 12
        # Reduce income of the higher one, eithe line 4 or 5 by the extra months above 12
        reductionAmt <- 250*creditDF["Qualifying_Person",]*(creditDF["FT_Student_Month",] + creditDF["Spouse_FT_Student_Month",]-12)
        #  print (paste("Reduction Amount: ", reductionAmt))
        if (line_4> line_5){
          line_4 <- line_4 - reductionAmt
          # print (paste("Line_4 afte reduction:", line_4))
          returnDF["Line 4:Your Earned Income", ] <- line_4
        } else {
          line_5 <- line_5 - reductionAmt
          # print (paste("Line_5 after reduction:", line_5))
          returnDF["Line 5: Spouse Earned Income (MFJ)", ] <- line_5
        }
      } # else: no need to do anything
    }
  }

  line_6 <- min(c(line_4, line_5, creditDF["Expense",])) # getting the smallest
  returnDF["Line_6: Smallest of 3 lines above", ] <- line_6
  
  returnDF["Line_7_AGI", ] <- AGI
  # print (paste("AGI:",AGI))
  if (AGI<LOWER_LIMIT) rate <- MAX_RATE
  else if (AGI>UPPER_LIMIT) rate <- MIN_RATE
  else {
    rate <- MAX_RATE -(round_any((AGI - LOWER_LIMIT)/2000,1, f= ceiling)/100)
  }
  returnDF["Line 8: Rate", ] <- rate #Step 8
  
  line_9 <- round(line_6 * rate, digits = 2)
  returnDF["Line 9: Amount of credit before limitation", ] <- line_9
  line_10 <- taxes # Tax_Amount
  returnDF["Line 10: Tax amount before credit",] <- line_10
  returnDF["Child_Dependent_Care_Credit",] <- round(min(line_9, line_10), digits = 2)
  colnames(returnDF) <- taxYear
  return (returnDF)
}
educationalCrd <- function (taxYear, AGI, taxes, filingStatus, creditDF, otherCrdDF){
  
  # This function will a dataframe that contains Part1 & Part2 calculation of form 8863
  # Rule1: MFS cannot claim the credit
  UPPER_MAGI <- ifelse (filingStatus=="MFJ", 180000, 90000)
  DENOMINATOR <- ifelse (filingStatus=="MFJ", 20000, 10000)
  educationDF <- data.frame(c(0,0,0,0,0,0,0,0,0,0,
                              0,0,0,0,0,0,0,0,0),
                            row.names = c("Part 1: AOC Expense","Phase Out Amount","Line_3_MAGI", "Difference From Above",
                                          "Line_5:Denominator","Line_6:Percentage","Line_7: AOC Eligible Amount","Refundable_AOC (40%)", "Line_9_AOC_Nonrefundable_Amount",
                                          "Line_10: Lifetime learning expense", "Line_11:Smaller of Line 10 or 10000", "Line_12:20% of Line 11","Line_13:Phase-out Amount", 
                                          "Line_14:AGI","Line_15:Difference from above", "Line_16:Denominator","Line_17:Percentage","Line_18:LL Eligible Amount", "Line_19:Nonrefundable Education Credits"))
  colnames(educationDF) <- taxYear
  rowValue <- LLTbl[LLTbl$YEAR == taxYear & grepl(filingStatus, LLTbl$FILING_STATUS),]
  # print (paste("Row Values:", rowValue))
  line_30 <- 0 # Use for Part 3, calcualte AOC
  line_31 <- 0 # Use for part 3, calculate LL. Both amount will change if Expense >0
  if (filingStatus == "MFS") return (educationDF)
  if (creditDF["Expense_1",]>0){ # Complete part 3
    if ((creditDF["Claimed_AOC_4Yrs_1",] == 0) & (creditDF["Complete_Post_4Yrs_1",] == 0) & (creditDF["At_least_half-time_student_1",] ==1)){
      # Student1 met the AOC requirements
      maxExpense <- creditDF["Number_Student_1",]*4000 
      creditDF["Expense_1",] <- min(creditDF["Expense_1",], maxExpense)
      # print (paste("Max Expense:", creditDF["Expense_1",]))
      line_28 <- creditDF["Expense_1",] - 2000*creditDF["Number_Student_1",]
      # print (paste("Line 28 amount", line_28))
      line_28 <- ifelse(line_28>0, line_28, 0)
      line_29 <- line_28*0.25
      line_30 <- line_30 + ifelse (line_28==0, creditDF["Expense_1",], line_29 + 2000*creditDF["Number_Student_1",])
      # print (paste("Line 30:", line_30))
    }
    else {
      # Student 1 did not meet the AOC requirements, calculate LL credit for this student
      line_31 <- line_31 + creditDF["Expense_1",]
      # print (paste("Line 31:", line_31))
    }
  }
  if (creditDF["Expense_2",]>0){ # Complete part 3 of form 8863
  
    if ((creditDF["Claimed_AOC_4Yrs_2",] == 0) & (creditDF["Complete_Post_4Yrs_2",] == 0) & (creditDF["At_least_half-time_student_2",] ==1)){
      # Student2 met the AOC requirements
      maxExpense <- creditDF["Number_Student_2",]*4000 
      creditDF["Expense_2",] <- min(creditDF["Expense_2",], maxExpense)
      # print (paste("Max Expense:", creditDF["Expense_2",]))
      line_28 <- creditDF["Expense_2",] - 2000*creditDF["Number_Student_2",]
      # print (paste("Line 28 amount", line_28))
      line_28 <- ifelse(line_28>0, line_28, 0)
      line_29 <- line_28*0.25
      line_30 <- line_30 + ifelse (line_28==0, creditDF["Expense_2",], line_29 + 2000*creditDF["Number_Student_2",])
      # print (paste("Line 30:", line_30))
    } else {
      # Student 1 did not meet the AOC requirements, calculate LL credit for this student
      line_31 <- line_31 + creditDF["Expense_2",]
      # print (paste("Line 31:", line_31))
    }
  }
  # ------- Calculate refundable AOC - Part 1 of form 8863
  educationDF["Part 1: AOC Expense",] <- line_30
  educationDF["Phase Out Amount", ] <- UPPER_MAGI
  educationDF["Line_3_MAGI", ] <- AGI # Equal AGI
  educationDF["Difference From Above", ] <- educationDF["Phase Out Amount",] -educationDF["Line_3_MAGI",]
  educationDF["Difference From Above", ] <- max(educationDF["Difference From Above", ],0)
  educationDF["Line_5:Denominator", ] <- DENOMINATOR
  line_6 <- ifelse (educationDF["Difference From Above",] >= educationDF["Line_5:Denominator",], 1, round(educationDF["Difference From Above",]/educationDF["Line_5:Denominator",], digits = 2))
  educationDF["Line_6:Percentage", ] <- line_6
  educationDF["Line_7: AOC Eligible Amount", ] <- educationDF["Part 1: AOC Expense",] * educationDF["Line_6:Percentage", ]
  educationDF["Refundable_AOC (40%)",] <- round(educationDF["Line_7: AOC Eligible Amount", ] *0.4, digits = 2)
  # print (paste("Refundable AOC", educationDF["Refundable_AOC (40%)",]))
  #-------- Calculate nonrefundable - Part 2 of form 8863
  educationDF["Line_9_AOC_Nonrefundable_Amount",] <- educationDF["Line_7: AOC Eligible Amount", ] - educationDF["Refundable_AOC (40%)",]
  educationDF["Line_10: Lifetime learning expense", ] <- line_31
  educationDF["Line_11:Smaller of Line 10 or 10000", ] <- min(educationDF["Line_10: Lifetime learning expense",], 10000)
  educationDF["Line_12:20% of Line 11", ] <- educationDF["Line_11:Smaller of Line 10 or 10000",] * 0.2
  educationDF["Line_13:Phase-out Amount", ] <- rowValue$UPPER_MAGI
  educationDF["Line_14:AGI", ] <- AGI
  line_15 <- educationDF["Line_13:Phase-out Amount", ] - educationDF["Line_14:AGI", ]
  line_15 <- ifelse(line_15>0, line_15, 0)
  educationDF["Line_15:Difference from above", ] <- line_15
  educationDF["Line_16:Denominator", ] <- DENOMINATOR
  line_17 <- ifelse(line_15>=DENOMINATOR, 1, round(line_15/DENOMINATOR, digits = 2))
  # print(paste("Line 17:", line_17))
  educationDF["Line_17:Percentage", ] <- line_17
  educationDF["Line_18:LL Eligible Amount", ] <- educationDF["Line_12:20% of Line 11", ] * line_17 # This amount is LL Nonrefundable credit amount
  #----- Calculate Credit limit at showed on page 7 of form 8863
  nonRefundableEducationCrd <- educationDF["Line_9_AOC_Nonrefundable_Amount",] + educationDF["Line_18:LL Eligible Amount",]
  # print(paste("Nonrefundable Credit", nonRefundableEducationCrd))
  line_6 <- taxes - otherCrdDF # This is equivalent of Tax_Amount - CDC Credit
  # print (paste("Line 6 -Remaining Tax:", line_6))
  educationDF["Line_19:Nonrefundable Education Credits", ] <- round(min(nonRefundableEducationCrd, line_6), digits = 2)
  colnames(educationDF) <- taxYear
  return (educationDF)
}# End Educaiton Credit
saverCrd <- function(taxYear, filingStatus, AGI, taxes, earnedIncome, IRAContribution, 
                     retirementContribution,sumOtherCredits){
  # Form 8880: https://www.irs.gov/pub/irs-pdf/f8880.pdf
  # Parameters: 
  # filingStatus: vector with single value
  # earnedIncome: vector with single value
  # IRAContribution: vector with length of 2
  # retirementContribution: vector with length of 2
  if (earnedIncome< sum(IRAContribution)){
      # Limit amount of IRA Contribution to earnedIncome, allocated 1/2 to each if MFJ
    print ("Reduce IRA Contribution due to lower earnedIncome")
      if (filingStatus =="MFJ"){
        IRAContribution[1] <- earnedIncome/2
        IRAContribution[2] <- earnedIncome/2
      } else {
        IRAContribution[1] <- earnedIncome
      }
  }
  rowValues <- SaverTbl[SaverTbl$YEAR == taxYear & grepl(filingStatus, SaverTbl$FILING_STATUS),] # get applicable rows
  # print (rowValues)
  AGIRanges <- rowValues$UPPER_LIMIT
  returnDF <- data.frame(c(0), row.names = c("Line_1: Sum of Traditional and Roth IRA"))
  returnDF[1,] <- sum(IRAContribution)
  returnDF["Sum of Elective Deferrals",] <- sum(retirementContribution)
  returnDF["Total above",] <- sum (returnDF[c(1,2),])
  eligible1 <- sum(IRAContribution[1], retirementContribution[1])
  eligible2 <- sum(IRAContribution[2], retirementContribution[2])
  eligible1 <- min(eligible1, 2000)
  eligible2 <- min(eligible2, 2000)
  returnDF["Eligible Amount",]  <- sum(eligible1, eligible2)
  AGI <- AGI
  returnDF["AGI",] <- AGI
  # Determine applicable rate based on AGI in AGIRanges
  returnDF["Rate",] <- 0
  if (AGI<=AGIRanges[1]){
    returnDF["Rate",] <- 0.5
  } else if ((AGIRanges[1]< AGI) & (AGI<= AGIRanges[2])) {
    returnDF["Rate",] <- 0.2
  } else if ((AGIRanges[2]<AGI) & (AGI<=AGIRanges[3])){
    returnDF["Rate",] <- 0.1
  } 
 
  returnDF["Credit_Amount",] <- returnDF["Eligible Amount",] * returnDF["Rate",]
  returnDF["Limitation based on tax liability",1] <- taxes - sumOtherCredits
  returnDF["Saver_Credit",] <- round(min(returnDF["Credit_Amount",], returnDF["Limitation based on tax liability",1]), digits = 2)
  colnames(returnDF) <- taxYear
  # print(returnDF)
  return (returnDF)
}
elderlyDisableCrd <- function (){
  # Schedule R Instruction: https://www.irs.gov/pub/irs-pdf/i1040sr.pdf

}
additionalChildTaxCrd <- function (taxYear, CTCDF, statusDF, earnedIncome){
   # Form 8812 instructions: https://www.irs.gov/pub/irs-pdf/i1040s8.pdf
   # There was a change in 2014 in which each qualifying child may eligible for up to $1400 of ACTC
  returnDF <- data.frame(c(0), row.names = c("Line_1:CTC_Before_Taxes"))
  returnDF["Line_1:CTC_Before_Taxes",1] <- CTCDF["Line_10",1]
  returnDF["Line_2:CTC_After_Taxes",1] <- CTCDF["Line_16:Child_Tax_Credit",1]
  returnDF["Line_3:Net_Amount_Above",1] <- returnDF["Line_1:CTC_Before_Taxes",1] -returnDF["Line_2:CTC_After_Taxes",1]
  returnDF["Line_3b",1] <- returnDF["Line_3:Net_Amount_Above",1]
  subtractionAmt <- 3000
  if (taxYear == 2018){
    returnDF["ACTC_Amount_Per_Qualifying_Child",1] <- as.numeric(statusDF["Qualifying_Child_Under_17",1]) *1400
    returnDF["Line_3b",1] <- min(returnDF["ACTC_Amount_Per_Qualifying_Child",1],returnDF["Line_3b",1])
    subtractionAmt <- 2500
  }
  returnDF["Line_4:Earned_Income",1] <- earnedIncome
  returnDF["Line_5:Subtraction_From_Earned_Income",1] <- subtractionAmt
  returnDF["Line_6:Net_Earned_Income",1] <- max(returnDF["Line_4:Earned_Income",1]-subtractionAmt,0)
  returnDF["Line_7:15% of Line_6 Above",1] <- returnDF["Line_6:Net_Earned_Income",1] *0.15
  returnDF["ACTC_Amount",1] <- round(min(returnDF["Line_7:15% of Line_6 Above",1],returnDF["Line_3b",1]), digits = 2)
  colnames(returnDF) <- taxYear
  return (returnDF)
}
EIC <- function(taxYear, earnedIncome, AGI, statusDF ){
  # Restrictions of claiming the EIC credit for this function
  # 1. Can't have MFS status
  # 2. Age: between 25 and 65
  # 3. Investment Income less than $3500(2018) or $3450 for 2017
  # 3b. Investment Income defined: Interest and Dividends, Capital Gains but not capital loss
  # 3b. Investment income did not include: Royalties, Rental, Passive Activities income
  numQualChild <- sum(as.numeric(statusDF[c("Qualifying_Child_Under_17","Qualifying_Child_Over_17"),1]))
  filingStatus <- toupper(statusDF["Filing_Status",1])
  numQualChild <- min(numQualChild,3)
  ages <- as.numeric(statusDF[c("Your_Age"),1])
  
  if (filingStatus =="MFJ"){
    ages <- as.numeric(statusDF[c("Your_Age","Spouse_Age"),1])
  }
  rowValues <- EICTbl[EICTbl$YEAR == taxYear & grepl(filingStatus, EICTbl$FILING_STATUS) & 
                        EICTbl$NUM_QUALIFYING_CHILD == numQualChild,] 
  #print (rowValues)
  MAX_EIC <- round(rowValues$EI_RANGE_1 * rowValues$RATE, digits = 2)
  returnDF <- data.frame(c(0,0,0,0,0,0), row.names = c("Num_Qualifying_Child","Earned_Income","EIC_Credit","AGI","EIC_Credit_Limit","EIC_Amount"))
  returnDF["Num_Qualifying_Child",1] <- numQualChild
  # limitation: This function will calculate EIC credit using Worksheet A of form 1040.
  # Step 1 - Determine if user is qualified for EIC
  if (numQualChild == 0 & (any(ages<25) | any(ages>65))){ # Check step 4 of EIC instruction on form 1040
    return (returnDF)
  }
  if (earnedIncome>=rowValues$MAX_OUT_AGI){
    return (returnDF)
  }
  returnDF["Earned_Income",1] <- earnedIncome
  # Step 2 - Determine EIC
  if (earnedIncome<=rowValues$EI_RANGE_1) {
    returnDF["EIC_Credit",1] <- round(earnedIncome* rowValues$RATE, digits = 2)
  } 
  else if ((earnedIncome >rowValues$EI_RANGE_1) & (earnedIncome <=rowValues$EI_RANGE_2)){ # Max EIC
    returnDF["EIC_Credit",1] <- MAX_EIC
  } else { # Reduce EIC
    returnDF["EIC_Credit",1] <- round(MAX_EIC - ((earnedIncome-rowValues$EI_RANGE_2)*rowValues$REDUCE_RATE), digits = 2)
  }
  returnDF["AGI",1] <- AGI
  # line 4 on Worksheet, compare AGI with earnedIncome
  if ((AGI == earnedIncome) | (AGI<rowValues$AGI_LIMIT)){ #Skip Part 2 line 5
    returnDF["EIC_Credit_Limit",1] <- returnDF["EIC_Credit",1]
  } else { # Calculate EIC based on AGI income
    #----------------------------------------------------------------------------
    if (AGI<=rowValues$EI_RANGE_1) {
      returnDF["EIC_Credit_Limit",1] <- round(AGI* rowValues$RATE, digits = 2)
    } 
    else if ((AGI >rowValues$EI_RANGE_1) & (AGI <=rowValues$EI_RANGE_2)){ # Max EIC
      returnDF["EIC_Credit_Limit",1] <- MAX_EIC
    } else { # Reduce EIC
      returnDF["EIC_Credit_Limit",1] <- MAX_EIC - ((AGI-rowValues$EI_RANGE_2)*rowValues$REDUCE_RATE)
    }
  }
  returnDF["EIC_Amount",1] <- round(min(returnDF["EIC_Credit",1],returnDF["EIC_Credit_Limit",1]), digit=2)
  colnames(returnDF) <- taxYear
  return (returnDF)
}


