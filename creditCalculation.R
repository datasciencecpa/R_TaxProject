# Credit calculation module
# Author: Long Nguyen
# Date Created: 01/26/2019

## This module will use to calcualte credits.
library(gdata)
library(plyr)
LLTbl <- read.xls("TaxRates.xls", sheet = 8)
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

dependentCareCrd <- function (taxYear,summaryDF, filingStatus, incomeDF, creditDF ){
  #https://www.irs.gov/pub/irs-pdf/i2441.pdf
  # Rules: Qualifying child must be under 13 year old/ Or Disabled
  # Rules: Expense can't be more than $3000 for one child, or 6000 for 2 or more child
  LOWER_LIMIT <- 15000
  UPPER_LIMIT <- 43000
  MAX_RATE <- 0.35
  MIN_RATE <- 0.2
  INCREMENT <- 2000
  returnDF <- data.frame(Number_Of_Qualifying_Person =creditDF["Qualifying_Person"])
  colnames(returnDF) <- taxYear
  # returnDF["Number_Of_Qualifying_Person",] <- creditDF["Qualifying_Person"]
  if (creditDF["Qualifying_Person"]==1){
    creditDF["Expense"] <- ifelse(creditDF["Expense"]>3000, 3000, creditDF["Expense"])
  } else {
    creditDF["Expense"] <- ifelse(creditDF["Expense"]>6000, 6000, creditDF["Expense"])
    creditDF["Qualifying_Person"] <- 2
  }
  returnDF["Max_Expense",] <- creditDF["Expense"] # Step 2, this equal to line 2, part 2 of form 2441

  # Rules: Caclualte earning for FT Students or Disable Spouse
  # Rules: Earned $250/Child for each month being FT Student or Disabled
  # Rules: If both are ST or disabled, sum of month can't be more than 12
  # Assumption: If user has both earned income, and some months as FTStudent, add both together
  line_4 <- incomeDF[1] + incomeDF[5] # Line 4 equal wages of user in the vector at position 1 and 5
  if (creditDF["You_FT_Student"]==1){ # if FT student checkbox was checked, add additional income for months that were student
    line_4 <- line_4 + 250* creditDF["FT_Student_Month"]* creditDF["Qualifying_Person"]
  }
  line_5 <- line_4
  returnDF["Line_4", ] <- line_4
  returnDF["Line_5", ] <- line_5
  if (filingStatus == "MFJ"){
    line_5 <-  incomeDF[3] + incomeDF[7] # W-2 income of spouse
    if (creditDF["Spouse_FT_Student"]==1){
      line_5 <- line_5 + 250 * creditDF["Spouse_FT_Student_Month"]* creditDF["Qualifying_Person"]
    }
    returnDF["Line_5", ] <- line_5 # Change line 5 to spouse income
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
          returnDF["Line_4", ] <- line_4
        } else {
          line_5 <- line_5 - reductionAmt
          # print (paste("Line_5 after reduction:", line_5))
          returnDF["Line_5", ] <- line_5
        }
      } # else: no need to do anything
    }
  }

  line_6 <- min(c(line_4, line_5, creditDF["Expense"])) # getting the smallest
  returnDF["Line_6_Smallest_Of_3_4_5_Above", ] <- line_6
  AGI <- summaryDF[1] # AGI
  returnDF["Line_7_AGI", ] <- AGI
  # print (paste("AGI:",AGI))
  if (AGI<LOWER_LIMIT) rate <- MAX_RATE
  else if (AGI>UPPER_LIMIT) rate <- MIN_RATE
  else {
    rate <- MAX_RATE -(round_any((AGI - LOWER_LIMIT)/2000,1, f= ceiling)/100)
  }
  returnDF["Rate", ] <- rate #Step 8
  
  line_9 <- round(line_6 * rate, digits = 4)
  returnDF["Line_9", ] <- line_9
  line_10 <- summaryDF[5] # Tax_Amount
  returnDF["Line_10",] <- line_10
  returnDF["Child_Dependent_Care_Credit",] <- min(line_9, line_10)
  return (returnDF)
}

educationalCrd <- function (taxYear, summaryDF, filingStatus, creditDF, otherCrdDF){
  
  # This function will a dataframe that contains Part1 & Part2 calculation of form 8863
  # Rule1: MFS cannot claim the credit
  UPPER_MAGI <- ifelse (filingStatus=="MFJ", 180000, 90000)
  DENOMINATOR <- ifelse (filingStatus=="MFJ", 20000, 10000)
  educationDF <- data.frame(c(0,0,0,0,0,0,0,0,0,0,
                              0,0,0,0,0,0,0,0,0),
                            row.names = c("Part_1_Line_1","Line_2","Line_3_MAGI", "Line_4",
                                          "Line_5","Line_6","Line_7","Refundable_AOC", "Line_9",
                                          "Line_10", "Line_11", "Line_12","Line_13", "Line_14",
                                          "Line_15", "Line_16","Line_17","Line_18", "Line_19"))
  colnames(educationDF) <- taxYear
  rowValue <- LLTbl[LLTbl$YEAR == taxYear & grepl(filingStatus, LLTbl$FILING_STATUS),]
  print (paste("Row Values:", rowValue))
  line_30 <- 0 # Use for Part 3, calcualte AOC
  line_31 <- 0 # Use for part 3, calculate LL. Both amount will change if Expense >0
  if (filingStatus == "MFS") return (educationDF)
  if (creditDF["Expense_1"]>0){ # Complete part 3
    if ((creditDF["Claimed_AOC_4Yrs_1"] == 0) & (creditDF["Complete_Post_4Yrs_1"] == 0) & (creditDF["At_least_half-time_student_1"] ==1)){
      # Student1 met the AOC requirements
      maxExpense <- creditDF["Number_Student_1"]*4000 
      creditDF["Expense_1"] <- min(creditDF["Expense_1"], maxExpense)
      # print (paste("Max Expense:", creditDF["Expense_1"]))
      line_28 <- creditDF["Expense_1"] - 2000*creditDF["Number_Student_1"]
      print (paste("Line 28 amount", line_28))
      line_28 <- ifelse(line_28>0, line_28, 0)
      line_29 <- line_28*0.25
      line_30 <- line_30 + ifelse (line_28==0, creditDF["Expense_1"], line_29 + 2000*creditDF["Number_Student_1"])
      # print (paste("Line 30:", line_30))
    }
    else {
      # Student 1 did not meet the AOC requirements, calculate LL credit for this student
      line_31 <- line_31 + creditDF["Expense_1"]
      # print (paste("Line 31:", line_31))
    }
  }
  if (creditDF["Expense_2"]>0){ # Complete part 3 of form 8863
  
    if ((creditDF["Claimed_AOC_4Yrs_2"] == 0) & (creditDF["Complete_Post_4Yrs_2"] == 0) & (creditDF["At_least_half-time_student_2"] ==1)){
      # Student2 met the AOC requirements
      maxExpense <- creditDF["Number_Student_2"]*4000 
      creditDF["Expense_2"] <- min(creditDF["Expense_2"], maxExpense)
      # print (paste("Max Expense:", creditDF["Expense_2"]))
      line_28 <- creditDF["Expense_2"] - 2000*creditDF["Number_Student_2"]
      print (paste("Line 28 amount", line_28))
      line_28 <- ifelse(line_28>0, line_28, 0)
      line_29 <- line_28*0.25
      line_30 <- line_30 + ifelse (line_28==0, creditDF["Expense_2"], line_29 + 2000*creditDF["Number_Student_2"])
      # print (paste("Line 30:", line_30))
    } else {
      # Student 1 did not meet the AOC requirements, calculate LL credit for this student
      line_31 <- line_31 + creditDF["Expense_2"]
      # print (paste("Line 31:", line_31))
    }
  }
  # ------- Calculate refundable AOC - Part 1 of form 8863
  educationDF["Part_1_Line_1",] <- line_30
  educationDF["Line_2", ] <- UPPER_MAGI
  educationDF["Line_3_MAGI", ] <- summaryDF[1] # Equal AGI
  educationDF["Line_4", ] <- educationDF["Line_2",] -educationDF["Line_3_MAGI",]
  educationDF["Line_4", ] <- ifelse(educationDF["Line_4", ]>=0, educationDF["Line_4", ], 0)
  educationDF["Line_5", ] <- DENOMINATOR
  line_6 <- ifelse (educationDF["Line_4",] >= educationDF["Line_5",], 1, round(educationDF["Line_4",]/educationDF["Line_5",], digits = 3))
  educationDF["Line_6", ] <- line_6
  educationDF["Line_7", ] <- educationDF["Part_1_Line_1",] * educationDF["Line_6", ]
  educationDF["Refundable_AOC",] <- educationDF["Line_7", ] *0.4
  # print (paste("Refundable AOC", educationDF["Refundable_AOC",]))
  #-------- Calculate nonrefundable - Part 2 of form 8863
  educationDF["Line_9",] <- educationDF["Line_7", ] - educationDF["Refundable_AOC",]
  educationDF["Line_10", ] <- line_31
  educationDF["Line_11", ] <- min(educationDF["Line_10",], 10000)
  educationDF["Line_12", ] <- educationDF["Line_11",] * 0.2
  educationDF["Line_13", ] <- rowValue$UPPER_MAGI
  educationDF["Line_14", ] <- summaryDF[1]
  line_15 <- educationDF["Line_13", ] - educationDF["Line_14", ]
  line_15 <- ifelse(line_15>0, line_15, 0)
  educationDF["Line_15", ] <- line_15
  educationDF["Line_16", ] <- DENOMINATOR
  line_17 <- ifelse(line_15>=DENOMINATOR, 1, round(line_15/DENOMINATOR, digits = 3))
  # print(paste("Line 17:", line_17))
  educationDF["Line_17", ] <- line_17
  educationDF["Line_18", ] <- educationDF["Line_12", ] * line_17 # This amount is LL Nonrefundable credit amount
  #----- Calculate Credit limit at showed on page 7 of form 8863
  nonRefundableEducationCrd <- educationDF["Line_9",] + educationDF["Line_18",]
  line_6 <- summaryDF[5] - otherCrdDF # This is equivalent of Tax_Amount - CDC Credit
  # print (paste("Line 6 -Remaining Tax:", line_6))
  educationDF["Line_19", ] <- min(nonRefundableEducationCrd, line_6)
  # print ("Final educationDF:")
  # print(educationDF)
  return (educationDF)
}# End Educaiton Credit

saverCrd <- function(){
  
}

elderlyDisableCrd <- function (){
  # Schedule R Instruction: https://www.irs.gov/pub/irs-pdf/i1040sr.pdf
  
}
additionalChildTaxCrd <- function (){
   # Form 8812
}



