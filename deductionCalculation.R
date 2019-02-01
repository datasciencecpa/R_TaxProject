# This module was used to calculate above the line deductions, and figure out
# amount of standard deduction or itemized deduction, which ever is larger.

# Author: Long Nguyen
# Date Created: 01/28/2019

# HSA Deductions, see form 8889 Instruction
# https://www.irs.gov/pub/irs-pdf/i8889.pdf

library (gdata)
hsaTbl <- read.xls(xls="TaxRates.xls", sheet= 10)
HSADeduction <- function (ages, contributionAmt, hsaPlan, taxYear) {
  # In order to calculate HSA deduction, this function will need the following information.
  # * Ages - this will be the ages of individual and/or spouses if both contribute to HSA either as Single or Family.
  # * Individual with ages >55 can eligible for HSA catch-up contribution of $1000. Assuming that they are eligible for entire year
  # * contributionAmt - This is a vector contains individual contribution to HSA (outsite of employee contribution), and employer contribution
  # * hsaPlan - which has either Single/Family value
  # * taxYear
  maxContribution <- as.numeric(hsaTbl[hsaTbl$YEAR == as.character(taxYear) & hsaTbl$TYPE == hsaPlan])
  print (maxContribution)
  maxContribution <- maxContribution + 1000 * sum(ages >=55)
  employeeContribution <- contributionAmt[1]
  employerContribution <- contributionAmt[2]
  eligibleAmount <- maxContribution - employerContribution # this is the maximum amount of additional HSA contribution employee can contribute
  return (ifelse (eligibleAmount>employeeContribution, employeeContribution, eligibleAmount))
}