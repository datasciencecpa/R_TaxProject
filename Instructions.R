instructionUI <- function (id){

  tabPanel("Instructions for how to use this app:",
           p("The goal of this app is to allow user with simple tax return (W-2 income, interest, dividends, capital gains/loss from stocks, simple itemized 
             deductions to enter these information for both 2018 & 2017 and see the compare result."),
           p("This app will also allow user to check box such as:max_out IRA, or change filing status, or simply just play with different numbers to see the tax effect."),
           p("Since I am using R to build this app, it can't go without graph! User can visualize amount of tax, credits, deductions, etc compared by year."),
           p("Hopefully, I will be able to add more features as time allows."),
           p("This app was born on Jan 19, 2019."),
           p("I will be back with real instructions."))
}
