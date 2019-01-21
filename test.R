fluidRow(
  column(5, 
         numericInput(inputId = "yourAge_2018", label = "Enter your age:",
                      min= 0, max=200, value = 30),
         numericInput(inputId = "spouseAge_2018", label = "Enter your spouse age:",
                      min= 0, max=200, value = 30),
         numericInput(inputId = "child1Age_2018", label = "Enter your child 1 age:",
                      min = 0, max=200, value = 10),
         
         numericInput(inputId = "child2Age_2018", label = "Enter your child 2 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "child3Age_2018", label = "Enter your child 3 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "child4Age_2018", label = "Enter your child 4 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "child5Age_2018", label = "Enter your child 5 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "qualifiedRel1_2018", label = "Enter your qualifying relative 1 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "qualifiedRel2_2018", label = "Enter your qualifying relative 2 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "qualifiedRel3_2018", label = "Enter your qualifying relative 3 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "qualifiedRel4_2018", label = "Enter your qualifying relative 4 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "qualifiedRel5_2018", label = "Enter your qualifying relative 5 age:",
                      min = 0, max=200, value = 10)
  ), 
  column(5, 
         numericInput(inputId = "yourAge_2017", label = "Enter your age:",
                      min= 0, max=200, value = 30),
         numericInput(inputId = "spouseAge_2017", label = "Enter your spouse age:",
                      min= 0, max=200, value = 30),
         numericInput(inputId = "child1Age_2017", label = "Enter your child 1 age:",
                      min = 0, max=200, value = 10),
         
         numericInput(inputId = "child2Age_2017", label = "Enter your child 2 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "child3Age_2017", label = "Enter your child 3 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "child4Age_2017", label = "Enter your child 4 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "child5Age_2017", label = "Enter your child 5 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "qualifiedRel1_2017", label = "Enter your qualifying relative 1 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "qualifiedRel2_2017", label = "Enter your qualifying relative 2 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "qualifiedRel3_2017", label = "Enter your qualifying relative 3 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "qualifiedRel4_2017", label = "Enter your qualifying relative 4 age:",
                      min = 0, max=200, value = 10),
         numericInput(inputId = "qualifiedRel5_2017", label = "Enter your qualifying relative 5 age:",
                      min = 0, max=200, value = 10)
  ) 
)

if (input$filingStatus_2018 == "Married Filing Jointly"){
  show(id = "spouseAge_2018")
} else hide(id = "spouseAge_2018")
if (input$filingStatus_2017 == "Married Filing Jointly"){
  show(id = "spouseAge_2017")
} else hide(id = "spouseAge_2017")