instructionUI <- function (id){

  tabPanel("Instructions:",
           p("Please refer to this ", a(href="https://adatascientist-cpa.com/wp-content/uploads/2019/03/R_TaxProject.pptx","User Guide"), " for instructions of how to use this app."),
           p("Please start by selecting the ", strong('Filing Status & Dependency')," Tab on the right panel"),
           tags$img(src = "TaxGraph.png",height = "600", width = "900")
          )
}
