myFunc <- function (){
  print("Hello Long")
  function () {
    print ("Print inside function")
  }
}
myFunc()()

x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)b()


