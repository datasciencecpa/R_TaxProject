hideshow <- function (IDs, hide = true){
  # This function will go through a list of IDs. If hide == TRUE, hide. Otherwise, show element in IDs
  for (id in IDs){
    if (hide) {
      hide(id)
    } else {
      show (id)
    }
  }
}
updateNumInput <- function(session, IDs, values, labels = list()) {
  # This helper function is use to help with updateNumericInput where there are 3 or more
  # updates needed. Reduce code on module.
  for (i in 1: length(IDs)){
    updateNumericInput(session, IDs[i], value = values[i], label = labels[i])
  }
}