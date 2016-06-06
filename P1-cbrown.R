#' Valence item analysis
#'
#' Prepares raw valence rating data for classical item analysis (istudy).
#'
#' @param x A valence data frame where the first row contains item IDs and the
#' first column contains subject IDs.
#' @param posval The original value assigned to code output as a positive response.
#' @param negval The original value assigned to code output as a negative response.
#' @param valence data frame with two columns, the first containing stimuli
#' names (item), the second containing valence information (val).
#' @examples
#' valitem(x, 1, 2)
#' @export

# Get example data to test function
x <- read.csv("trworddata1p2n.csv")
valence <- read.csv("valdata.csv")

ready <- function(x, posval, negval, na.rm = TRUE) {
  x[x == posval] <- 0
  x[x == negval] <- 1
  rownames(x) <- x[,1]
  return(x)
}

valitem <- function(x, posval, negval, na.rm = TRUE) {
  mydata <- ready(x, posval, negval)
  as.vector(valence$val)
  attr(mydata[-1,], "names") <- valence$val
  positems <- epmr::istudy(mydata[, valence$val == "POS"])
  print(positems)

}
