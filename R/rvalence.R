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
#' # Get example data to test function
#' x <- read.csv("trworddata1p2n.csv", row.names = 1)
#' valence <- read.csv("valdata.csv")
#' valitem(x, 1, 2, valence)
#' 
#' @export
valitem <- function(x, posval, negval, valence, na.rm = TRUE) {
  mydata <- ready(x, posval, negval, valence, na.rm = na.rm)
  valence <- attributes(mydata)$valence
  out <- tapply(paste(valence$item), valence$val, function(i)
    epmr::istudy(mydata[, i]))
  return(out)
}

#' @rdname valitem
ready <- function(x, posval, negval, valence, na.rm = TRUE) {
  x[x == posval] <- 0
  x[x == negval] <- 1
  attr(x, "valence") <- valence
  return(x)
}