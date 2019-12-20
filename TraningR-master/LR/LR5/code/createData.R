#' Add together 6 params.
#'
#' @param n1 - number.
#' @param mn - number.
#' @param s - number.
#' @param number - number.
#' @param numberMin - number.
#' @param numberMax - number.
#' @param x - number.
#' @param y - number.
#' @param fr - number.
#' @param birth - list of value double.
#' 
#' @return The list of values \code{example1,exsamle2,exsamle3}.
#' 
#' @examples
#' createData(111, 11, 1, 11, 1, 11, 11, 1111, 1,
#'  scan("http://robjhyndman.com/tsdldata/data/nybirths.dat"))
#' createData(222, 22, 2, 22, 2, 22, 22, 2222, 2,
#'  scan("http://robjhyndman.com/tsdldata/data/nybirths.dat"))
#' 
#' @export

createData <- function(n1, mn, s, number, numberMin, numberMax, x, y, fr, birth)
  {
    exsamle1 <- rnorm(n = n1, mean = mn, sd = s)

    exsamle2 <- runif(number, numberMin, numberMax)

    exsamle3 <- ts(birth, start = c(x, y), frequency = fr)
    
    myList <- list(exsamle1, exsamle2, exsamle3)
    
    return(myList)
}

# createData(111, 11, 1, 11, 11, 11, 11, 1111, 1, scan("http://robjhyndman.com/tsdldata/data/nybirths.dat"))
