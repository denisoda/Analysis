#' Add together 6 params.
#' 
#' @param col1 - color.
#' @param size1 - number.
#' @param data2 - table.
#' @param size2 - number.
#' @param size3 - number.
#' @param col3 - color.
#' 
#' @return The list of graphs \code{example1, example2, example3}.
#' 
#' @examples
#' buildPlot("blue", 3, mpg, 4, 6, "white")
#' buildPlot("red", 2, mpg, 6, 3, "green")
#' 
#' @export

buildPlot <- function(col1, size1, ata2, size2, size3, col3)
  {
  example1 <- ggplot(data = mpg) +
    geom_point(mapping = aes(x = displ, y = hwy,
    shape = factor(cyl)),size = size1, colour = col1)

  example2 <- ggplot(data = data2, mapping = aes(x = displ, y = hwy)) +
    geom_point(mapping = aes(color = drv), size = size2) +
    geom_smooth(se = FALSE)

  example3 <- ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) +
    geom_point(size = size3,colour = col1) +
    geom_point(mapping = aes(color = drv), size = 2)
}

# buildPlot("blue", 3, mpg, 4, 6, "white")
