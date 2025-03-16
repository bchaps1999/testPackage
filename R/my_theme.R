#' Apply a custom theme to a ggplot object
#'
#' This function applies a minimal theme to a ggplot object and customizes the
#' plot title and axis titles.
#'
#' @name my_theme
#' @import ggplot2
#' @param plot A ggplot object to which the theme will be applied.
#'
#' @return A ggplot object with the custom theme applied.
#' @export
#'
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#' my_theme(p)
library(ggplot2)
my_theme <- function(plot) {
    plot +
        theme_minimal() +
        theme(
            plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold")
        )
}