#' Dotted line
#' Adds dotted line with label to plot_ly plot
#'
#' @param plot plot_ly plot
#' @param df Dataframe used to build plot_ly plot
#' @param x_col String column name of x-axis
#' @param line_val Value on y-axis at which to draw dotted line
#' @param gluetext Text fed to glue::glue function used to label dotted line
#' @param .yshift How far to shift label relative to y-axis
#' @param .xshift How far to shift label relative to x-axis
#'
#' @return plot_ly plot with dotted line and label on dotted line
#' @export
#'
#' @examples
dotted_line <- function(plot, df, x_col, line_val, gluetext, .yshift = 5, .xshift = 2){
  x_length <- length(df[[x_col]] %>%
           unique())

  plot %>%
    plotly::layout(
      shapes = list(
        list(
          type = "line",
          y0 = line_val,
          y1 = line_val,
          x0 = 0,
          x1 = x_length,
          xref = "paper",
          line = list(dash = "dash")
        )
      )
    ) %>%
    plotly::add_annotations(
      text = glue::glue(gluetext),
      x = x_length - x_length/2,
      yshift = .yshift,
      xshift = .xshift,
      y = line_val,
      showarrow = TRUE)
}
