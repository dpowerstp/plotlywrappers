#' Dotted line
#' Adds dotted line with label to plot_ly plot
#'
#' @importFrom magrittr %>%
#'
#' @param plot plot_ly plot
#' @param df Dataframe used to build plot_ly plot
#' @param x_col String column name of x-axis
#' @param line_val Value on y-axis at which to draw dotted line
#' @param gluetext Text fed to glue::glue function used to label dotted line
#' @param .yshift How far to shift label relative to y-axis; default 5
#' @param .xshift How far to shift label relative to x-axis; default 2
#' @param .showarrow Whether to show the arrow, T or F. Default true
#' @param .lineadjust How much to adjust the position of the line along the x-axis. Default 0.5
#'
#' @return plot_ly plot with dotted line and label on dotted line
#' @export
#'
#' @examples{
#'
#' quickplot <- plotly::plot_ly(mtcars,
#' x = ~ carb,
#' y = ~ disp,
#' type = "bar")
#'
#' dotted_line(quickplot, mtcars, "carb", 200, "200 line")
#'
#' }
dotted_line <- function(plot, df, x_col, line_val, gluetext, .yshift = 5, .xshift = 2, .showarrow = T, .lineadjust = 0.5){
  x_length <- length(df[[x_col]] %>%
                       unique())

  # browser()

  plot %>%
    plotly::layout(
      shapes = list(
        list(
          type = "line",
          y0 = line_val,
          y1 = line_val,
          x0 = 0 - .lineadjust,
          x1 = x_length - .lineadjust,
          xref = "x",
          line = list(dash = "dash")
        )
      )
    ) %>%
    plotly::add_annotations(
      text = glue::glue(gluetext),
      x = x_length - x_length / 2,
      yshift = .yshift,
      xshift = .xshift,
      y = line_val,
      showarrow = .showarrow)
}




#' Multiple Dotted Lines
#'
#' Create multiple dotted lines in a plot_ly graph
#'
#' @param plot Base-plotly chart
#' @param df Base-dataframe
#' @param x_col String name of x-axis column
#' @param yvec Numeric vector of y-values at which to draw dotted lines
#' @param textvec Character vector of labels for dotted lines; fed to glue function
#' @param .lineadjust How much to adjust the length of the dotted line relative to the x-axis; default 0.5
#' @param .showarrow Whether to show an arrow pointing to the dotted line or not; default T
#' @param .yshift How much to adjust the y-axis position of the text label relative to the dotted line; default 5
#' @param .xshift How much to adjust the x-axis position of the text label relative to the dotted line; default 2
#'
#' @return
#' @export
#'
#' @examples{
#'
#' quickplot <- plotly::plot_ly(mtcars,
#' x = ~ carb,
#' y = ~ disp,
#' type = "bar")
#'
#' multi_line(quickplot, mtcars, "carb", c(200, 250), c("200 line", "250 line"))
#'
#' }
multi_line <- function(plot, df, x_col, yvec, textvec, .lineadjust = 0.5, .showarrow =T, .yshift = 5, .xshift = 2){

  if (length(yvec) != length(textvec)){
    stop("Length of yvec must equal length of textvec")
  }

  x_length <- length(df[[x_col]] %>% unique())

  linelist <- purrr::map(yvec, ~{
    list(type = "line",
         y0 = .x, y1 = .x, x0 = 0 - .lineadjust, x1 = x_length -
           .lineadjust, xref = "x", line = list(dash = "dash"))
  })

  purrr::walk2(textvec, yvec, ~ {

    plot <<- plot %>%
      plotly::add_annotations(text = glue::glue(.x),
                              x = x_length - x_length/2, yshift = .yshift, xshift = .xshift,
                              y = .y, showarrow = .showarrow)
  })

  plot %>%
    plotly::layout("shapes" = linelist)

}
