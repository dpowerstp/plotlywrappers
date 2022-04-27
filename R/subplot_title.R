#' Function to add a subplot title to a plotly subplot
#'
#'
#' @param plot plotly subplot
#' @param title title of subplot
#' @param .x Numeric position of subplot title relative to x-axis
#' @param .y Numeric position of subplot title relative to y-axis
#'
#' @return plotly with subtitle
#' @export
#'
#' @examples{
#'
#' quickplot <- plotly::plot_ly(mtcars,
#' x = ~ carb,
#' y = ~ disp,
#' type = "bar")
#'
#' subplot_title(quickplot, "Cars subplot")
#'
#' }
subplot_title <- function(plot, title, .x = 0.1, .y = 1){

  plot %>%
    plotly::add_annotations(text = paste0("<i><b>", title, "</i></b>"),
                            x = .x,
                            y =.y,
                            yref = "paper",
                            xref = "paper",
                            xanchor = "left",
                            yanchor = "top",
                            showarrow = FALSE,
                            font = list(size = 12))
}




#' Remove Plot and Axes Titles
#'
#' Set title of plotly plot to nothing, and title of axes to nothing. Useful if creating subplots and want to set layout at end.
#'
#' @param plot Plotly plot
#'
#' @return Plotly plot with no labels.
#' @export
#'
#' @examples{
#'
#' quickplot <- plotly::plot_ly(mtcars,
#' x = ~ carb,
#' y = ~ disp,
#' type = "bar")
#'
#' no_titleaxeslabels(quickplot)
#'
#' }
no_titleaxeslabels <- function(plot){
  plot %>%
    plotly::layout(xaxis = list(title = ""),
           yaxis = list(title = ""),
           title = "")
}
