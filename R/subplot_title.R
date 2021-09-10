#' Function to add a subplot title to a plotly subplot
#'
#' @param plot plotly subplot
#' @param title title of subplot
#' @param .x Numeric position of subplot title relative to x-axis
#' @param .y Numeric position of subplot title relative to y-axis
#'
#' @return plotly with subtitle
#' @export
#'
#' @examples
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
