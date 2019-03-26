library(reshape2)
#' gainslift gainschart Function
#'
#'
#' @param gainsdata A gainsdata object.
#' see \code{gainsdata}.
#'
#' @param title a text label for the main title.
#'
#' @param xlabel a text label for the x axis.
#'
#' @param ylabel a text label for the y axis.
#'
#'
#' @usage
#' gainschart(gainsdata, title = NULL, xlabel = NULL, ylabel = NULL)
#'
#' @export
gainschart <- function(gainsliftTable, title = NULL, xlabel = NULL, ylabel = NULL){
  chart_type <- gainsliftTable$table_type
  data <- gainsliftTable$data

  # Plot charts
  switch(chart_type,
    'cum' = {
      plot_obj <- gains_chart(data, title, xlabel, ylabel)
    },
    'profit' = {
      plot_obj <- profit_gains_chart(data, title, xlabel, ylabel)
    },
    'decile' = {
      plot_obj <- decile_gains_chart(data, title, xlabel, ylabel)
    },
    'profit_decile' = {
      plot_obj <- decile_profit_gains_chart(data, title, xlabel, ylabel)
    },
    'noncum' = {
      plot_obj <- noncum_decile_gains_chart(data, title, xlabel, ylabel)
    },
    'profit_noncum' = {
      plot_obj <- noncum_decile_profit_gains_chart(data, title, xlabel, ylabel)
    }
  )
  # return object
  obj <- list(data = data,
              table_type = chart_type,
              plot_object = plot_obj
              )
  class(obj) <- "gainschart"
  return(invisible(obj))
}

#' gainslift liftchart Function
#'
#'
#' @param liftdata A liftdata object.
#' see \code{liftdata}.
#'
#' @param title a text label for the main title.
#'
#' @param xlabel a text label for the x axis.
#'
#' @param ylabel a text label for the y axis.
#'
#'
#' @usage
#' liftchart(liftdata, title = NULL, xlabel = NULL, ylabel = NULL)
#'
#' @export
liftchart <- function(gainsliftTable, title = NULL, xlabel = NULL, ylabel = NULL){
  chart_type <- gainsliftTable$table_type
  data <- gainsliftTable$data

  # Plot charts
  switch(chart_type,
   'cum' = {
     plot_obj <- lift_chart(data, title, xlabel, ylabel)
   },
   'profit' = {
     plot_obj <- profit_lift_chart(data, title, xlabel, ylabel)
   },
   'decile' = {
     plot_obj <- decile_lift_chart(data, title, xlabel, ylabel)
   },
   'profit_decile' = {
     plot_obj <- decile_profit_lift_chart(data, title, xlabel, ylabel)
   },
   'noncum' = {
     plot_obj <- noncum_decile_lift_chart(data, title, xlabel, ylabel)
   },
   'profit_noncum' = {
     plot_obj <- noncum_decile_profit_lift_chart(data, title, xlabel, ylabel)
   }
  )
  # return object
  obj <- list(data = data,
              table_type = chart_type,
              plot_object = plot_obj
  )
  class(obj) <- "liftchart"

  return(invisible(obj))
}
