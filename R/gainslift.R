#' gainslift gainsliftTable Function
#' @import magrittr
#' @import dplyr
#' @import reshape2
#' @import ggplot2
#'
#' @param predictions A dataframe containing one or multiple numeric vectors of predicted response values.
#' All of the vectors must have the same length as \code{responses}.
#'
#' @param responses a numeric vector of actual response values.
#'
#' @param type 'cum' will give a cumulative table.
#' 'decile' will give a cumulative decile table.
#' 'noncum' will give a non-cumulative decile table.
#' (The default is set as 'cum')
#'
#' @param cost a numeric value of the cost of an incorrect classification.
#' e.g. If the cost of mailing a target is $5, then the cost should be set as 5.
#'
#' @param benefit a numeric value of the benefit of a correct classification.
#' e.g. If the value of a responder is $20, then the benefit should be set as 25.
#'
#'
#' @usage
#' gainsliftTable(predictions, responses, type = 'cum', cost = 0, benefit = 0)
#'
#' @export
gainsliftTable <- function(predictions, responses, type = 'cum', cost = 0, benefit = 0){
  # Check if "predictions" and "responses" meets the data requirements
  data_requirement_check(predictions, responses, cost, benefit)

  # Identify chart type
  chart_type <- type_check(type, cost, benefit)

  # Plot charts
  switch(chart_type,
    'cum' = {
     data <- lapply(predictions, full_table, responses = responses)
    },
    'profit' = {
     data <- lapply(predictions, profit_table, responses = responses, benefit = benefit, cost = cost)
    },
    'decile' = {
     data <- lapply(predictions, decile_table, responses = responses)
    },
    'profit_decile' = {
     data <- lapply(predictions, decile_profit_table, responses = responses, benefit = benefit, cost = cost)
    },
    'noncum' = {
     data <- lapply(predictions, decile_table, responses = responses)
    },
    'profit_noncum' = {
      data <- lapply(predictions, decile_profit_table, responses = responses, benefit = benefit, cost = cost)
    }

  )
  # return object
  obj <- list(data = data,
              table_type = chart_type)
  class(obj) <- "gainsliftTable"

  return(invisible(obj))
}

#' gainslift gainsliftTable_diffsize Function
#'
#'
#' @param list a list containing one or multiple lists of \code{list(predictions, reponses)}.
#' See \code{gainsliftTable} for the description of predict and reponse.
#'
#' @param type 'cum' will give a cumulative gains table.
#' 'decile' will give a cumulative decile gains table.
#' 'noncum' will give a non-cumulative decile gains table.
#' (The default is set as 'cum')
#'
#' @usage
#' gainsliftTable_diffsize(list, type = 'cum')
#'
#' @export
gainsliftTable_diffsize <- function(list, type = 'cum'){
  # Check if "predictions" and "responses" meets the data requirements
  for (i in 1:length(list)){
    data_requirement_check(data.frame(list[[i]][[1]]), as.vector(list[[i]][[2]]), cost=0, benefit=0)
  }
  # Identify chart type
  chart_type <- type_check(type, cost=0, benefit=0)

  # Plot charts
  switch(chart_type,
    'cum' = {
      data <- lapply(list[[1]][[1]], decile_table, responses = list[[1]][[2]])
      for (i in 1:(length(list)-1)){
        data <- append(data, lapply(list[[i+1]][[1]], decile_table, responses = list[[i+1]][[2]]))
      }
    },
    'decile' = {
      data <- lapply(list[[1]][[1]], decile_table, responses = list[[1]][[2]])
      for (i in 1:(length(list)-1)){
        data <- append(data, lapply(list[[i+1]][[1]], decile_table, responses = list[[i+1]][[2]]))
      }
    },
    'noncum' = {
      data <- lapply(list[[1]][[1]], decile_table, responses = list[[1]][[2]])
      for (i in 1:(length(list)-1)){
        data <- append(data, lapply(list[[i+1]][[1]], decile_table, responses = list[[i+1]][[2]]))
      }
    },
    {
      stop('Wrong type: Profit charts not provided')
    }
  )
  # return object
  obj <- list(data = data,
              table_type = chart_type)
  class(obj) <- "gainsliftTable_diffsize"

  return(invisible(obj))
}
