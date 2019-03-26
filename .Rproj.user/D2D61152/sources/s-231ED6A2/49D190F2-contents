#' gainslift gainsdata Function
#'
#'
#' @param predictions A dataframe containing one or multiple numeric vectors of predicted response values.
#' All of the vectors must have the same length as \code{responses}.
#'
#' @param responses a numeric vector of actual response values.
#'
#' @param type 'cum' will give a cumulative gains table.
#' 'decile' will give a cumulative decile gains table.
#' 'noncum' will give a non-cumulative decile gains table.
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
#' gainsdata(predictions, responses, type = 'cum', cost = 0, benefit = 0)
#'
#' @export
gainsdata <- function(predictions, responses, type = 'cum', cost = 0, benefit = 0){
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
  class(obj) <- "gainsdata"

  return(obj)
}

#' gainslift gainsdata_diffsize Function
#'
#'
#' @param list a list containing one or multiple lists of \code{list(predictions, reponses)}.
#' See \code{gainsdata} for the description of predict and reponse.
#'
#' @param type 'cum' will give a cumulative gains table.
#' 'decile' will give a cumulative decile gains table.
#' 'noncum' will give a non-cumulative decile gains table.
#' (The default is set as 'cum')
#'
#' @usage
#' gainsdata_diffsize(list, type = 'cum')
#'
#' @export
gainsdata_diffsize <- function(list, type = 'cum'){
  # Check if "predictions" and "responses" meets the data requirements
  for (i in 1:length(list)){
    data_requirement_check(data.frame(list[[i]][[1]]), as.vector(list[[i]][[2]]), cost=0, benefit=0)
  }
  # Identify chart type
  chart_type <- type_check(type, cost=0, benefit=0)

  # Plot charts
  switch(chart_type,
    'cum' = {
      for (i in 1:(length(list)-1)){
        data <- lapply(list[[i]][[1]], decile_table, responses = list[[i]][[2]])
        data <- append(data, lapply(list[[i+1]][[1]], decile_table, responses = list[[i+1]][[2]]))
      }
    },
    'decile' = {
      for (i in 1:(length(list)-1)){
        data <- lapply(list[[i]][[1]], decile_table, responses = list[[i]][[2]])
        data <- append(data, lapply(list[[i+1]][[1]], decile_table, responses = list[[i+1]][[2]]))
      }
    },
    'noncum' = {
      for (i in 1:(length(list)-1)){
        data <- lapply(list[[i]][[1]], decile_table, responses = list[[i]][[2]])
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
  class(obj) <- "gainsdata.diffsize"

  return(obj)
}

#' gainslift liftdata Function
#'
#'
#' @param predictions A dataframe containing one or multiple numeric vectors of predicted response values.
#' All of the vectors must have the same length as \code{responses}.
#'
#' @param responses a numeric vector of actual response values.
#'
#' @param type 'cum' will give a cumulative gains table.
#' 'decile' will give a cumulative decile gains table.
#' 'noncum' will give a non-cumulative decile gains table.
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
#' liftdata(predictions, responses, type = 'cum', cost = 0, benefit = 0)
#'
#' @export
liftdata <- function(predictions, responses, type = 'cum'){
  # Check if "predictions" and "responses" meets the data requirements
  data_requirement_check(predictions, responses, cost, benefit)

  # Identify chart type
  chart_type <- type_check(type, cost, benefit)

  # Plot charts
  switch(chart_type,
   'curve' = {
     data <- lapply(predictions, full_table, responses = responses)
   },
   'profit_curve' = {
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
  class(obj) <- "liftdata"

  return(obj)
}

#' gainslift liftdata_diffsize Function
#'
#'
#' @param list a list containing one or multiple lists of \code{list(predictions, reponses)}.
#' See \code{gainsdata} for the description of predict and reponse.
#'
#' @param type 'cum' will give a cumulative gains table.
#' 'decile' will give a cumulative decile gains table.
#' 'noncum' will give a non-cumulative decile gains table.
#' (The default is set as 'cum')
#'
#' @usage
#' liftdata_diffsize(list, type = 'cum')
#'
#' @export
liftdata_diffsize <- function(list, type = 'cum'){
  # Check if "predictions" and "responses" meets the data requirements
  for (i in 1:length(list)){
    data_requirement_check(data.frame(list[[i]][[1]]), as.vector(list[[i]][[2]]), cost=0, benefit=0)
  }
  # Identify chart type
  chart_type <- type_check(type, cost=0, benefit=0)

  # Plot charts
  switch(chart_type,
   'cum' = {
     for (i in 1:(length(list)-1)){
       data <- lapply(list[[i]][[1]], decile_table, responses = list[[i]][[2]])
       data <- append(data, lapply(list[[i+1]][[1]], decile_table, responses = list[[i+1]][[2]]))
     }
   },
   'decile' = {
     for (i in 1:(length(list)-1)){
       data <- lapply(list[[i]][[1]], decile_table, responses = list[[i]][[2]])
       data <- append(data, lapply(list[[i+1]][[1]], decile_table, responses = list[[i+1]][[2]]))
     }
   },
   'noncum' = {
     for (i in 1:(length(list)-1)){
       data <- lapply(list[[i]][[1]], decile_table, responses = list[[i]][[2]])
       data <- append(data, lapply(list[[i+1]][[1]], decile_table, responses = list[[i+1]][[2]]))
     }
   },
   {
     stop('Wrong type')
   }
  )
  # return object
  obj <- list(data = data,
              table_type = chart_type)
  class(obj) <- "liftdata.diffsize"

  return(obj)
}
