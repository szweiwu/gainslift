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

gainsdata.diffsize <- function(list, type = 'cum'){
  # Check if "predictions" and "responses" meets the data requirements
  for (i in 1:length(list)){
    data_requirement_check(data.frame(list[[i]][[1]]), as.vector(list[[i]][[2]]), cost=0, benefit=0)
  }
  # Identify chart type
  chart_type <- chart_type_check(type, cost=0, benefit=0)
  
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


liftdata <- function(predictions, responses, type = 'cum'){
  # Check if "predictions" and "responses" meets the data requirements
  data_requirement_check(predictions, responses, cost, benefit)
  
  # Identify chart type
  chart_type <- chart_type_check(type, cost, benefit)
  
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

liftdata.diffsize <- function(list, type = 'cum'){
  # Check if "predictions" and "responses" meets the data requirements
  for (i in 1:length(list)){
    data_requirement_check(data.frame(list[[i]][[1]]), as.vector(list[[i]][[2]]), cost=0, benefit=0)
  }
  # Identify chart type
  chart_type <- chart_type_check(type, cost=0, benefit=0)
  
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