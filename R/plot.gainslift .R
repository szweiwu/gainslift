plot.gainsdata <- function(gainsdata, title = NULL, xlabel = NULL, ylabel = NULL){
  chart_type <- gainsdata$table_type
  data <- gainsdata$data
  
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
  class(obj) <- "plot.gainsdata"
  
  return(obj)
}

plot.gainsdata.diffsize <- function(gainsdata.diffsize, title = NULL, xlabel = NULL, ylabel = NULL){
  chart_type <- gainsdata$table_type
  data <- gainsdata$data
  
  # Plot charts
  switch(chart_type,
    'cum' = {
      plot_obj <- gains_chart(data, title, xlabel, ylabel)
    },
    'decile' = {
      plot_obj <- decile_gains_chart(data, title, xlabel, ylabel)
    },
    'noncum' = {
      plot_obj <- noncum_decile_gains_chart(data, title, xlabel, ylabel)
    },
    {
      stop('Wrong type: Profit charts not provided')
    }
  )
  # return object
  obj <- list(data = data,
              table_type = chart_type,
              plot_object = plot_obj
  )
  class(obj) <- "plot.gainsdata.diffsize"
  
  return(obj)
}

########## Lift ##########
plot.liftdata <- function(liftdata, title = NULL, xlabel = NULL, ylabel = NULL){
  chart_type <- liftdata$table_type
  data <- liftdata$data
  
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
  class(obj) <- "plot.liftdata"
  
  return(obj)
}

plot.liftdata.diffsize <- function(liftdata.diffsize, title = NULL, xlabel = NULL, ylabel = NULL){
  chart_type <- liftdata$table_type
  data <- liftdata$data
  
  # Plot charts
  switch(chart_type,
   'cum' = {
     plot_obj <- lift_chart(data, title, xlabel, ylabel)
   },
   'decile' = {
     plot_obj <- decile_lift_chart(data, title, xlabel, ylabel)
   },
   'noncum' = {
     plot_obj <- noncum_decile_lift_chart(data, title, xlabel, ylabel)
   },
   {
     stop('Wrong type: Profit charts not provided')
   }
  )
  # return object
  obj <- list(data = data,
              table_type = chart_type,
              plot_object = plot_obj
  )
  class(obj) <- "plot.liftdata.diffsize"
  
  return(obj)
}