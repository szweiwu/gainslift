# Function to build the full table for ploting gains and lift chart.
full_table <- function(predictions, responses){
  # Create data table
  data <- data.frame("Cum.Records"= "", "Cum.Records.Pct"= "", "Response"= responses, "Predictions"= predictions, "Cum.Response"= "")

  # Rank columns by Predictions
  data <- data[order(data$Predictions, decreasing = TRUE),]
  data$Cum.Records <- seq(from = 1, to = length(data$Predictions), by = 1)
  rownames(data) <- data$Cum.Records

  # Calculate percentage of records
  data$Cum.Records.Pct <- (data$Cum.Records/length(data$Cum.Records))*100

  # Calculate number and percentage of cumulative responsess with predictionsion model
  data$Cum.Response <- cumsum(data$Response)
  data$Cum.Response.Pct <- (data$Cum.Response/tail(data$Cum.Response, n=1))*100

  # Calculate number and percentage of cumulative responsess with random model
  data$Random.Response <- tail(data$Cum.Response, n=1)*(data$Cum.Records/tail(data$Cum.Records, n=1))
  data$Random.Response.Pct <- tail(data$Cum.Response.Pct, n=1)*data$Cum.Records.Pct*0.01

  # Calaulate lift values
  data$Lift <- (data$Cum.Response/data$Cum.Records) / (tail(data$Cum.Response, n=1)/tail(data$Cum.Records, n=1))

  return(data)
}

########## Decile Table ##########
decile_table <- function(predictions, responses){
  # Create data table
  data <- full_table(predictions, responses)

  # Create quantiles in data table
  data <- data %>% mutate(quantile = ntile(Cum.Records, 10))

  # Create decile table
  decile <- seq(from = 10, to = 100, by = 10)
  decile_data <- data.frame("Cum.Records.Pct"= decile)

  # Aggregate Responses into deciles
  decile_data$Response <- aggregate(Response ~ quantile, data, sum)[,2]

  # Calculate number and percentage of cumulative responsess with predictionsion model
  decile_data$Cum.Response <- cumsum(decile_data$Response)
  decile_data$Cum.Response.Pct <- (decile_data$Cum.Response/tail(decile_data$Cum.Response, n=1))*100

  # Calculate percentage of non-cumulative responsess with predictionsion model
  decile_data$Response.Pct <- (decile_data$Response/tail(decile_data$Cum.Response, n=1))*100

  # Calculate number and percentage of cumulative responsess with random model
  decile_data$Random.Response <- tail(decile_data$Cum.Response, n=1)*(decile_data$Cum.Records/tail(decile_data$Cum.Records, n=1))
  decile_data$Random.Response.Pct <- tail(decile_data$Cum.Response.Pct, n=1)*decile_data$Cum.Records.Pct*0.01

  # Calaulate lift values
  decile_data$Lift <- (decile_data$Cum.Response / tail(decile_data$Cum.Response, n=1)) / (decile_data$Cum.Records.Pct*0.01)
  # Calculate non-cumulative lift values
  decile_data$Noncum.Lift <- (decile_data$Response / tail(decile_data$Response, n=1)) / (decile_data$Cum.Records.Pct*0.01)

  # Add (0,0) point
  decile_data <- rbind(c(0,0,0,0,0,0,0,0,0), decile_data)

  return(decile_data)
}

########## Profit Table ##########
profit_table <- function(predictions, responses, benefit, cost){
  # Create data table
  data <- full_table(predictions, responses)

  # Calculate Profits with predictionsion model
  data$Profit <- replace(data$Response, data$Response == 1, (benefit-cost))
  data$Profit <- replace(data$Profit, data$Profit == 0, -cost)

  # Calculate cumulative Profits with predictionsion model
  data$Cum.Profit <- cumsum(data$Profit)

  # Calculate cumulative Profits with random model
  data$Random.Profit <- tail(data$Profit, n=1)*(data$Cum.Records.Pct*0.01)
  data$Cum.Random.Profit <- tail(data$Cum.Profit, n=1)*(data$Cum.Records.Pct*0.01)

  # Calculate the Incremental Profits (differeces of cumulative profits using preiction and random model)
  data$Profit.Diff <- data$Cum.Profit - data$Cum.Random.Profit

  return(data)
}

########## Decile Profit Table ##########
decile_profit_table <- function(predictions, responses, benefit, cost){
  # Create data table
  data <- profit_table(predictions, responses, benefit, cost)

  # Create quantiles in data table
  data <- data %>% mutate(quantile = ntile(Cum.Records, 10))

  # Create decile table
  decile <- seq(from = 10, to = 100, by = 10)
  decile_data <- data.frame("Cum.Records.Pct"= decile)

  # Aggregate Responses into deciles
  decile_data$Response <- aggregate(Response ~ quantile, data, sum)[,2]

  # Calculate number and percentage of cumulative responsess with predictionsion model
  decile_data$Cum.Response <- cumsum(decile_data$Response)
  decile_data$Cum.Response.Pct <- (decile_data$Cum.Response/tail(decile_data$Cum.Response, n=1))*100

  # Calculate percentage of non-cumulative responsess with predictionsion model
  decile_data$Response.Pct <- (decile_data$Response/tail(decile_data$Cum.Response, n=1))*100

  # Calculate number and percentage of cumulative responsess with random model
  decile_data$Random.Response <- tail(decile_data$Cum.Response, n=1)*(decile_data$Cum.Records/tail(decile_data$Cum.Records, n=1))
  decile_data$Random.Response.Pct <- tail(decile_data$Cum.Response.Pct, n=1)*decile_data$Cum.Records.Pct*0.01

  # Calaulate lift values
  decile_data$Lift <- (decile_data$Cum.Response / tail(decile_data$Cum.Response, n=1)) / (decile_data$Cum.Records.Pct*0.01)
  # Calculate non-cumulative lift values
  decile_data$Noncum.Lift <- (decile_data$Response / tail(decile_data$Response, n=1)) / (decile_data$Cum.Records.Pct*0.01)

  # Aggregate Profits into deciles
  decile_data$Profit <- aggregate(Profit ~ quantile, data, sum)[,2]

  # Calculate cumulative Profits with predictionsion model
  decile_data$Cum.Profit <- cumsum(decile_data$Profit)

  # Calculate cumulative Profits with random model
  decile_data$Random.Profit <- tail(decile_data$Profit, n=1)*(decile_data$Cum.Records.Pct*0.01)
  decile_data$Cum.Random.Profit <- tail(decile_data$Cum.Profit, n=1)*(decile_data$Cum.Records.Pct*0.01)

  # Calculate the Incremental Profits (differeces of cumulative profits using preiction and random model)
  decile_data$Profit.Diff <- decile_data$Cum.Profit - decile_data$Cum.Random.Profit
  # Calculate non-cumulative Incremental Profits
  decile_data$Noncum.Profit.Diff <- decile_data$Profit - decile_data$Random.Profit

  return(decile_data)
}
