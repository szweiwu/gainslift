# PURPOSE: Functions to check data requirements

# Check that the data vectors are numerical and of equal length
data_requirement_check <- function(predictions, responses, cost, benefit){
  # Check if cost and benefit value are numerical
  if (is.numeric(cost) == F || is.numeric(benefit) == F){
    stop('Your cost or benefit value is not numerical')
  }
  
  # Check if "responses" argument is a numerical vector
  if (is.numeric(responses) == F || is.vector(responses) == F) {
    stop('responses is not a numerical vector')
  }
  
  invisible(lapply(predictions, predictions_check, responses_vec = responses))
}

predictions_check <- function(predictions_vec, responses_vec){
  # Check if "predictions" argument is a numerical vector
  if (is.numeric(predictions_vec) == F || is.vector(predictions_vec) == F) {
    stop('predictions is not a numerical vector')
  }
  
  # Check if "predictions" and "responses" have the same length
  if (length(predictions_vec) != length(responses_vec)) {
    stop('predictions and responses have different length')
  }
}

# Identify which type of table to build
type_check <- function(type, cost, benefit){
  switch (type,
    'cum' = {
      if (cost == 0 && benefit == 0) {
        return('cum')
      }
      else{
        return('profit')
      }
    },
    'decile' = {
      if (cost == 0 && benefit == 0) {
        return('decile')
      }
      else{
        return('profit_decile')
      }
    },
    'noncum' = {
      if (cost == 0 && benefit == 0) {
        return('noncum')
      }
      else{
        return('profit_noncum')
      }
    },
    {
      stop('Wrong type: your type shoud be set as "cum", "decile", or "noncum"')
    }
  )
}