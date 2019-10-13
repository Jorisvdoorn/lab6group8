#' Dynamic Knapsack
#' 
#' Returns the maximum value given the capacity of knapsack by using dynamic programming.
#' @usage dynamic_knapsack(x, W)
#' @param x A \code{data.frame} consisting of two variables. \code{w} represents the object's weight and \code{v} is the value.
#' @param W the maximum capacity of the knapsack.
#' @return A \code{list} containing the maximum value and the elements.
#' @references \url{en.wikipedia.org/wiki/Knapsack_problem}
#' @name dynamic_knapsack
#' @export

dynamic_knapsack = function(x, W){
  
  stopifnot(is.data.frame(x), length(colnames(x)) == 2, colnames(x) == c("w", "v"), W>=0)
  
  # create a placeholder matrix of possible values
  n_items = nrow(x)+1
  w = W+1
  value_matrix = matrix(0, nrow = n_items, ncol = w)
  
  # fill the value_matrix with possible values
  for (i in 2:nrow(value_matrix)){
    for (j in 1:ncol(value_matrix)){
      if (x[i-1,1] > j){
        value_matrix[i,j] = value_matrix[i-1,j]
      }
      else {
        value_matrix[i,j] = max(value_matrix[i-1,j], value_matrix[i-1,j-x[i-1,1]] + x[i-1,2])
      }
    }
  }
  
  # record the optimum value
  results = list(value = round(value_matrix[n_items, w]))
  
  # find the items
  items = c()
  while (value_matrix[n_items, w] > 0){
    if (value_matrix[n_items, w] == value_matrix[n_items-1, w]){
      n_items = n_items-1
    }
    
    else {
      w = w - x[n_items-1,1]
      n_items = n_items-1
      items = c(items, n_items)
    }
  }
  
  results$elements = sort(items)
  return(results)
}

