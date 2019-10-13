#' Greedy Knapsack
#' 
#' Return the maximum value given the capacity of knapsack by sorting the items in decreasing order of value per unit of weight.
#' @usage greedy_knapsack(x, W)
#' @param x A \code{data.frame} consisting of two variables. \code{w} represents the object's weight and \code{v} is the value.
#' @param W the maximum capacity of the knapsack.
#' @return A \code{list} containing the maximum value and the elements.
#' @references \url{en.wikipedia.org/wiki/Knapsack_problem}
#' @name greedy_knapsack
#' @export


greedy_knapsack = function(x, W){
  
  stopifnot(is.data.frame(x), length(colnames(x)) == 2, colnames(x) == c("w", "v"), W>=0)
  
  # sort x by value/weight 
  vpw = x[,2]/x[,1]
  index = 1:nrow(x)
  x = cbind(x, vpw, index)
  x = x[order(-vpw),]
  
  # insert items until there's no longer space in the sack
  items = c()
  value = 0
  i = 1
  repeat{
    if (W - x[i,1] < 0){break()}
    else {
      value = value + x[i,2]
      W = W - x[i,1]
      items = c(items, x[i,4])
      i = i + 1
    }
  }
  
  return(list(value = value, elements = items))
}

