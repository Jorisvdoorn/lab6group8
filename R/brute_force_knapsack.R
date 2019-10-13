#' Brute Force Knapsack
#' 
#' Return the maximum value given the capacity of knapsack by looking through every possible alternatives.
#' @usage brute_force_knapsack(x, W, parallel = FALSE)
#' @param x A \code{data.frame} consisting of two variables. \code{w} represents the object's weight and \code{v} is the value.
#' @param W the maximum capacity of the knapsack.
#' @param parallel \code{FALSE} by default. If \code{TRUE}, the function is parallelized over detected cores.
#' @return A \code{list} containing the maximum value and the elements.
#' @references \url{en.wikipedia.org/wiki/Knapsack_problem}
#' @name brute_force_knapsack
#' @import parallel
#' @export


library(parallel)

brute_force_knapsack = function(x, W, parallel = FALSE){
  
  stopifnot(is.data.frame(x), length(colnames(x)) == 2, colnames(x) == c("w", "v"), W>=0)
  
  if (parallel == FALSE){
    
    # calculate the sum of weights and values from all possible subsets
    total_weight = unlist(lapply(1:nrow(x), FUN = function(i){combn(x[,1], i, sum)}))
    total_value = unlist(lapply(1:nrow(x), FUN = function(i){combn(x[,2], i, sum)}))
    subsets = unlist(lapply(1:nrow(x), FUN = function(i){combn(1:nrow(x), m = i, paste, collapse = "")}))
    
    # find the maximum value given the knapsack size
    max_index = which(total_value == max(total_value[which(total_weight<=W)]))
    
    return(list(value = round(total_value[max_index]), elements = as.numeric(strsplit(subsets[max_index], "")[[1]])))
  }
  
  
  else{
    # make clusters
    cores = detectCores()
    cl = makeCluster(cores, type = "PSOCK")
    
    # calculate the sum of weights and values from all possible subsets
    total_weight = unlist(parLapply(cl, 1:nrow(x), fun = function(i){combn(x[,1], i, sum)}))
    total_value = unlist(parLapply(cl, 1:nrow(x), fun = function(i){combn(x[,2], i, sum)}))
    subsets = unlist(parLapply(cl, 1:nrow(x), fun = function(i){combn(1:nrow(x), m = i, paste, collapse = "")}))
    
    stopCluster(cl)
    
    # find the maximum value given the knapsack size
    max_index = which(total_value == max(total_value[which(total_weight<=W)]))
    
    return(list(value = round(total_value[max_index]), elements = subsets[[max_index]]))
  }
  
}
