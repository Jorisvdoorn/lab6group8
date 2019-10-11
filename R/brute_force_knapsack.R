
brute_force_knapsack = function(x, W){
  
  stopifnot(is.data.frame(x), length(colnames(x)) == 2, colnames(x) == c("w", "v"))
  
  # create all possible subsets
  subsets = list()
  p = 1
  for (i in 1:nrow(x)){
    sets = combn(x = 1:nrow(x), m = i, simplify = FALSE)
    for (j in 1:length(sets)){
      subsets[[p]] = sets[[j]]
      p = p+1
    }
  }
  
  # calculate the total weight and value for each subset
  total_value = c()
  total_weight = c()
  for (i in 1:length(subsets)){
    total_weight[i] = sum(x[,1][subsets[[i]]])
    total_value[i] = sum(x[,2][subsets[[i]]])
  }
  
  # find the maximum value given the knapsack size
  max_index = which(total_value == max(total_value[which(total_weight<=W)]))
  
  return(list(value = round(total_value[max_index]), elements = subsets[[max_index]]))
  
}
