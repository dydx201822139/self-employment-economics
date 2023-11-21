t_invariant_func <- function(list){
  n <- length(list)
  for(k in 1:n){
    val <- list[k]
    if(val >= 0){
      list <- rep(val,n)
      break
        }
  }
  if(list[1] < 0){
    list <- rep(NaN,n)
  }
  return(list)
}

t_partial_func <- function(list){
  n <- length(list)
  update_val <- pi
  start_index <- pi
  for(k in 1:n){
    val <- list[k]
    if(val != update_val & val >= 0){
      start_index <- k
      update_val <- val 
    }else if(val == update_val & val >= 0){
      end_index <- k-1
      list[start_index:end_index] <- rep(update_val,end_index)
    }else if(val < 0){
      list[k] <- NaN
    }
  }
  return(list)
}  