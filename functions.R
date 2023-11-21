#intermediary function to see if a variable is one of the wanted variables
names_compare <- function(name, to_compare_vec){
  variable_present <- FALSE #initialize output of logical
  #loop through each element of variables to be compared to
  for (i in 1:length(to_compare_vec)) {
    if(str_contains(name, to_compare_vec[i])) { #str_contains() from sjmisc 
      variable_present <- TRUE #change output if there is intersection of strings
      break
    }
  }
  return(variable_present)
}

#function to output vector of logicals that determines whether a variable in dataframe should be included
get_indices <- function(df, to_compare){
  variables <- names(df)
  return(sapply(variables, FUN = names_compare, to_compare_vec = to_compare)) #call intermediary function on each element of vector
}

#intermediary function to return a string with the first 2 characters removed
drop_characters <- function(string){
  return(substring(string, 3))
}

#function to return a vector of strings for the variables after removing the wave-specific parts of string
unify_names <- function(df){
  names_vec <- names(df)
  names_vec <- names_vec[names_vec != "pidp"] #'pidp' is excluded given it is common between waves
  names(df)[names(df) != "pidp"] <- sapply(names_vec, FUN = drop_characters) #call intermediary function
  return(df)
}

add_cross_wave_vars <- function(val, source_df, end_df, col_names){
  cols_indices <- which(names(source_df) %in% col_names)
  row_vec <- source_df[source_df$pidp == val,cols_indices]
  count <- length(end_df$pidp[end_df$pidp == val])
  to_be_copied_df <- rbind(row_vec, row_vec[rep(1, count - 1), ])
  end_df[end_df$pidp == val, cols_indices] <- to_be_copied_df
}


#function to replace all negative values if a non-negative value is observed for each pidp over all waves
t_invariant_func <- function(list){
  n <- length(list)
  for(k in 1:n){
    val <- list[k]
    #check if there is value that is non-negative
    if(val >= 0){
      list <- rep(val,n) #update the vector
      break
        }
  }
  #replace all negative values to NaN if no non-negative value identified
  if(list[1] < 0){
    list <- rep(NaN,n)
  }
  return(list)
}

#function to replace all negative values if a non-negative value is observed for each pidp over all waves
#and changes update if another non-negative value is found
t_partial_func <- function(list){
  n <- length(list)
  update_val <- pi #initialize with irrational number
  start_index <- pi
  for(k in 1:n){
    val <- list[k]
    # check cases and update accordingly
    if(val != update_val & val >= 0){
      start_index <- k
      update_val <- val 
    }else if(val == update_val & val >= 0){ #change update value if a new non-negative value is encountered
      end_index <- k-1
      list[start_index:end_index] <- rep(update_val,end_index)
    }else if(val < 0){ #change all obs. to NaN if only negative values were identified
      list[k] <- NaN
    }
  }
  return(list)
}  

#function to remove all the rows of dataframe if a NaN is identified for specific set of columns
del_NaN_from_cols <- function(df, cols) {
  rows_to_del <- complete.cases(df[, cols])
  return(df[rows_to_del, ])
}