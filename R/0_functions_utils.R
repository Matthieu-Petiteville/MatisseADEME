

# easy_stats ----------------------------------------------------------------------------------------------------------------------------------------------
#' @title easy_stats
#' @description Returns a list of standard statistics about the vec input
#'
#' @param x A vector
#' @param stats_to_show The type of data we want
#'
#'
#' @return
#' @export
#'
#' @examples
#' easy_stats(1:10)
easy_stats <- function(x, stats_to_show = 1:2){


  res_l <- list()
  if(typeof(x) == "list"){
    for(name_it in names(x)){
      res_l[[name_it]] <- easy_stats(x[[name_it]])
    }
  }else{
    if(1 %in% stats_to_show){
      if(is.numeric(x)){
        stat_df <- tibble(Type = typeof(x),
                          Nb  = length(x),
                          Mean = mean(x, na.rm = T),
                          Median = median(x, na.rm = T),
                          Max = max_na(x, na.rm = T),
                          Min = min_na(x, na.rm = T),
                          StDev = sd(x, na.rm = T),
                          NbNa = length(which(is.na(x))))
      }else if(is.character(x)){
        stat_df <- tibble(Type = typeof(x),
                          Nb  = length(x),
                          Max = max_na(x, na.rm = T),
                          Min = min_na(x, na.rm = T),
                          NbNa = length(which(is.na(x))),
                          NbUnique = length(unique(x)))
      }else if(is.logical(x)){
        stat_df <- tibble(Type = typeof(x),
                          Nb  = length(x),
                          NbTrue = length(which(x)),
                          NbFalse = length(which(!x)),
                          TruePct = length(which(x)) / length(x),
                          NbNa = length(which(is.na(x))))
      }
      res_l[["Stats"]] <- stat_df
    }
    if(2 %in% stats_to_show){
        stat_df <- tibble(NbVal = length(x),
                          RkMax = which(x == max_na(x, na.rm = T))[1],
                          RkMin = which(x == min_na(x, na.rm = T))[1],
                          RkNA = which(is.na(x))[1])
        res_l[["Rank"]] <- stat_df
    }
  }
  return(res_l)


}


# floor_by_value ------------------------------------------------------------------------------------------------------------------------------------------
#' @title floor_by_value
#' @description This function takes a numeric vector and floors the individual value by the parameter value
#' or by default 0
#'
#' @param vector A numeric vector
#' @param value The floor value (defaults at 0)
#'
#' @return A floored vector
#'
#' @examples
#' a<- -5:10
#' floor_by_value(a)
floor_by_value <- function(vector, value = 0){

  return(sapply(seq_along(vector), function(x){
    return(max(vector[x], value))
  } ))


}


# min_na --------------------------------------------------------------------------------------------------------------------------------------------------
#' @title min_na
#' @description A finer min that handles situations where the vector is empty or full of NA
#'
#' @param x A vector
#' @param ... A free arg
#'
#' @return A vector or NA
#'
#' @examples
#' min_na(c())
#' min_na(c(NA, 10), na.rm = T)
min_na <- function(x = c(), ...){

  if(length(x) == 0){
    return(NA)
  }else{
    return(na_if(min(replace_na(x, Inf), ...), Inf))
  }

}


# max_na --------------------------------------------------------------------------------------------------------------------------------------------------
#' @title max_na
#' @description A finer max that handles situations where the vector is empty or full of NA
#'
#' @param x A vector
#' @param ... A free arg
#'
#' @return A vector or NA
#'
#' @examples
#' max_na(c())
#' max_na(c(NA, 10), na.rm = T)
max_na <- function(x = c(), ...){

  if(length(x) == 0){
    return(NA)
  }else{
    return(na_if(max(replace_na(x, -Inf), ...), -Inf))
  }

}


# replace_nan ---------------------------------------------------------------------------------------------------------------------------------------------
#' @title replace_nan
#' @description Function that replaces NaN values with "by" parameter
#'
#' @param x An input object
#' @param by The replacement for NaN values
#' @param ...
#'
#' @return An object
#' @export
#'
#' @examples
#' replace_nan(c(NA, NaN, 1,2,3), by = 0)
replace_nan <- function(x = c(), by = NA, ...){

  if(is_tibble(x) | is.data.frame(x)){
    for(x_it in names(x)){
      l_idx <- which(is.nan(x[[x_it]]))
      if(length(l_idx) > 0){
        x[l_idx, x_it] <- by
      }
    }
    return(x)
  }
  if(is.array(x) | is.vector(x)){
    l_idx <- which(is.nan(x))
    x[l_idx] <- by
    return(x)
  }
  stop("Not supported x type for function : replace_nan")

}



# remove_item ---------------------------------------------------------------------------------------------------------------------------------------------

#' @title remove_item
#' @description Removes a vector of items from a vector
#'
#' @param x The vector
#' @param item The item to be removed
#'
#' @return
#' @export
#'
#' @examples
remove_item <- function(x = c(), item = c()){

  if(is.array(x) | is.vector(x)){
    x <- x[which(!(x %in% item))]
    return(x)
  }
  stop("Not supported x type for function : remove_item")

}
