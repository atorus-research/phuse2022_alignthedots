library(Tplyr)
library(dplyr)
library(purrr)


#' Apply formats as specified by an f_str
#' 
#' This is the outer function that would take the data, take the format, and
#' apply the formats as specified
#'
#'
#' @param format_string A string representing the desired format of the output character vector
#' @param ... Variable from the dataframe to be formatted
#' @param empty If NA values are present, text to fill the space
#'
#' @return Character vector of formatted values the length of the input vectors
#'
#' @examples
#' 
#' mtcars %>% 
#'   mutate(
#'      formatted_string = apply_formats("xxx (x.xx)", hp, wt)
#'   )
apply_formats <- function(format_string, ..., empty = c(.overall = "")) {
   
   format <- f_str(format_string, ..., empty=empty)
   
   # The purpose of this is simply to properly vectorize over apply_fmts
   pmap_chr(list(...), function(...) apply_fmts(...), fmt=format)
}


#' Internal function to apply formatting
#'
#' This is the primary work of applying the formatting. This is non-vectorized,
#' and is intended to be called withing a pmap_chr call. Any custom application
#' of the f_str could be updated here, to cater to the specific structure of the
#' input data
#' 
#' @param ... Ellipse collected by pmap
#' @param fmt Format string object
#'
#' @return Output character vector
apply_fmts <- function(..., fmt) {

   nums <- list(...)

   # Allocate the output character vector
   repl <- vector('list', length(fmt$settings))
   
   # Loop over and grab the formatted values of each argument in the f_str
   for (i in seq_along(fmt$settings)) {
      repl[[i]] <- Tplyr:::num_fmt(nums[[i]], i, fmt=fmt)
   }
   
   # Build up the arguments and apply to sprintf
   args <- append(list(fmt$repl_str), repl)
   do.call('sprintf', args)
}

mtcars %>% 
   mutate(
      formatted_string = apply_formats("xxx (x.xx)", hp, wt)
   )