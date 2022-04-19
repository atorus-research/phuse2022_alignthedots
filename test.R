library(Tplyr)
library(dplyr)
library(purrr)


#' Apply formats as specified by an f_str
#' 
#' This is the outer function that would take the data, take the format, and
#' apply the formats as specified
#'
#' @param dat Input data frame
#' @param format An f_str object from Tplyr
#'
#' @return Character vector of formatted values the length of dat
#'
#' @examples
#' 
#' mtcars %>% 
#'   apply_formats(format=f_str("xx (x.xx)", hp, wt))
apply_formats <- function(dat, format=NULL) {
   # The purpose of this is simply to properly vectorize over apply_fmts
   pmap_chr(dat, function(...) apply_fmts(...), fmt=format)
}


#' Internal function to apply formatting
#'
#' This is the primary work of applying the formatting. This is non-vectorized,
#' and is intended to be called withing a pmap_chr call. Any custom application
#' of the f_str could be updated here, to cater to the specific structure of the
#' input data
#' 
#' @param ... Ellipse collected by pmap
#'
#' @return Output character vector
apply_fmts <- function(...) {
   # This pulls out the ellipse into the function environment as local variables
   list2env(list(...), env=environment())

   # Allocate the output character vector
   repl <- vector('list', length(fmt$settings))
   
   # Loop over and grab the formatted values of each argument in the f_str
   for (i in seq_along(fmt$settings)) {
      repl[[i]] <- Tplyr:::num_fmt(eval(fmt$vars[[i]]), i, fmt=fmt)
   }
   
   # Build up the arguments and apply to sprintf
   args <- append(list(fmt$repl_str), repl)
   do.call('sprintf', args)
}

mtcars %>% 
   apply_formats(format=f_str("xxx (x.xx)", hp, wt))