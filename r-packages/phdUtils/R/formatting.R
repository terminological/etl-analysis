#' 2 decimal place
#'
#' @param x a numeric or vector of numerics
#' @param x a unit specification
#' @keywords format
#' @export
#' @examples
#' df %>% vocabLookup(omop)
twoDp <- function(x,unit="") {
  paste0(sprintf("%.2f",x),unit)
}

#' 3 decimal place
#'
#' @param x a numeric or vector of numerics
#' @param x a unit specification
#' @keywords format
#' @export
#' @examples
#' df %>% vocabLookup(omop)
threeDp <- function(x,unit="") {
  paste0(sprintf("%.3f",x),unit)
}

#' 4 decimal place
#'
#' @param x a numeric or vector of numerics
#' @param x a unit specification
#' @keywords format
#' @export
#' @examples
#' df %>% vocabLookup(omop)
fourDp <- function(x,unit="") {
  paste0(sprintf("%.4f",x),unit)
}

#' mean and confidence intervals using t-test
#'
#' @param x a numeric or vector of numerics
#' @param f a formatting function
#' @keywords format
#' @export
#' @examples
#' df %>% vocabLookup(omop)
meanAndConfidence <- function(x, f=twoDp, ...) {
  tmp <- t.test(x)
  return(paste0(f(tmp$estimate[["mean of x"]],...)," (",f(tmp$conf.int[1],...)," - ",f(tmp$conf.int[2],...),")"))
}
