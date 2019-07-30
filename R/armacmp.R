#' Compile Linear Algebra Code to C++
#'
#' @param fun a function
#'
#' @export
armacmp <- function(fun) {
  stopifnot(is.function(fun))
  compiled_code <- compile_to_str(fun, function_name = "armacmpfun")
  envir <- new.env(parent = globalenv())
  Rcpp::cppFunction(compiled_code,
    depends = "RcppArmadillo",
    plugins = "cpp11", env = envir
  )
  envir$armacmpfun
}
