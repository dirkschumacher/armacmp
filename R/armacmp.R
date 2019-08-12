#' Compile Linear Algebra Code to C++
#'
#' @param fun a function
#' @param verbose print out compiler information
#'
#' @export
compile <- function(fun, verbose = FALSE) {
  stopifnot(is.function(fun))
  compiled_code <- translate(fun, function_name = "armacmp_fun")
  if (verbose) {
    message(format(compiled_code))
  }
  envir <- new.env(parent = globalenv())
  Rcpp::cppFunction(compiled_code$cpp_code,
    depends = "RcppArmadillo",
    plugins = "cpp11", env = envir
  )
  envir$armacmp_fun
}
