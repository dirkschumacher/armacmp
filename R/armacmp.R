#' Compile Linear Algebra Code to C++
#'
#' @param fun a function
#'
#' @export
armacmp <- function(fun) {
  stopifnot(is.function(fun))
  compiled_code <- armacmp_compile(fun, function_name = "armacmp_fun")
  envir <- new.env(parent = globalenv())
  Rcpp::cppFunction(compiled_code$cpp_code,
    depends = "RcppArmadillo",
    plugins = "cpp11", env = envir
  )
  envir$armacmp_fun
}
