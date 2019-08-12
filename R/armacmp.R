#' Compile Linear Algebra Code to C++
#'
#' @param fun a function
#' @param verbose optional logical, print out compiler information
#'
#' This function always compiles functions.
#' Every function needs to have a `return` statement with an optional type argument.
#' All input parameters are by default of type double matrix.
#' Type inference is tried to be done, but sometimes it is helpful to add type annotation.
#'
#' Take a look at function reference vignette for more information.
#'
#' @examples
#' \dontrun{
#' trans <- compile(function(X) {
#'   return(t(X))
#' })
#' trans(matrix(1:10))
#' }
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
