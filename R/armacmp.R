#' Compile Linear Algebra Code to C++
#'
#' @param code an R like expression that is translated to C++
#'
#' @export
armacmp <- function(code) {
  ast <- substitute(code)
  ast <- annotate_ast(ast)
  compiled_code <- compile_to_function(ast, function_name = "armacmpfun")
  envir <- new.env(parent = globalenv())
  Rcpp::cppFunction(compiled_code,
    depends = "RcppArmadillo",
    plugins = "cpp11", env = envir
  )
  envir$armacmpfun
}
