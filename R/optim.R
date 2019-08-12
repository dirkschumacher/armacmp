#' Optimize arbitrary and differentiable functions
#'
#' The function compiles the code to C++ and uses Armadillo and ensmallen to optimize it.
#'
#' @param data a named list of prior data you would like to supply to the evaluate function.
#' @param evaluate a function that is to be minimized. It should return a single numeric.
#' @param gradient optional, a function computing the gradient of `evaluate`
#' @param optimizer one of the many optimizers
#' @include optimizers.R
#'
#' @examples
#' \dontrun{
#' optimize <- compile_optimization_problem(
#'   data = list(),
#'   evaluate = function(x) {
#'     return(2 * norm(x)^2)
#'   },
#'   optimizer = optimizer_SA()
#' )
#'
#' # should be roughly c(0, 0, 0)
#' result <- optimize(matrix(c(1, -1, 1), ncol = 1))
#' }
#' @export
compile_optimization_problem <- function(data = list(), evaluate, gradient, optimizer = optimizer_SA()) {
  stopifnot(is.function(evaluate))
  fun_args <- Filter(function(x) !is.null(x), as.list(args(evaluate)))
  eval_fun_argname <- names(fun_args)
  stopifnot(length(eval_fun_argname) == 1L)
  eval_fun_code <- armacmp_compile_internal(evaluate, "Evaluate", overwrite_return = "double")
  gradient_fun_code <- NULL
  if (!missing(gradient)) {
    gradient_fun_code <- armacmp_compile_internal(gradient, "UserGradient", overwrite_return = "arma::mat")
  }
  ens_code <- generate_ens_optimzer_cpp_code(data,
    eval_fun_argname = eval_fun_argname,
    eval_fun_code = eval_fun_code,
    gradient_fun_code = gradient_fun_code,
    optimizer
  )
  envir <- new.env(parent = globalenv())
  Rcpp::cppFunction(ens_code,
    depends = c("RcppArmadillo", "RcppEnsmallen"),
    includes = "#include <ensmallen.hpp>",
    plugins = "cpp11", env = envir
  )
  envir$armacmp_optim
}

generate_ens_optimzer_cpp_code <- function(data, eval_fun_argname, eval_fun_code, optimizer_code, gradient_fun_code) {
  input_code <- generate_cpp_input_parameters_code(data)
  input_var_names <- names(data)
  any_input_vars <- length(data) > 0L
  if (any_input_vars) {
    optional_class_constructor <- paste0(
      "OptimFunction(", paste0(input_code, collapse = ", "), ")",
      " : ", paste0(paste0(input_var_names, "(", input_var_names, ")"), collapse = ", "), " {};\n"
    )
    optional_member_vars <- paste0(
      paste0(input_code, collapse = ";\n"), ";\n"
    )
    class_init_code <- paste0("OptimFunction f(", paste0(input_var_names, collapse = ", "), ");\n")
  } else {
    optional_class_constructor <- optional_member_vars <- ""
    class_init_code <- "OptimFunction f;\n"
  }
  if (!is.null(gradient_fun_code)) {
    optional_public_gradient <- paste0(
      "void Gradient(const arma::mat& x, arma::mat& gradient) {",
      "gradient = UserGradient(x);",
      "};\n"
    )
    optional_private_gradient <- paste0(gradient_fun_code$cpp_code, ";\n")
  } else {
    optional_public_gradient <- optional_private_gradient <- ""
  }
  code <- paste0(
    "arma::mat armacmp_optim(", paste0(input_code, collapse = ", "),
    if (any_input_vars) ",", "const arma::mat& ", eval_fun_argname, ") {\n",
    "\nclass OptimFunction {\n",
    "public:\n",
    optional_class_constructor,
    optional_public_gradient,
    eval_fun_code$cpp_code, ";\n",
    "private:\n",
    optional_member_vars,
    optional_private_gradient,
    "\n};\n",
    "arma::mat input = ", eval_fun_argname, ";\n",
    optimizer_code,
    class_init_code,
    "optimizer.Optimize(f, input);\n",
    "return input;",
    "}\n"
  )
  code
}
