compile_to_function <- function(annotated_ast, function_name) {
  stopifnot(is.list(annotated_ast))
  n_input_parameters <- 0L
  input_parameter_names <- c()
  new_input_matrix_parameter <- function() {
    n_input_parameters <<- n_input_parameters + 1L
    name <- paste0("param__input_", n_input_parameters)
    input_parameter_names <<- c(input_parameter_names, name)
    name
  }

  # define the compiler that takes an annotated AST and turns it
  # into code
  compile_element <- function(x) UseMethod("compile_element")
  compile_element.annotated_element_terminal <- function(x) {
    as.character(x$annotated_sexp)
  }
  compile_element.annotated_element_assignment <- function(x) {
    stopifnot(x$annotated_sexp[[2L]]$type == "terminal")

    # deduce type
    arma_type <- "arma::mat"
    if (x$annotated_sexp[[3L]]$type == "input_matrix") {
      arma_type <- "const arma::mat&"
    }

    paste0(
      arma_type, " ",
      compile_element(x$annotated_sexp[[2L]]),
      " = ",
      compile_element(x$annotated_sexp[[3L]]),
      ";"
    )
  }
  compile_element.annotated_element_replace <- function(x) {
    paste0(
      compile_element(x$annotated_sexp[[2L]]),
      " = ",
      compile_element(x$annotated_sexp[[3L]]),
      "; \n"
    )
  }

  compile_element.annotated_element_curley_bracket <- function(x) {
    paste0(
      "{\n",
      paste0(lapply(x$annotated_sexp[-1L], function(y) compile_element(y)), collapse = "\n"),
      "\n}\n"
    )
  }

  compile_element.annotated_element_for <- function(x) {
    # we currently allow just one pattern
    # for (i in seq_len(10)) { ... }
    # will be translated to
    # for (const int i : Rcpp::seq_len(10)) {
    #   ...
    # }
    iter_var_name <- x$annotated_sexp[[2L]]
    n <- x$annotated_sexp[[3L]][[2L]][[2L]]
    body <- x$annotated_sexp[[4L]]
    paste0(
      "for (const int ", compile_element(iter_var_name),
      " : Rcpp::seq_len(", compile_element(n), ")) { \n",
      compile_element(body),
      "}\n"
    )
  }

  compile_element.annotated_element_input_matrix <- function(x) {
    new_input_matrix_parameter()
  }

  compile_element.annotated_element_bracket <- function(x) {
    paste0("( ", compile_element(x$annotated_sexp[[2L]]), " )")
  }

  compile_element.annotated_element_pow <- function(x) {
    paste0(
      "arma::pow( ",
      compile_element(x$annotated_sexp[[2L]]),
      ", ",
      compile_element(x$annotated_sexp[[3L]]),
      " )"
    )
  }

  make_operator_fun <- function(op) {
    function(x) {
      len <- length(x$annotated_sexp)
      stopifnot(len %in% c(2L:3L))
      if (len == 2L) {
        paste0(
          op,
          compile_element(x$annotated_sexp[[2L]])
        )
      } else {
        paste0(
          compile_element(x$annotated_sexp[[2L]]),
          " ",
          op,
          " ",
          compile_element(x$annotated_sexp[[3L]])
        )
      }
    }
  }

  compile_element.annotated_element_matmul <- make_operator_fun("*")

  compile_element.annotated_element_mult <- make_operator_fun("%")
  compile_element.annotated_element_div <- make_operator_fun("/")
  compile_element.annotated_element_plus <- make_operator_fun("+")
  compile_element.annotated_element_minus <- make_operator_fun("-")

  compile_element.annotated_element_simple_unary_function <- function(x) {
    stopifnot(length(x$annotated_sexp) == 2L, !is.null(x$meta_data$armadillo_fun))
    paste0(x$meta_data$armadillo_fun, "( ", compile_element(x$annotated_sexp[[2L]]), " )")
  }

  compile_element.annotated_element_backsolve <- function(x) {
    paste0("arma::solve(arma::trimatu( ", compile_element(x$annotated_sexp[[2L]]), " ), ", compile_element(x$annotated_sexp[[3L]]), " )")
  }

  compile_element.annotated_element_forwardsolve <- function(x) {
    paste0("arma::solve(arma::trimatl( ", compile_element(x$annotated_sexp[[2L]]), " ), ", compile_element(x$annotated_sexp[[3L]]), " )")
  }

  compile_element.annotated_element_simple_binary_function <- function(x) {
    stopifnot(length(x$annotated_sexp) == 3L, !is.null(x$meta_data$armadillo_fun))
    paste0(
      x$meta_data$armadillo_fun,
      "( ",
      compile_element(x$annotated_sexp[[2L]]),
      ", ",
      compile_element(x$annotated_sexp[[3L]]),
      ")"
    )
  }

  compile_element.annotated_element_return <- function(x) {
    # always return something
    stopifnot(length(x$annotated_sexp) == 2L)
    paste0(
      "return ",
      compile_element(x$annotated_sexp[[2L]]),
      ";"
    )
  }

  output <- list()
  for (el in annotated_ast) {
    output <- c(output, list(compile_element(el)))
  }

  # build the final function body string
  input_params <- vapply(
    input_parameter_names,
    function(x) {
      paste0("const arma::mat& ", x)
    }, character(1L)
  )
  paste0(
    "arma::mat ", function_name,
    "(",
    paste0(input_params, collapse = ", "),
    ")", "\n",
    "{", "\n",
    paste0(output, collapse = "\n"), "\n",
    "}", "\n"
  )
}
