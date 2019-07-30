#' Compile a function to C++
#'
#' @param fun a function
#' @param function_name the function name
#'
#' @return a list of type "armacmp_cpp_fun"
#' @export
armacmp_compile <- function(fun, function_name) {
  stopifnot(
    is.function(fun),
    is.character(function_name),
    length(function_name) == 1L
  )
  fun_body <- methods::functionBody(fun)
  fun_args <- as.list(args(fun))

  fun_args <- Filter(function(x) !is.null(x), fun_args)
  fun_args <- lapply(fun_args, function(x) {
    if (is.call(x)) {
      eval(x, envir = parent.frame())
    } else {
      # we overwrite by matrix type ... YOLO
      type_matrix()
    }
  })
  annotated_ast <- annotate_ast(fun_body)

  return_type <- NULL

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

  compile_element.annotated_element_colsums <- function(x) {
    paste0("arma::sum( ", compile_element(x$annotated_sexp[[2L]]), ", 0 )")
  }

  compile_element.annotated_element_rowsums <- function(x) {
    paste0("arma::sum( ", compile_element(x$annotated_sexp[[2L]]), ", 1 )")
  }

  compile_element.annotated_element_colmeans <- function(x) {
    paste0("arma::mean( ", compile_element(x$annotated_sexp[[2L]]), ", 0 )")
  }

  compile_element.annotated_element_rowmeans <- function(x) {
    paste0("arma::mean( ", compile_element(x$annotated_sexp[[2L]]), ", 1 )")
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

  compile_element.annotated_element_not_supported <- function(x) {
    stop("Sorry, but the expression:\n\n",
      deparse(x$original_sexp),
      "\n\ncannot be translated into a C++ construct.\n",
      "Not all R functions are supported.",
      call. = FALSE
    )
  }

  compile_element.annotated_element_return <- function(x) {
    # always return something
    stopifnot(length(x$annotated_sexp) %in% c(2L, 3L))
    if (length(x$annotated_sexp) == 3L) {
      type_spec <- eval(x$annotated_sexp[[3]]$original_sexp)
      if (is.null(return_type) || return_type == type_spec$cpp_type) {
        return_type <<- type_spec$cpp_type
      } else {
        stop("You specified two different return types ",
          return_type, " and ", type_spec$cpp_type,
          ". In C++ a function can only have one return type.",
          call. = FALSE
        )
      }
    }
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
    seq_along(fun_args),
    function(i) {
      x <- fun_args[[i]]
      name <- names(fun_args)[[i]]
      paste0("const ", x$cpp_type, "& ", name)
    }, character(1L)
  )
  if (is.null(return_type)) {
    return_type <- "arma::mat"
  }
  cpp_code <- paste0(
    return_type, " ", function_name,
    "(",
    paste0(input_params, collapse = ", "),
    ")", "\n",
    "{", "\n",
    paste0(output, collapse = "\n"), "\n",
    "}", "\n"
  )
  new_cpp_function(
    original_code = fun,
    cpp_code = cpp_code
  )
}

new_cpp_function <- function(original_code, cpp_code) {
  structure(
    list(
      original_code = original_code,
      cpp_code = cpp_code
    ),
    class = "armacmp_cpp_fun"
  )
}

#' @export
format.armacmp_cpp_fun <- function(x, ...) {
  original_code_str <- deparse(x$original_code)
  paste0(
    "Compiled R function\n\n",
    paste0(original_code_str, collapse = "\n"),
    "\n\n=>\n\n",
    x$cpp_code
  )
}

#' @export
print.armacmp_cpp_fun <- function(x, ...) {
  cat(format(x, ...), "\n")
}
