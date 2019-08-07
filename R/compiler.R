#' Compile a function to C++
#'
#' @param fun a function
#' @param function_name the function name
#'
#' @return a list of type "armacmp_cpp_fun"
#' @export
armacmp_compile <- function(fun, function_name) {
  armacmp_compile_internal(fun, function_name)
}

armacmp_compile_internal <- function(fun, function_name, overwrite_return) {
  stopifnot(
    is.function(fun),
    is.character(function_name),
    length(function_name) == 1L
  )
  # fun_body <- methods::functionBody(fun)
  # fun_args <- as.list(args(fun))
  #
  # fun_args <- Filter(function(x) !is.null(x), fun_args)
  # fun_args <- lapply(fun_args, function(x) {
  #   if (is.call(x)) {
  #     eval(x, envir = parent.frame())
  #   } else {
  #     # we overwrite by matrix type ... YOLO
  #     type_matrix()
  #   }
  # })
  #
  # # here we identify the arguments of type matrix/vector
  # # this is then used in the annotation process to decide
  # # the right type for assignment operations
  # arma_args <- Map(function(x) {
  #   grepl("^arma", x$cpp_type)
  # }, fun_args)
  # is_no_arma_datatype <- function(symbol_name) {
  #   !is.null(arma_args[[symbol_name]]) && arma_args[[symbol_name]]
  # }

  annotated_ast <- annotate_ast(fun)
  return_cpp_type <- if (!missing(overwrite_return)) {
    overwrite_return
  }
  cpp_code <- annotated_ast$compile(fun_name = function_name,
                                    overwrite_return = return_cpp_type)

  # return_nodes <- annotated_ast$find_all_returns()
  # if (length(return_nodes) == 0L) {
  #   stop("Your function needs to have at least on `return` statement.", call. = FALSE)
  # }
  # return_cpp_type <- vapply(return_nodes, function(x) x$get_cpp_type(), character(1L))
  # stopifnot(
  #   length(unique(return_cpp_type)) == 1L
  # )
  # return_cpp_type <- return_cpp_type[[1L]]

  # build the final function body string
  # input_params <- generate_cpp_input_parameters_code(fun_args)
  # cpp_code <- paste0(
  #   return_cpp_type, " ", function_name,
  #   "(",
  #   paste0(input_params, collapse = ", "),
  #   ")", "\n",
  #   paste0(output, collapse = "\n"), "\n"
  # )
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
    "R function\n\n",
    paste0(original_code_str, collapse = "\n"),
    "\n\nC++ function translation\n\n",
    x$cpp_code
  )
}

#' @export
print.armacmp_cpp_fun <- function(x, ...) {
  cat(format(x, ...), "\n")
}

generate_cpp_input_parameters_code <- function(fun_args) {
  vapply(
    seq_along(fun_args),
    function(i) {
      x <- fun_args[[i]]
      name <- names(fun_args)[[i]]
      if (x$cpp_type %in% c("double", "int")) {
        paste0(x$cpp_type, " ", name)
      } else {
        paste0("const ", x$cpp_type, "& ", name)
      }
    }, character(1L)
  )
}
