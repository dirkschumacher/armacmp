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

  annotated_ast <- annotate_ast(fun)
  return_cpp_type <- if (!missing(overwrite_return)) {
    overwrite_return
  }

  cpp_code <- annotated_ast$compile(
    fun_name = function_name,
    overwrite_return = return_cpp_type
  )

  if (is.null(return_cpp_type) && annotated_ast$get_cpp_type() == "auto") {
    function_body <- annotated_ast$get_function_body()$get_sexp()
    stop(
      "The return type of the function cannot be automatically deduced.",
      " Consider adding an explicit type annotation to your `return` statement.",
      "\n\n",
      paste0(deparse(function_body), collapse = "\n"),
      call. = FALSE
    )
  }

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

generate_cpp_input_types <- function(fun_args) {
  vapply(
    seq_along(fun_args),
    function(i) {
      x <- fun_args[[i]]
      name <- names(fun_args)[[i]]
      if (x$cpp_type %in% c("double", "int")) {
        x$cpp_type
      } else {
        paste0("const ", x$cpp_type, "&")
      }
    }, character(1L)
  )
}

generate_cpp_input_parameters_code <- function(fun_args, is_reassigned = function(x) FALSE) {
  vapply(
    seq_along(fun_args),
    function(i) {
      x <- fun_args[[i]]
      name <- names(fun_args)[[i]]
      if (x$cpp_type %in% c("double", "int") || is_reassigned(name)) {
        paste0(x$cpp_type, " ", name)
      } else {
        paste0("const ", x$cpp_type, "& ", name)
      }
    }, character(1L)
  )
}
