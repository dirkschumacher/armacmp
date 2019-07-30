annotate_ast <- function(ast) {
  # here we have one S expression or a List of S expression
  # a list of S expression is only assumed at the top level
  is_list_of_sexps <- "{" %in% as.character(ast[[1L]])
  if (is_list_of_sexps) {
    annotated_ast <- list()
    iterator <- seq_along(ast)[-1L]
    for (i in iterator) {
      annotated_ast <- c(annotated_ast, list(classify_sexp(ast[[i]])))
    }
    annotated_ast
  } else {
    # here we only assume one SEXP
    list(classify_sexp(ast))
  }
}

classify_sexp <- function(sexp) {
  if (length(sexp) <= 1L) {
    sexp_chr <- as.character(sexp)
    if (is.call(sexp) && sexp_chr %in% c("input_matrix", "input_colvec")) {
      return(new_element_type(sexp_chr, sexp))
    }
    return(new_element_type("terminal", sexp))
  }
  first_element <- sexp[[1L]]
  first_element_chr <- as.character(first_element)
  annotated_sexp <- as.list(sexp)
  for (i in seq_along(annotated_sexp)[-1L]) {
    annotated_sexp[[i]] <- classify_sexp(annotated_sexp[[i]])
  }
  element_type <- "not_supported"
  meta_data <- list()
  element_type_map <- new.env(parent = emptyenv())
  element_type_map[["<-"]] <- "assignment"
  element_type_map[["{"]] <- "curley_bracket"
  element_type_map[["replace"]] <- "replace"
  element_type_map[["+"]] <- "plus"
  element_type_map[["-"]] <- "minus"
  element_type_map[["^"]] <- "pow"
  element_type_map[["*"]] <- "mult"
  element_type_map[["/"]] <- "div"
  element_type_map[["%*%"]] <- "matmul"
  element_type_map[["return"]] <- "return"
  element_type_map[["("]] <- "bracket"
  element_type_map[["backsolve"]] <- "backsolve"
  element_type_map[["forwardsolve"]] <- "forwardsolve"
  element_type_map[["if"]] <- "if"
  element_type_map[["for"]] <- "for"
  if (!is.null(element_type_map[[first_element_chr]])) {
    element_type <- element_type_map[[first_element_chr]]
  }
  if (!is.null(unary_function_mapping[[first_element_chr]])) {
    element_type <- "simple_unary_function"
    meta_data <- list(armadillo_fun = unary_function_mapping[[first_element_chr]])
  }
  if (!is.null(binary_function_mapping[[first_element_chr]])) {
    element_type <- "simple_binary_function"
    meta_data <- list(armadillo_fun = binary_function_mapping[[first_element_chr]])
  }
  new_element_type(element_type, sexp, annotated_sexp, meta_data)
}

new_element_type <- function(type, original_sexp, annotated_sexp = original_sexp, meta_data = list()) {
  structure(list(
    original_sexp = original_sexp,
    annotated_sexp = annotated_sexp,
    type = type,
    meta_data = meta_data
  ),
  class = paste0("annotated_element_", type)
  )
}

# functions that we simply translate to armadillo

binary_function_mapping <- new.env(hash = TRUE, parent = emptyenv())
binary_function_mapping$solve <- "arma::solve"

unary_function_mapping <- new.env(hash = TRUE, parent = emptyenv())
unary_function_mapping$t <- "arma::trans"
unary_function_mapping$exp <- "arma::exp"
unary_function_mapping$abs <- "arma::abs"
unary_function_mapping$log <- "arma::log"
unary_function_mapping$sum <- "arma::accu"
unary_function_mapping$diag <- "arma::diagmat"
unary_function_mapping$sqrt <- "arma::sqrt"
unary_function_mapping$square <- "arma::square"
unary_function_mapping$inv <- "arma::inv"
unary_function_mapping$sort <- "arma::sort"
unary_function_mapping$unique <- "arma::unique"
unary_function_mapping$cos <- "arma::cos"
unary_function_mapping$acos <- "arma::acos"
unary_function_mapping$cosh <- "arma::cosh"
unary_function_mapping$acosh <- "arma::acosh"
unary_function_mapping$sin <- "arma::sin"
unary_function_mapping$asin <- "arma::asin"
unary_function_mapping$sinh <- "arma::sinh"
unary_function_mapping$asinh <- "arma::asinh"
unary_function_mapping$tan <- "arma::tan"
unary_function_mapping$atan <- "arma::atan"
unary_function_mapping$tanh <- "arma::tanh"
unary_function_mapping$atanh <- "arma::atanh"
unary_function_mapping$cumsum <- "arma::cumsum"
unary_function_mapping$cumprod <- "arma::cumprod"
