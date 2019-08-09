#' Type matrix
#' @export
type_matrix <- function() {
  structure(
    list(cpp_type = "arma::mat"),
    class = "type_matrix"
  )
}

#' Type colvec
#' @export
type_colvec <- function() {
  structure(
    list(cpp_type = "arma::colvec"),
    class = "type_colvec"
  )
}

#' Type rowvec
#' @export
type_rowvec <- function() {
  structure(
    list(cpp_type = "arma::rowvec"),
    class = "type_rowvec"
  )
}


#' Type int
#' @export
type_scalar_int <- function() {
  structure(
    list(cpp_type = "int"),
    class = "type_scalar_int"
  )
}

#' Type numeric
#' @export
type_scalar_numeric <- function() {
  structure(
    list(cpp_type = "double"),
    class = "type_scalar_numeric"
  )
}

#' Type logical
#' @export
type_scalar_logical <- function() {
  structure(
    list(cpp_type = "bool"),
    class = "type_scalar_logical"
  )
}
