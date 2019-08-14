#' Type matrix
#' @export
type_matrix <- function() {
  structure(
    "arma::mat",
    class = "type_matrix"
  )
}

#' Type colvec
#' @export
type_colvec <- function() {
  structure(
    "arma::colvec",
    class = "type_colvec"
  )
}

#' Type rowvec
#' @export
type_rowvec <- function() {
  structure(
    "arma::rowvec",
    class = "type_rowvec"
  )
}


#' Type int
#' @export
type_scalar_integer <- function() {
  structure(
    "int",
    class = "type_scalar_integer"
  )
}

#' Type numeric
#' @export
type_scalar_numeric <- function() {
  structure(
    "double",
    class = "type_scalar_numeric"
  )
}

#' Type logical
#' @export
type_scalar_logical <- function() {
  structure(
    "bool",
    class = "type_scalar_logical"
  )
}
