
#' Simulated-Annealing with exponential schedule
#'
#' @export
optimizer_SA <- function() {
  cpp_init_code <- paste0(
    "ens::ExponentialSchedule expSchedule;\n",
    "ens::SA<> optimizer(expSchedule);\n"
  )
  cpp_init_code
}

#' L-BFGS Optimizer
#'
#' @export
optimizer_L_BFGS <- function() {
  "ens::L_BFGS optimizer;\n"
}

#' Gradient Descent Optimizer
#'
#' @export
optimizer_GradientDescent <- function() {
  "ens::GradientDescent optimizer;\n"
}
