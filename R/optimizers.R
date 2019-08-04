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
#'
#' @param numBasis Number of memory points to be stored (default 10)
#' @param maxIterations Maximum number of iterations for the optimization (0 means no limit and may run indefinitely)
#' @param armijoConstant Controls the accuracy of the line search routine for determining the Armijo condition
#' @param wolfe Parameter for detecting the Wolfe condition
#' @param minGradientNorm Minimum gradient norm required to continue the optimization
#' @param factr Minimum relative function value decrease to continue the optimization
#' @param maxLineSearchTrials The maximum number of trials for the line search (before giving up)
#' @param minStep The minimum step of the line search
#' @param maxStep The maximum step of the line search
#'
#' @export
optimizer_L_BFGS <- function(numBasis = 10,
                             maxIterations = 10000,
                             armijoConstant = 1e-4,
                             wolfe = 0.9,
                             minGradientNorm = 1e-6,
                             factr = 1e-15,
                             maxLineSearchTrials = 50,
                             minStep = 1e-20,
                             maxStep = 1e20) {
  stopifnot(
    is.numeric(numBasis),
    is.numeric(maxIterations),
    is.numeric(armijoConstant),
    is.numeric(wolfe),
    is.numeric(minGradientNorm),
    is.numeric(factr),
    is.numeric(maxLineSearchTrials),
    is.numeric(minStep),
    is.numeric(maxStep)
  )
  paste0(
    "ens::L_BFGS optimizer(",
    numBasis, ", ",
    maxIterations, ", ",
    armijoConstant, ", ",
    wolfe, ", ",
    minGradientNorm, ", ",
    factr, ", ",
    maxLineSearchTrials, ", ",
    minStep, ", ",
    maxStep,
    ");\n"
  )
}

#' Gradient Descent Optimizer
#'
#' @param stepSize Step size for each iteration
#' @param maxIterations Maximum number of iterations allowed (0 means no limit).
#' @param tolerance Maximum absolute tolerance to terminate algorithm.
#' @export
optimizer_GradientDescent <- function(stepSize = 0.01, maxIterations = 100000, tolerance = 1e-5) {
  stopifnot(stepSize > 0, maxIterations >= 0, tolerance >= 0)
  paste0(
    "ens::GradientDescent optimizer(", stepSize, ", ", maxIterations, ", ", tolerance, ");\n"
  )
}

#' Conventional Neural Evolution Optimizer
#' @param populationSize The number of candidates in the population. This should be at least 4 in size 	500
#' @param maxGenerations The maximum number of generations allowed for CNE 	5000
#' @param mutationProb Probability that a weight will get mutated 	0.1
#' @param mutationSize The range of mutation noise to be added. This range is between 0 and mutationSize	0.02
#' @param selectPercent The percentage of candidates to select to become the the next generation	0.2
#' @param tolerance The final value of the objective function for termination. If set to negative value, tolerance is not considered 	1e-5
#'
#' @export
optimizer_CNE <- function(populationSize = 500,
                          maxGenerations = 5000,
                          mutationProb = 0.1,
                          mutationSize = 0.02,
                          selectPercent = 0.2,
                          tolerance = 1e-5) {
  paste0(
    "ens::CNE optimizer(",
    populationSize, ", ",
    maxGenerations, ", ",
    mutationProb, ", ",
    mutationSize, ", ",
    selectPercent, ", ",
    tolerance,
    ");\n"
  )
}
