test_that("SA works", {
  optimize <- arma_optim(
    data = list(),
    evaluate = function(x) {
      return(2 * norm(x)^2)
    },
    optimizer = optimizer_SA()
  )

  result <- optimize(matrix(c(1, -1, 1), ncol = 1))
  expect_true(sum(result) < 0.1)
})

test_that("CNE works", {
  optimize <- arma_optim(
    data = list(),
    evaluate = function(x) {
      return(2 * norm(x)^2)
    },
    optimizer = optimizer_CNE()
  )

  result <- optimize(matrix(c(1, -1, 1), ncol = 1))
  expect_true(sum(result) < 0.1)
})

test_that("L_BFGS works", {
  optimize_lbfgs <- arma_optim(
    data = list(design_matrix = type_matrix(), response = type_colvec()),
    evaluate = function(beta) {
      return(norm(response - design_matrix %*% beta)^2)
    },
    gradient = function(beta) {
      return(-2 %*% t(design_matrix) %*% (response - design_matrix %*% beta))
    },
    optimizer = optimizer_L_BFGS()
  )

  # this example is taken from the RcppEnsmallen package
  # https://github.com/coatless/rcppensmallen/blob/master/src/example-linear-regression-lbfgs.cpp
  n <- 1e4
  beta <- c(-2, 1.5, 3, 8.2, 6.6)
  p <- length(beta)
  X <- cbind(1, matrix(rnorm(n), ncol = p - 1))
  y <- X %*% beta + rnorm(n / (p - 1))

  # Run optimization with lbfgs
  result <- optimize_lbfgs(
    design_matrix = X,
    response = y,
    beta = matrix(runif(p), ncol = 1)
  )
  expect_equal(as.numeric(result), beta, tolerance = 0.1)
})

test_that("rosenbrock works", {
  rosenbrock <- arma_optim(
    data = list(),
    evaluate = function(x) {
      return(100 * (x[2] - x[1]^2)^2 + (1 - x[1])^2)
    },
    optimizer = optimizer_SA()
  )

  result <- rosenbrock(matrix(c(-23, 50), nrow = 1))
  #optimal value at 1,1
  expect_equal(abs(sum(result) - 2) < 0.5)
})
