test_that("crossprod", {
  skip_on_cran()
  crossprod2 <- armacmp(function(X = type_matrix(), Y = type_matrix()) {
    return(t(X) %*% Y)
  })
  x <- matrix(1:1000)
  expect_equal(crossprod2(x, x), crossprod(x, x))
})

test_that("inverse", {
  skip_on_cran()
  inverse <- armacmp(function(X) {
    return(solve(X), type = type_matrix())
  })
  x <- matrix(runif(100), ncol = 10)
  expect_equal(inverse(x), solve(x))
})

test_that("for loop", {
  for_loop <- armacmp(function(X, offset = type_scalar_numeric()) {
    X_new <- X
    # only seq_len is currently supported
    for (i in seq_len(10 + 10)) {
      # use = to update an existing variable
      X_new = log(t(X_new) %*% X_new + i + offset)
    }
    return(X_new)
  })

  for_loop_r <- function(X, offset) {
    X_new <- X
    for (i in seq_len(10 + 10)) {
      X_new <- log(t(X_new) %*% X_new + i + offset)
    }
    return(X_new)
  }

  expect_equal(
    for_loop_r(matrix(as.numeric(1:100), ncol = 10), offset = 10),
    for_loop(matrix(as.numeric(1:100), ncol = 10), offset = 10)
  )
})

test_that("if", {
  if_clause <- armacmp(function(X) {
    # infers that test needs to be bool (auto)
    test <- sum(log(X)) < 10
    if (test) {
      return((t(X) %*% X) + 10)
    } else if (sum(X) < 10) {
      return(t(X) %*% X)
    } else {
      return((t(X) %*% X) + 10)
    }
  })

  if_clause_r <- function(X) {
    test <- sum(log(X)) < 10
    if (test) {
      return((t(X) %*% X) + 10)
    } else if (sum(X) < 10) {
      return(t(X) %*% X)
    } else {
      return(t(X) %*% X + 10)
    }
  }

  X <- matrix(1:100)
  expect_equal(
    if_clause_r(X),
    if_clause(X)
  )
})

test_that("qr", {
  qr2 <- armacmp(function(X) {
    x <- qr(X)
    return(qr.Q(x) %*% qr.R(x))
  })
  X <- matrix(runif(100), ncol = 10)
  x <- qr(X)
  expect_equal(
    qr2(X),
    qr.Q(x) %*% qr.R(x)
  )
})
