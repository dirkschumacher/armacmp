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
      X_new <- log(t(X_new) %*% X_new + i + offset)
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

test_that("chol", {
  chol2 <- armacmp(function(X) return(chol(X)))
  X <- matrix(c(5, 1, 1, 3), 2, 2)
  expect_equal(
    chol2(X),
    chol(X)
  )
})

test_that("nrow and ncol", {
  nrc <- armacmp(function(X) {
    return(nrow(X) + ncol(X), type = type_scalar_int())
  })
  X <- matrix(1:100, ncol = 10)
  expect_equal(nrc(X), 20)
})

test_that("scoping works", {
  expect_silent(
    fun <- armacmp(function(X) {
      x <- X
      y <- 10
      test <- 5 < 10
      if (test) {
        offset <- 20
        y <- offset
      }
      {
        offset <- 40
        y <- offset
      }
      offset <- 30
      y <- offset
      x <- (x %*% 2) + 1
      return(x + y)
    })
  )
  fun2 <- function(X) {
    x <- X
    y <- 10
    test <- 5 < 10
    if (test) {
      offset <- 20
      y <- offset
    }
    {
      offset <- 40
      y <- offset
    }
    offset <- 30
    y <- offset
    x <- (x %*% 2) + 1
    return(x + y)
  }
  expect_equal(
    fun(matrix(1:10)),
    fun2(matrix(1:10))
  )
})
