test_that("crossprod", {
  crossprod2 <- compile(function(X = type_matrix(), Y = type_matrix()) {
    return(t(X) %*% Y)
  })
  x <- matrix(1:1000)
  expect_equal(crossprod2(x, x), crossprod(x, x))
})

test_that("inverse", {
  inverse <- compile(function(X) {
    return(solve(X), type = type_matrix())
  })
  x <- matrix(runif(100), ncol = 10)
  expect_equal(inverse(x), solve(x))
})

test_that("for loop", {
  for_loop <- compile(function(X, offset = type_scalar_numeric()) {
    X_new <- X
    for (i in seq_len(10 + 10)) {
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
  if_clause <- compile(function(X) {
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
  qr2 <- compile(function(X) {
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
  chol2 <- compile(function(X) return(chol(X)))
  X <- matrix(c(5, 1, 1, 3), 2, 2)
  expect_equal(
    chol2(X),
    chol(X)
  )
})

test_that("nrow and ncol", {
  nrc <- compile(function(X) {
    return(nrow(X) + ncol(X), type = type_scalar_int())
  })
  X <- matrix(1:100, ncol = 10)
  expect_equal(nrc(X), 20)
})

test_that("scoping works", {
  expect_silent(
    fun <- compile(function(X) {
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

test_that("for loops only create loop variable if used", {
  expect_silent(fun <- compile(function() {
    x <- 10
    for (i in 1:10) {
      x <- x + 1
    }
    return(x, type = type_scalar_numeric())
  }))
  expect_equal(fun(), 20)
})

test_that("back and forwardsolve", {
  backsolve2 <- compile(function(x, y = type_colvec()) {
    return(backsolve(x, y))
  })
  forwardsolve2 <- compile(function(x, y = type_colvec()) {
    return(forwardsolve(x, y))
  })

  # example from the docs of backsolve
  r <- rbind(
    c(1, 2, 3),
    c(0, 1, 1),
    c(0, 0, 2)
  )
  x <- matrix(c(8, 4, 2))
  expect_equal(backsolve(r, x), backsolve2(r, x))
  expect_equal(forwardsolve(r, x), forwardsolve2(r, x))
})

test_that("cum* functions work", {
  cumprod_sum <- compile(function(x = type_colvec()) {
    return(cumprod(x) + cumsum(x))
  })

  x <- as.numeric(1:10)
  expect_equal(
    cumprod(x) + cumsum(x),
    as.numeric(cumprod_sum(x))
  )
})

test_that("colsums et al. work", {
  colSums2 <- compile(function(X) return(colSums(X)))
  rowSums2 <- compile(function(X) return(rowSums(X)))
  colMeans2 <- compile(function(X) return(colMeans(X)))
  rowMeans2 <- compile(function(X) return(rowMeans(X)))

  X <- matrix(1:100, ncol = 10)
  expect_equal(colSums(X), as.numeric(colSums2(X)))
  expect_equal(rowSums(X), as.numeric(rowSums2(X)))
  expect_equal(colMeans(X), as.numeric(colMeans2(X)))
  expect_equal(rowMeans(X), as.numeric(rowMeans2(X)))
})

test_that("QR decompositions", {
  qr2 <- compile(function(X) {
    qr_res <- qr(X)
    return(qr.Q(qr_res) %*% qr.R(qr_res))
  })

  qr_r <- function(X) {
    qr_res <- qr(X)
    return(qr.Q(qr_res) %*% qr.R(qr_res))
  }

  # example from the R docs of lm.fit
  X <- matrix(1:1000, ncol = 10)
  expect_equal(
    qr2(X),
    qr_r(X)
  )
})

test_that("rep.int generates a colvec", {
  fun <- compile(function() {
    x <- rep.int(1, 10)
    return(x + 10, type = type_colvec())
  })
  expect_equal(
    fun(),
    matrix(rep.int(1, 10), ncol = 1) + 10
  )
})

test_that("element access works with doubles", {
  fun <- compile(function(X) {
    X2 <- X
    X2[2] <- X2[1] + 13
    return(X2)
  })
  expect_equal(
    fun(matrix(c(1, 2))),
    matrix(c(1, 14))
  )
})

test_that("lambdas work", {
  expect_silent(
    compile(function(X, y = type_scalar_numeric(), z = type_colvec()) {
      log_fun <- function(el, e = type_scalar_numeric()) {
        return(log(el)^log(e))
      }
      mod <- function(cv) {
        offset <- function(o) return(o + 10, type = type_scalar_numeric())
        return(X + cv + offset(z))
      }
      return(mod(z) + log_fun(X, y))
    })
  )
})

test_that("boolean integration test", {
  fun <- compile(function(y = type_scalar_logical()) {
    z <- TRUE
    if (y) {
      z <- FALSE
    }
    return(z, type = type_scalar_logical())
  })
  expect_true(fun(FALSE))
  expect_false(fun(TRUE))
})

test_that("svd works", {
  X <- matrix(rnorm(100), ncol = 10)
  fun_r <- function(X) {
    s <- svd(X)
    x <- s$d
    y <- s$v
    D <- diag(s$d)
    X <- s$u %*% D %*% t(y)
    return(X)
  }
  fun <- compile(fun_r)
  expect_equal(
    fun(X),
    fun_r(X)
  )
})
