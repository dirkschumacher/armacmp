test_that("some basic checks", {
  a_lot_of_fun <- function(x, y = type_matrix()) {
    return(t(x) %*% y)
  }
  expect_silent(armacmp_compile(a_lot_of_fun, "wat")$cpp_code)
})

test_that("it fails if an element is not supported", {
  a_lot_of_fun <- function(x, y = type_matrix()) {
    return(asodied(x^10))
  }
  expect_error(armacmp_compile(a_lot_of_fun, "wat")$cpp_code,
    regexp = "asodied"
  )
})

test_that("it fails if two different return types", {
  a_lot_of_fun <- function(x, y = type_matrix()) {
    return(10, type = type_scalar_int())
    return(10, type = type_scalar_numeric())
  }
  expect_error(armacmp_compile(a_lot_of_fun, "wat")$cpp_code,
    regexp = "return"
  )
})

test_that("you can reassign variables", {
  a_lot_of_fun <- function(x) {
    x2 <- x
    x2 <- x
    return(x2)
  }
  code <- armacmp_compile(a_lot_of_fun, "wat")$cpp_code
  expect_true(grepl("x2 = x", code, fixed = TRUE))
})

test_that("solve accepts 1 and 2 arguments", {
  code <- armacmp_compile(function(X) {
    return(solve(X))
  }, "wat")$cpp_code
  expect_true(
    grepl(
      "arma::inv(X)",
      code,
      fixed = TRUE
    )
  )
  code <- armacmp_compile(function(X, y) {
    return(solve(X, y))
  }, "wat")$cpp_code
  expect_true(
    grepl(
      "arma::solve(X, y)",
      code,
      fixed = TRUE
    )
  )
  expect_error(
    armacmp_compile(function(X, y) {
      return(solve(X, y, X))
    }, "wat")
  )
})

test_that("arma element wise mult only when arma is used", {
  code <- armacmp_compile(function(x = type_scalar_numeric(), y = type_scalar_numeric()) {
    return(x * y, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("x * y", code, fixed = TRUE)
  )
})

test_that("multi dispatch of arma and std functions", {
  code <- armacmp_compile(function(x, y = type_scalar_numeric()) {
    X <- exp(x)
    Y <- exp(y)
    return(1, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("auto Y = std::exp(y)", code, fixed = TRUE)
  )
  expect_true(
    grepl("arma::mat X = arma::exp(x)", code, fixed = TRUE)
  )
})

test_that("generic range-based for loops work", {
  code <- armacmp_compile(function(y = type_scalar_numeric()) {
    iter <- seq_len(y)
    x <- 1
    for (i in iter) {
      x <- x + i
    }
    return(x, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("for (const auto& i : iter)", code, fixed = TRUE)
  )
  expect_true(
    grepl("arma::colvec iter = arma::linspace<arma::colvec>(1, y, y)", code, fixed = TRUE)
  )
})

test_that("generic range-based for loops work", {
  code <- armacmp_compile(function(X = type_matrix()) {
    X2 <- crossprod(X)
    X3 <- crossprod(X, X2)
    return(X3)
  }, "wat")$cpp_code
  expect_true(
    grepl("arma::trans(X) * X", code, fixed = TRUE)
  )
  expect_true(
    grepl("arma::trans(X) * X2", code, fixed = TRUE)
  )
})

test_that("determinants work", {
  code <- armacmp_compile(function(X = type_matrix()) {
    return(det(X))
  }, "wat")$cpp_code
  expect_true(
    grepl("arma::det(X)", code, fixed = TRUE)
  )
})

test_that("access individual elements", {
  code <- armacmp_compile(function(x) {
    return(x[1, 2] - x[1]^2 - x[1, 1]^2, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("x(1.0 - 1, 2.0 - 1)", code, fixed = TRUE)
  )
  expect_true(
    grepl("std::pow(x(1.0 - 1), 2.0)", code, fixed = TRUE)
  )
  expect_true(
    grepl("std::pow(x(1.0 - 1, 1.0 - 1), 2.0)", code, fixed = TRUE)
  )
})

test_that("set individual elements", {
  code <- armacmp_compile(function(X) {
    X2 <- X
    X2[1] <- 5
    X2[1, 2] <- 10
    return(X2)
  }, "wat")$cpp_code
  expect_true(
    grepl("X2(1.0 - 1) = 5.0", code, fixed = TRUE)
  )
  expect_true(
    grepl("X2(1.0 - 1, 2.0 - 1) = 10.0", code, fixed = TRUE)
  )
})

test_that("access individual elements", {
  code <- armacmp_compile(function(x) {
    i <- 0
    for (i in seq_len(10)) {
      if (i < 5) {
        next
      }
      if (i > 8) break
      i <- i + 1
    }
    return(i, type = type_scalar_int())
  }, "wat")$cpp_code
  expect_true(
    grepl("break;", code, fixed = TRUE)
  )
  expect_true(
    grepl("continue;", code, fixed = TRUE)
  )
})

test_that("proper double representation for integer like numerics", {
  code <- armacmp_compile(function(x) {
    y1 <- 2
    y2 <- 2.1
    y3 <- 2L
    return(y, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("y1 = 2.0;", code, fixed = TRUE)
  )
  expect_true(
    grepl("y2 = 2.1;", code, fixed = TRUE)
  )
  expect_true(
    grepl("y3 = 2;", code, fixed = TRUE)
  )
})

test_that("tcrossprod is supported", {
  code <- armacmp_compile(function(x, y) {
    X <- tcrossprod(x)
    X <- tcrossprod(x, y)
    return(X)
  }, "wat")$cpp_code
  expect_true(
    grepl("x * arma::trans(x)", code, fixed = TRUE)
  )
  expect_true(
    grepl("x * arma::trans(y)", code, fixed = TRUE)
  )
})

test_that("element-wise multiplication only for arma types", {
  code <- armacmp_compile(function(x, y) {
    X <- x * 2
    Y <- x * y
    return(X)
  }, "wat")$cpp_code
  expect_true(
    grepl("x * 2.0", code, fixed = TRUE)
  )
  expect_true(
    grepl("x % y", code, fixed = TRUE)
  )
})

test_that("function can have 0 arguments", {
  expect_silent(
    armacmp_compile(function() {
      return(5)
    }, "wat")
  )
  fun <- function() {
    return(5)
  }
  expect_silent(
    armacmp_compile(fun, "wat")
  )
})

test_that("test lambdas", {
  code <- armacmp_compile(function(X) {
    square <- function(y) {
      return(y^2)
    }
    Y <- square(X)
    return(Y)
  }, "wat")$cpp_code
  expect_true(
    grepl("auto square = [&](const arma::mat& y) mutable -> arma::mat", code, fixed = TRUE)
  )
  expect_true(
    grepl("arma::mat Y = square(X)", code, fixed = TRUE)
  )
})

test_that("type decution works with lambdas", {
  code <- armacmp_compile(function(x = type_scalar_numeric(), X) {
    fun <- function(y = type_scalar_numeric()) {
      x2 <- x + y
      return(x2 + 10)
    }
    fun2 <- function() {
      return(X + 1)
    }
    fun3 <- function() return(X + 1)
    x3 <- fun(20)
    x4 <- fun2()
    x5 <- fun3()
    return(fun(50), type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("auto x2 = x + y", code, fixed = TRUE)
  )
  expect_true(
    grepl("arma::mat x5", code, fixed = TRUE)
  )
  expect_true(
    grepl("arma::mat x4", code, fixed = TRUE)
  )
  expect_true(
    grepl("auto x3", code, fixed = TRUE)
  )
  expect_true(
    grepl("auto fun3 = [&]() mutable -> arma::mat", code, fixed = TRUE)
  )
  expect_true(
    grepl("auto fun2 = [&]() mutable -> arma::mat", code, fixed = TRUE)
  )
  expect_true(
    grepl("auto fun = [&](double y)", code, fixed = TRUE)
  )
  expect_true(
    grepl("auto x3 = fun(20.0)", code, fixed = TRUE)
  )
})

test_that("element-wise multiplications only used for arma::types in loops", {
  code <- armacmp_compile(function() {
    for (k in seq_len(10L)) {
      t <- k * k
    }
    return(1)
  }, "wat")$cpp_code
  expect_true(
    grepl("auto t = k * k", code, fixed = TRUE)
  )
})

test_that("for loops without blocks work", {
  expect_silent(
    armacmp_compile(function() {
      x <- 0
      for (k in seq_len(10L)) x <- k + 1
      return(x, type = type_scalar_numeric())
    }, "wat")
  )
})

test_that("while loops are supported", {
  code <- armacmp_compile(function() {
    x <- 0
    while (x < 10) {
      y <- 5
      while (y > 0.1) y <- y / 10
      x <- x + y
    }
    return(x, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("y = y / 10.0", code, fixed = TRUE)
  )
  expect_true(
    grepl("while (y > 0.1)\n{", code, fixed = TRUE)
  )
  expect_true(
    grepl("while (x < 10.0)\n{", code, fixed = TRUE)
  )
})

test_that("lambdas are mutable by default and do not need to have a return", {
  code <- armacmp_compile(function() {
    x <- 1
    fun <- function(y = type_scalar_numeric()) {
      x <- x + y
    }
    fun()
    return(x, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("mutable", code, fixed = TRUE)
  )
  expect_true(
    grepl("fun();", code, fixed = TRUE)
  )
})

test_that("lambdas are mutable by default and do not need to have a return", {
  code <- armacmp_compile(function() {
    fun <- function(lo = type_scalar_int(), hi = type_scalar_int()) {
      i <- lo
      j <- hi
      while (0 == 0) {
        i <- floor(lo / 2)
        lo <- j
      }
      i <- lo
    }
    return(x, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("\ni = std::floor(lo / 2.0);", code, fixed = TRUE)
  )
  expect_true(
    grepl("auto i = lo;", code, fixed = TRUE)
  )
})

test_that("recursive lambdas are detected and work", {
  code <- armacmp_compile(function(X) {
    fun <- function(x = type_scalar_numeric(), X) {
      if (x == 0) {
        return(0, type = type_scalar_numeric())
      }
      return(fun(x - 1, X), type = type_scalar_numeric())
    }
    return(fun(5, X), type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("std::function<double(double, const arma::mat&)> fun;", code, fixed = TRUE)
  )
})

test_that("sequences with the colon operator", {
  code <- armacmp_compile(function(X) {
    return(1L:10L)
  }, "wat")$cpp_code
  expect_true(
    grepl("return arma::linspace<arma::colvec>(1, 10, 10 - 1 + 1);", code, fixed = TRUE)
  )
})

test_that("ncol and nrow have the right type", {
  code <- armacmp_compile(function(X) {
    return(ncol(X))
  }, "wat")$cpp_code
  expect_true(
    grepl("int wat(", code, fixed = TRUE)
  )
  code <- armacmp_compile(function(X) {
    return(nrow(X))
  }, "wat")$cpp_code
  expect_true(
    grepl("int wat(", code, fixed = TRUE)
  )
})

test_that("type propagation for log reg", {
  code <- armacmp_compile(function(X, y = type_colvec()) {
    beta <- rep.int(0, ncol(X))
    b_old <- beta
    alpha <- X %*% beta
    p <- 1 / (1 + exp(-alpha))
    W <- p * (1 - p)
    XtX <- crossprod(X, diag(W) %*% X)
    score <- t(X) %*% (y - p)
    delta <- solve(XtX, score)
    return(beta)
  }, "wat")$cpp_code

  # no auto
  expect_false(
    grepl("auto", code, fixed = TRUE)
  )
})

test_that("type deduction works for scales", {
  code <- armacmp_compile(function(X) {
    x <- 1
    return(x)
  }, "wat")$cpp_code
  expect_true(
    grepl("double wat(", code, fixed = TRUE)
  )

  code <- armacmp_compile(function(X) {
    x <- 1L
    return(x)
  }, "wat")$cpp_code
  expect_true(
    grepl("int wat(", code, fixed = TRUE)
  )

  code <- armacmp_compile(function(X) {
    x <- TRUE
    return(x)
  }, "wat")$cpp_code
  expect_true(
    grepl("bool wat(", code, fixed = TRUE)
  )
})

test_that("errors if type annotation is needed", {
  expect_error(armacmp_compile(function(X) {
    return(1 + 1 + 1) # not yet possible
  }, "wat"), "annotation")
})

test_that("type of lambdas can be deduced: example #42", {
  code <- armacmp_compile(function(X) {
    square <- function(y) {
      return(y^2)
    }
    Y <- square(X)
    return(Y)
  }, "wat")$cpp_code
  expect_true(
    grepl("arma::mat Y = square(X)", code, fixed = TRUE)
  )
  expect_true(
    grepl("arma::mat wat(", code, fixed = TRUE)
  )
})
