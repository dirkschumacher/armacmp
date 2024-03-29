test_that("some basic checks", {
  a_lot_of_fun <- function(x, y = type_matrix()) {
    return(t(x) %*% y)
  }
  expect_silent(translate(a_lot_of_fun, "wat")$cpp_code)
})

test_that("it fails if an element is not supported", {
  a_lot_of_fun <- function(x, y = type_matrix()) {
    return(asodied(x^10))
  }
  expect_error(translate(a_lot_of_fun, "wat")$cpp_code,
    regexp = "asodied"
  )
})

test_that("it fails if two different return types", {
  a_lot_of_fun <- function(x, y = type_matrix()) {
    return(10, type = type_scalar_integer())
    return(10, type = type_scalar_numeric())
  }
  expect_error(translate(a_lot_of_fun, "wat")$cpp_code,
    regexp = "return"
  )
})

test_that("you can reassign variables", {
  a_lot_of_fun <- function(x) {
    x2 <- x
    x2 <- x
    return(x2)
  }
  code <- translate(a_lot_of_fun, "wat")$cpp_code
  expect_true(grepl("x2 = x", code, fixed = TRUE))
})

test_that("solve accepts 1 and 2 arguments", {
  code <- translate(function(X) {
    return(solve(X))
  }, "wat")$cpp_code
  expect_true(
    grepl(
      "arma::inv(X)",
      code,
      fixed = TRUE
    )
  )
  code <- translate(function(X, y) {
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
    translate(function(X, y) {
      return(solve(X, y, X))
    }, "wat")
  )
})

test_that("arma element wise mult only when arma is used", {
  code <- translate(function(x = type_scalar_numeric(), y = type_scalar_numeric()) {
    return(x * y, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("x * y", code, fixed = TRUE)
  )
})

test_that("multi dispatch of arma and std functions", {
  code <- translate(function(x, y = type_scalar_numeric()) {
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
  code <- translate(function(y = type_scalar_numeric()) {
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
  code <- translate(function(X = type_matrix()) {
    X2 <- crossprod(X)
    X3 <- crossprod(X, X2)
    return(X3)
  }, "wat")$cpp_code
  expect_true(
    grepl("arma::trans(X) * (X)", code, fixed = TRUE)
  )
  expect_true(
    grepl("arma::trans(X) * (X2)", code, fixed = TRUE)
  )
})

test_that("determinants work", {
  code <- translate(function(X = type_matrix()) {
    return(det(X))
  }, "wat")$cpp_code
  expect_true(
    grepl("arma::det(X)", code, fixed = TRUE)
  )
})

test_that("access individual elements", {
  code <- translate(function(x) {
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
  expect_true(
    grepl("wat(const arma::mat& x)", code, fixed = TRUE)
  )
})

test_that("set individual elements", {
  code <- translate(function(X) {
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
  code <- translate(function(x) {
    i <- 0
    for (i in seq_len(10)) {
      if (i < 5) {
        next
      }
      if (i > 8) break
      i <- i + 1
    }
    return(i, type = type_scalar_integer())
  }, "wat")$cpp_code
  expect_true(
    grepl("break;", code, fixed = TRUE)
  )
  expect_true(
    grepl("continue;", code, fixed = TRUE)
  )
})

test_that("proper double representation for integer like numerics", {
  code <- translate(function(x) {
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
  code <- translate(function(x, y) {
    X <- tcrossprod(x)
    X <- tcrossprod(x, y)
    return(X)
  }, "wat")$cpp_code
  expect_true(
    grepl("(x) * arma::trans(x)", code, fixed = TRUE)
  )
  expect_true(
    grepl("(x) * arma::trans(y)", code, fixed = TRUE)
  )
})

test_that("element-wise multiplication only for arma types", {
  code <- translate(function(x, y) {
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
    translate(function() {
      return(5)
    }, "wat")
  )
  fun <- function() {
    return(5)
  }
  expect_silent(
    translate(fun, "wat")
  )
})

test_that("test lambdas", {
  code <- translate(function(X) {
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
  code <- translate(function(x = type_scalar_numeric(), X) {
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
  code <- translate(function() {
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
    translate(function() {
      x <- 0
      for (k in seq_len(10L)) x <- k + 1
      return(x, type = type_scalar_numeric())
    }, "wat")
  )
})

test_that("while loops are supported", {
  code <- translate(function() {
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
  code <- translate(function() {
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
  code <- translate(function() {
    fun <- function(lo = type_scalar_integer(), hi = type_scalar_integer()) {
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
  code <- translate(function(X) {
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
  code <- translate(function(X) {
    return(1L:10L)
  }, "wat")$cpp_code
  expect_true(
    grepl("return arma::linspace<arma::colvec>(1, 10, 10 - 1 + 1);", code, fixed = TRUE)
  )
})

test_that("ncol and nrow have the right type", {
  code <- translate(function(X) {
    return(ncol(X))
  }, "wat")$cpp_code
  expect_true(
    grepl("int wat(", code, fixed = TRUE)
  )
  code <- translate(function(X) {
    return(nrow(X))
  }, "wat")$cpp_code
  expect_true(
    grepl("int wat(", code, fixed = TRUE)
  )
})

test_that("type propagation for log reg", {
  # in parts from Arnold, T., Kane, M., & Lewis, B. W. (2019). A Computational Approach to Statistical Learning. CRC Press.
  code <- translate(function(X, y = type_colvec()) {
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
  code <- translate(function(X) {
    x <- 1
    return(x)
  }, "wat")$cpp_code
  expect_true(
    grepl("double wat(", code, fixed = TRUE)
  )

  code <- translate(function(X) {
    x <- 1L
    return(x)
  }, "wat")$cpp_code
  expect_true(
    grepl("int wat(", code, fixed = TRUE)
  )

  code <- translate(function(X) {
    x <- TRUE
    return(x)
  }, "wat")$cpp_code
  expect_true(
    grepl("bool wat(", code, fixed = TRUE)
  )
})

test_that("errors if type annotation is needed", {
  expect_error(translate(function(X) {
    return(1 + 1 + 1) # not yet possible
  }, "wat"), "annotation")
})

test_that("type of lambdas can be deduced: example #42", {
  code <- translate(function(X) {
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

test_that("lin reg example works", {
  # from Arnold, T., Kane, M., & Lewis, B. W. (2019). A Computational Approach to Statistical Learning. CRC Press.
  expect_silent(
    translate(function(X, y = type_colvec()) {
      qr_res <- qr(X)
      qty <- t(qr.Q(qr_res)) %*% y
      beta_hat <- backsolve(qr.R(qr_res), qty)
      return(beta_hat, type = type_colvec())
    }, "wat")
  )
})

test_that("reassignments of parameters trigger copys", {
  code <- translate(function(X, Y, Z) {
    fun <- function() {
      X <- X + 1
    }
    fun()
    Y[5] <- 10
    return(X, type = type_matrix())
  }, "wat")$cpp_code
  expect_true(
    grepl("arma::mat X", code, fixed = TRUE)
  )
  expect_true(
    grepl("arma::mat Y", code, fixed = TRUE)
  )
  expect_true(
    grepl("const arma::mat& Z", code, fixed = TRUE)
  )
})

test_that("for loops without a loop variable is deterministic", {
  fun <- function() {
    x <- 0
    for (k in seq_len(10L)) {
      x <- x + 1
    }
    return(x, type = type_scalar_numeric())
  }
  code1 <- translate(fun, "wat")$cpp_code
  code2 <- translate(fun, "wat")$cpp_code
  expect_equal(code1, code2)
})

test_that("test assign with '='", {
  assign2 <- function(x) {
    x2 = x
    return(x2)
  }
  code <- translate(assign2, "wat")$cpp_code
  expect_true(grepl("x2 = x", code, fixed = TRUE))
})

test_that("we can return lists", {
  code <- translate(function(X) {
    x <- 1
    return(list(x, 1))
  }, "wat")$cpp_code
  expect_true(
    grepl("Rcpp::List wat(", code, fixed = TRUE)
  )
})

test_that("subviews for col/row selection are supported", {
  code <- translate(function(X) {
    col <- 1
    X[col, ] <- t(X[, col])
    return(X)
  }, "wat")$cpp_code
  expect_true(
    grepl("X.row(col - 1) = arma::trans(X.col(col - 1));", code, fixed = TRUE)
  )
})

test_that("subviews have typle matrix", {
  code <- translate(function(X) {
    col <- 1
    a <- sum(X[, col] * X[1, ])
    return(a, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("X.col(col - 1) % X.row(1.0 - 1)", code, fixed = TRUE)
  )
})

test_that("min/max are supported", {
  code <- translate(function(X) {
    col_min <- min(X[, 1])
    row_max <- max(X[1, ])
    return(col_min + row_max, type = type_scalar_numeric())
  }, "wat")$cpp_code
  expect_true(
    grepl("arma::min(X.col", code, fixed = TRUE) &&
      grepl("arma::max(X.row", code, fixed = TRUE)
  )
})
