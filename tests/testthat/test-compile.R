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

# test_that("namespaced functions", {
#   code <- armacmp_compile(function(X, y = type_scalar_numeric()) {
#     return(arma::sqrt(X) + std::pow(y, 2))
#   }, "wat")$cpp_code
#   expect_true(
#     grepl("std::pow(y, 2)", code, fixed = TRUE)
#   )
#   expect_true(
#     grepl("arma::sqrt(X)", code, fixed = TRUE)
#   )
# })

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
    grepl("auto iter = Rcpp::seq_len(y)", code, fixed = TRUE)
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
    return(x[1, 2] - x[1]^2 - x[1, 1]^2)
  }, "wat")$cpp_code
  expect_true(
    grepl("x(1 - 1, 2 - 1)", code, fixed = TRUE)
  )
  expect_true(
    grepl("std::pow(x(1 - 1), 2)", code, fixed = TRUE)
  )
  expect_true(
    grepl("std::pow(x(1 - 1, 1 - 1), 2)", code, fixed = TRUE)
  )
})
