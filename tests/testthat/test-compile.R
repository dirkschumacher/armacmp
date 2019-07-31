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
      "arma::inv( X )",
      code,
      fixed = TRUE
    )
  )
  code <- armacmp_compile(function(X, y) {
    return(solve(X, y))
  }, "wat")$cpp_code
  expect_true(
    grepl(
      "arma::solve( X, y )",
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
