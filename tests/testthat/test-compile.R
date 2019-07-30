test_that("some basic checks", {
  a_lot_of_fun <- function(x, y = type_matrix()) {
    return(t(x) %*% y)
  }
  expect_silent(compile_to_str(a_lot_of_fun, "wat"))
})

test_that("it fails if an element is not supported", {
  a_lot_of_fun <- function(x, y = type_matrix()) {
    return(asodied(x^10))
  }
  expect_error(compile_to_str(a_lot_of_fun, "wat"),
               regexp = "asodied")
})

test_that("it fails if two different return types", {
  a_lot_of_fun <- function(x, y = type_matrix()) {
    return(10, type = type_scalar_int())
    return(10, type = type_scalar_numeric())
  }
  expect_error(compile_to_str(a_lot_of_fun, "wat"),
               regexp = "return")
})
