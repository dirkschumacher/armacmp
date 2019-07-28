test_that("some quick checks", {
  x <- classify_sexp(quote(x <- input_matrix()))
  expect_true(x$type == "assignment")
  expect_true(x$annotated_sexp[[3L]]$type == "input_matrix")

  x <- classify_sexp(quote(input_matrix()))
  expect_true(x$type == "input_matrix")

  x <- classify_sexp(quote(return(x)))
  expect_true(x$type == "return")
})

test_that("supports simple operators", {
  x <- classify_sexp(quote(x * x))
  expect_true(x$type == "mult")
  x <- classify_sexp(quote(x / x))
  expect_true(x$type == "div")
  x <- classify_sexp(quote(x - x))
  expect_true(x$type == "minus")
  x <- classify_sexp(quote(x + x))
  expect_true(x$type == "plus")
  x <- classify_sexp(quote(-x))
  expect_true(x$type == "minus")
})

test_that("it supports brackets", {
  x <- classify_sexp(quote(1 / (1 + 2)))
  expect_equal(x$annotated_sexp[[2]]$annotated_sexp, 1)
  expect_equal(x$annotated_sexp[[3]]$type, "bracket")
})

test_that("it supports backsolve", {
  x <- classify_sexp(quote(backsolve(x, y)))
  expect_true(x$type == "backsolve")
})

test_that("it supports forwardsolve", {
  x <- classify_sexp(quote(forwardsolve(x, y)))
  expect_true(x$type == "forwardsolve")
})
