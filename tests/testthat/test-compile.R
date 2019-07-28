test_that("some basic checks", {
  a <- annotate_ast(quote({
    x <- input_matrix()
    y <- input_matrix()
    return(t(x) %*% y)
  }))
  expect_silent(compile_to_function(a, "wat"))
})
