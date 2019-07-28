
armacmp({
  x <- input_vector()
  y <- input_vector()
  t(x) %*% y
}, name = "crossprod2")

