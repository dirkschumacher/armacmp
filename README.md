
<!-- README.md is generated from README.Rmd. Please edit that file -->

# armacmp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/dirkschumacher/armacmp.svg?branch=master)](https://travis-ci.org/dirkschumacher/armacmp)
[![Codecov test
coverage](https://codecov.io/gh/dirkschumacher/armacmp/branch/master/graph/badge.svg)](https://codecov.io/gh/dirkschumacher/armacmp?branch=master)
<!-- badges: end -->

The goal of `armacmp` is to create an experimental DSL to formulate
linear algebra code in R that is compiled to C++ using the Armadillo
Template Library.

Currently just a quick idea and prototype. If this sounds useful, let me
know.

## Installation

``` r
remotes::install_github("dirkschumacher/armacmp")
```

## Example

You can compile R like code to C++. Not all R functions are supported.

``` r
library(armacmp)
```

Takes an input\_matrix and returns its transpose.

``` r
trans <- armacmp({
  return(t(input_matrix()))
})
trans(matrix(1:10))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    1    2    3    4    5    6    7    8    9    10
```

Equivalent to R’s crossprod (`t(x) %*% y`)

``` r
crossprod2 <- armacmp({
  X <- input_matrix()
  Y <- input_matrix()
  return(t(X) %*% Y)
})
x <- matrix(1:100000)
all.equal(crossprod2(x, x), crossprod(x, x))
#> [1] TRUE
microbenchmark::microbenchmark(
  crossprod2(x, x),
  crossprod(x, x)
)
#> Unit: microseconds
#>              expr     min        lq     mean   median       uq      max
#>  crossprod2(x, x) 349.722  964.2375 1292.697 1002.904 1065.416 6216.602
#>   crossprod(x, x) 447.769 1065.8295 1107.765 1116.031 1163.930 5105.667
#>  neval
#>    100
#>    100
```

Compute the coefficient of a linear regression problem:

``` r
lm_fit <- armacmp({
  X <- input_matrix()
  y <- input_matrix()
  return(solve(X, y))
})
X <- model.matrix(mpg ~ hp + cyl, data = mtcars)
y <- matrix(mtcars$mpg)
all.equal(as.numeric(qr.solve(X, y)), as.numeric(lm_fit(X, y)))
#> [1] TRUE
```

Or a C++ version of plogis:

``` r
plogis2 <- armacmp({
  return(1 / (1 + exp(-input_matrix())))
})
all.equal(plogis2(matrix(1:10)), stats::plogis(matrix(1:10)))
#> [1] TRUE
```

Or make predictions in a logistic regression model given the
coefficients:

``` r
log_predict <- armacmp({
  coef <- input_matrix()
  new_X <- input_matrix()
  res <- new_X %*% coef
  score <- 1 / (1 + exp(-res))
  return(score)
})

formula <- I(mpg < 20) ~ -1 + hp + cyl
X <- model.matrix(formula, data = mtcars)
glm_fit <- glm(formula, data = mtcars, family = binomial())

all.equal(
  as.numeric(log_predict(matrix(coef(glm_fit)), X)),
  as.numeric(predict(glm_fit, newdata = mtcars, type = "response"))
)
#> [1] TRUE
```

Forward and backward solve are implemented

``` r
backsolve2 <- armacmp({
  x <- input_matrix()
  y <- input_matrix()
  return(backsolve(x, y))
})
forwardsolve2 <- armacmp({
  x <- input_matrix()
  y <- input_matrix()
  return(forwardsolve(x, y))
})

# example from the docs of backsolve
r <- rbind(
  c(1, 2, 3),
  c(0, 1, 1),
  c(0, 0, 2)
)
x <- matrix(c(8, 4, 2))
all.equal(backsolve(r, x), backsolve2(r, x))
#> [1] TRUE
all.equal(forwardsolve(r, x), forwardsolve2(r, x))
#> [1] TRUE
```

### For loops

``` r
for_loop <- armacmp({
  X <- input_matrix()
  X_new <- X
  # only seq_len is currently supported
  for (i in seq_len(10 + 10)) {
    # use replace to update an existing variable
    replace(X_new, log(X_new + i))
  }
  return(X_new)
})

for_loop_r <- function(X) {
  X_new <- X
  for (i in seq_len(10 + 10)) {
    X_new <- log(X_new + i)
  }
  return(X_new)
}

all.equal(
  for_loop_r(matrix(1:10000, ncol = 10)),
  for_loop(matrix(1:10000, ncol = 10))
)
#> [1] TRUE

microbenchmark::microbenchmark(
  for_loop_r(matrix(1:10000, ncol = 10)),
  for_loop(matrix(1:10000, ncol = 10))
)
#> Unit: milliseconds
#>                                    expr      min       lq     mean
#>  for_loop_r(matrix(1:10000, ncol = 10)) 2.034266 2.479202 3.420276
#>    for_loop(matrix(1:10000, ncol = 10)) 1.374095 1.430728 1.709544
#>    median       uq       max neval
#>  2.613669 3.090027 10.329957   100
#>  1.494744 1.716525  3.981985   100
```

## API

### Inputs

  - `input_matrix` defines a new input parameter of type matrix
  - …

### Body

  - `<-` you can use assignments that cause a C++ copy. As most
    operations return armadillo expressions, this is often not a
    problem. All assignments are constant and nothing can be reassigned.
  - `...` many more functions :)

### Return

All functions need to return a value using the `return` function.

## TODO

The idea is to implement a number of more complex constructs and also
infer some data types to generate better code.

``` r
qr_lm_coef <- armacmp({
  # TBD
  X <- input_matrix()
  y <- input_matrix()
  qr_res <- qr(X)
  qty <- t(qr.Q(qr_res)) %*% y
  beta_hat <- backsolve(qr.R(qr_res), qty)
  return(beta_hat)
})
```

``` r
if_clause <- armacmp({
  X <- input_matrix()
  y <- input_colvec()
  test <- sum(exp(X)) < 10 # infers that test needs to be bool
  if (test) {
    return(X %*% y + 10)
  } else {
    return(X %*% y)
  }
})
```