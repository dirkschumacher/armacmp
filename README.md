
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

Takes a matrix and returns its transpose.

``` r
trans <- armacmp(function(X) {
  return(t(X))
})
trans(matrix(1:10))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    1    2    3    4    5    6    7    8    9    10
```

Equivalent to R’s crossprod (`t(x) %*% y`)

``` r
# by default arguments are assumed to be matrices
# but you can also make it explicit
crossprod2 <- armacmp(function(X = type_matrix(), Y = type_matrix()) {
  return(t(X) %*% Y)
})
x <- matrix(1:100000)
all.equal(crossprod2(x, x), crossprod(x, x))
#> [1] TRUE
microbenchmark::microbenchmark(
  crossprod2(x, x),
  crossprod(x, x),
  t(x) %*% x
)
#> Unit: microseconds
#>              expr     min        lq     mean   median       uq       max
#>  crossprod2(x, x) 357.337  969.9115 1199.647 1018.377 1096.747  7272.153
#>   crossprod(x, x) 471.707 1081.6490 1415.818 1158.952 1230.923  5701.794
#>        t(x) %*% x 806.691 1597.5390 1983.228 1677.217 1808.017 13010.352
#>  neval
#>    100
#>    100
#>    100
```

Compute the coefficient of a linear regression problem:

``` r
# by default X is assumed a matrix
lm_fit <- armacmp(function(X, y = type_colvec()) {
  return(solve(X, y))
})
X <- model.matrix(mpg ~ hp + cyl, data = mtcars)
y <- matrix(mtcars$mpg)
all.equal(as.numeric(qr.solve(X, y)), as.numeric(lm_fit(X, y)))
#> [1] TRUE
```

Or a C++ version of plogis:

``` r
plogis2 <- armacmp(function(x = type_colvec()) {
  return(1 / (1 + exp(-x)))
})
all.equal(as.numeric(plogis2(1:10)), stats::plogis(1:10))
#> [1] TRUE
```

Or make predictions in a logistic regression model given the
coefficients:

``` r
log_predict <- armacmp(function(coef = type_colvec(), new_X) {
  res <- new_X %*% coef
  score <- 1 / (1 + exp(-res))
  return(score, type = type_colvec())
})

formula <- I(mpg < 20) ~ -1 + hp + cyl
X <- model.matrix(formula, data = mtcars)
glm_fit <- glm(formula, data = mtcars, family = binomial())

all.equal(
  as.numeric(log_predict(coef(glm_fit), X)),
  as.numeric(predict(glm_fit, newdata = mtcars, type = "response"))
)
#> [1] TRUE
```

Forward and backward solve are implemented

``` r
backsolve2 <- armacmp(function(x, y = type_colvec()) {
  return(backsolve(x, y))
})
forwardsolve2 <- armacmp(function(x, y = type_colvec()) {
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
for_loop <- armacmp(function(X, offset = type_scalar_numeric()) {
  X_new <- X
  # only seq_len is currently supported
  for (i in seq_len(10 + 10)) {
    # use replace to update an existing variable
    replace(X_new, log(t(X_new) %*% X_new + i + offset))
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

all.equal(
  for_loop_r(matrix(as.numeric(1:1000), ncol = 10), offset = 10),
  for_loop(matrix(as.numeric(1:1000), ncol = 10), offset = 10)
)
#> [1] TRUE

microbenchmark::microbenchmark(
  for_loop_r(matrix(1:1000, ncol = 10), offset = 10),
  for_loop(matrix(1:1000, ncol = 10), offset = 10)
)
#> Unit: microseconds
#>                                                expr     min       lq
#>  for_loop_r(matrix(1:1000, ncol = 10), offset = 10) 119.316 132.5790
#>    for_loop(matrix(1:1000, ncol = 10), offset = 10)  37.289  38.5475
#>       mean   median      uq     max neval
#>  144.37346 134.2080 139.286 332.674   100
#>   42.36518  39.3855  40.950 132.454   100
```

### A faster `cumprod`

``` r
cumprod2 <- armacmp(function(x = type_colvec()) {
  return(cumprod(x))
})

x <- as.numeric(1:1e6)
bench::mark(
  cumprod(x),
  as.numeric(cumprod2(x))
)
#> # A tibble: 2 x 6
#>   expression                   min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>              <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 cumprod(x)              121.62ms 129.43ms      7.88   15.26MB     2.63
#> 2 as.numeric(cumprod2(x))   3.76ms   4.26ms    200.      7.63MB    69.1
```

### Return type

``` r
return_type <- armacmp(function(X) {
  return(sum(log(X)), type = type_scalar_numeric())
})
X <- matrix(1:1000, ncol = 10)
all.equal(
  return_type(X),
  sum(log(X))
)
#> [1] TRUE
return_type(X)
#> [1] 5912.128
```

### `colSums` et al.

``` r
colSums2 <- armacmp(function(X) return(colSums(X)), verbose = TRUE)
#> R function
#> 
#> function (X) 
#> return(colSums(X))
#> 
#> C++ function translation
#> 
#> arma::mat armacmp_fun(const arma::mat& X)
#> {
#> return arma::sum( X, 0 );
#> }
rowSums2 <- armacmp(function(X) return(rowSums(X)))
colMeans2 <- armacmp(function(X) return(colMeans(X)))
rowMeans2 <- armacmp(function(X) return(rowMeans(X)))

X <- matrix(1:100, ncol = 10)
all.equal(colSums(X), as.numeric(colSums2(X)))
#> [1] TRUE
all.equal(rowSums(X), as.numeric(rowSums2(X)))
#> [1] TRUE
all.equal(colMeans(X), as.numeric(colMeans2(X)))
#> [1] TRUE
all.equal(rowMeans(X), as.numeric(rowMeans2(X)))
#> [1] TRUE
```

### Just compile and look at the C++ code

You can also take a look at the code itself, in case you would like to
generate a C++ snippet that you can further extend.

``` r
armacmp_compile(function(new_X, coef = type_colvec()) {
  res <- new_X %*% coef
  score <- 1 / (1 + exp(-res))
  return(score, type = type_colvec())
}, "log_predict")
#> R function
#> 
#> function (new_X, coef = type_colvec()) 
#> {
#>     res <- new_X %*% coef
#>     score <- 1/(1 + exp(-res))
#>     return(score, type = type_colvec())
#> }
#> 
#> C++ function translation
#> 
#> arma::colvec log_predict(const arma::mat& new_X, const arma::colvec& coef)
#> {
#> arma::mat res = new_X * coef;
#> arma::mat score = 1 / ( 1 + arma::exp( -res ) );
#> return score;
#> }
#> 
```

### If clause

``` r
if_clause <- armacmp(function(X) {
  # infers that test needs to be bool (auto)
  test <- sum(log(X)) < 10
  if (test) {
    return((t(X) %*% X) + 10)
  } else {
    return(t(X) %*% X)
  }
})

if_clause_r <- function(X) {
  test <- sum(log(X)) < 10
  if (test) {
    return((t(X) %*% X) + 10)
  } else {
    return(t(X) %*% X)
  }
}

X <- matrix(1:10000)
all.equal(
  if_clause_r(X),
  if_clause(X)
)
#> [1] TRUE

microbenchmark::microbenchmark(
  if_clause_r(X),
  if_clause(X)
)
#> Unit: microseconds
#>            expr     min       lq     mean   median       uq       max
#>  if_clause_r(X) 282.046 295.7720 491.2066 312.1915 420.1165 10413.537
#>    if_clause(X) 125.642 128.8145 174.3683 133.9040 159.0430  1402.795
#>  neval
#>    100
#>    100
```

## API

### Inputs

You can define your inputs using the standard function syntax. By
default paramters are of type `type_matrix`. But they can have other
types such as:

  - `type_colvec` - a column vector
  - `type_rowvec` - a row vector
  - `type_scalar_integer` - a single int value
  - `type_scalar_numeric` - a single double value

### Body

  - `<-` you can use assignments that cause a C++ copy. As most
    operations return armadillo expressions, this is often not a
    problem. All assignments create new matrix variables. Other types
    are not possible at the moment :(.
  - `...` many more functions :)

### Return

All functions need to return a value using the `return` function.

## TODO

The idea is to implement a number of more complex constructs and also
infer some data types to generate better code.

``` r
qr_lm_coef <- armacmp(function(X, y) {
  # TBD
  qr_res <- qr(X)
  qty <- t(qr.Q(qr_res)) %*% y
  beta_hat <- backsolve(qr.R(qr_res), qty)
  return(beta_hat)
})
```

### Related projects

  - [nCompiler](https://github.com/nimble-dev/nCompiler) - Code-generate
    C++ from R. Inspired the approach to compile R functions directly.
