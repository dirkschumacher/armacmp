
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

The goal of `armacmp` is to create a DSL to formulate linear algebra
code in R that is compiled to C++ using the Armadillo Template Library.

The scope of the package is linear algebra and Armadillo. It is not
meant to evolve into a general purpose R to C++ transpiler.

This is currently an *experimental prototype* with most certainly a lot
of bugs and the appropriate code quality ;). However I would be happy
for any type of feedback, alpha testers, feature requests and potential
use cases.

Potential use cases:

  - Speed up your code :)
  - Quickly estimate `Rcpp` speedup gain for linear algebra code
  - Learn how R linear algebra code can be expressed in C++ using
    `armacmp_compile` and use the code as a starting point for further
    development.
  - …

## Installation

``` r
remotes::install_github("dirkschumacher/armacmp")
```

## On speeding up your code with `armacmp`

R is already really fast when it comes to linear algebra operations. So
simply compiling your code to C++ might not give you a *significant and
relevant* speed boost. The best way to check is to measure it yourself
and see for your specific use-case, if compiling your code to C++
justifies the additional complexity.

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

## API

`armacmp` always compiles functions. Every function needs to have a
`return` statement with an optional type argument.

``` r
my_fun <- armacmp(function(X, y = type_colvec())) {
  return(X %*% y, type = type_colvec())
}
```

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
    problem. Usually assignments create new matrix variables, unless all
    operands on the right hand side can be assumed to not be any matrix
    code. Then the C++11 compiler will figure out the concrete type.
  - `=` use the equal assignment if you want to reassign a variable with
    a new value.

Below is a list of functions that are currently supported:

… *TDD* For now, please take a look at the examples below.

### Return

All functions need to return a value using the `return` function.

## Further examples

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
#>              expr     min       lq     mean   median       uq       max
#>  crossprod2(x, x) 416.093 1067.318 1524.063 1136.677 1290.149 10401.938
#>   crossprod(x, x) 465.263 1193.555 1925.607 1273.711 1492.729 16572.836
#>        t(x) %*% x 876.882 1542.110 2113.463 1789.693 2069.581  6909.747
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
    X_new <- log(t(X_new) %*% X_new + i + offset)
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
#>                                                expr     min      lq
#>  for_loop_r(matrix(1:1000, ncol = 10), offset = 10) 150.029 253.277
#>    for_loop(matrix(1:1000, ncol = 10), offset = 10)  42.598  68.211
#>      mean   median       uq      max neval
#>  406.8802 308.2925 446.5135 2333.085   100
#>  163.3694  81.9190 125.6545 6182.758   100
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
#> 1 cumprod(x)              132.09ms 399.89ms      2.50   15.26MB     1.25
#> 2 as.numeric(cumprod2(x))   4.41ms   6.43ms    104.      7.63MB    56.9
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
#> arma::mat score = 1 / (1 + arma::exp(-res));
#> return score;
#> }
#> 
#> 
```

### If clause

``` r
if_clause <- armacmp(function(X) {
  # infers that test needs to be bool (auto)
  test <- sum(log(X)) < 10
  if (test) {
    return((t(X) %*% X) + 10)
  } else if (sum(X) < 10) {
    return(t(X) %*% X)
  } else {
    return((t(X) %*% X) + 10)
  }
})

if_clause_r <- function(X) {
  test <- sum(log(X)) < 10
  if (test) {
    return((t(X) %*% X) + 10)
  } else if (sum(X) < 10) {
    return(t(X) %*% X)
  } else {
    return(t(X) %*% X + 10)
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
#>            expr     min       lq     mean  median       uq      max neval
#>  if_clause_r(X) 286.947 317.1115 407.5185 325.027 341.4065 6895.925   100
#>    if_clause(X)  96.471 135.1730 149.6851 139.450 151.9175  307.139   100
```

### QR decomposition

``` r
lm_cpp <- armacmp(function(X, y = type_colvec()) {
  qr_res <- qr(X)
  qty <- t(qr.Q(qr_res)) %*% y
  beta_hat <- backsolve(qr.R(qr_res), qty)
  return(beta_hat, type = type_colvec())
})

# example from the R docs of lm.fit
n <- 70000 ; p <- 20
X <- matrix(rnorm(n * p), n, p) 
y <- rnorm(n)
all.equal(
  as.numeric(coef(lm.fit(X, y))),
  as.numeric(lm_cpp(X, y))
)
#> [1] TRUE
```

### Related projects

  - [nCompiler](https://github.com/nimble-dev/nCompiler) - Code-generate
    C++ from R. Inspired the approach to compile R functions directly
    instead of just a code block as in the initial version.
