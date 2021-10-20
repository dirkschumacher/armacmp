
<!-- README.md is generated from README.Rmd. Please edit that file -->

# armacmp

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Codecov test
coverage](https://codecov.io/gh/dirkschumacher/armacmp/branch/master/graph/badge.svg)](https://codecov.io/gh/dirkschumacher/armacmp?branch=master)
[![R-CMD-check](https://github.com/dirkschumacher/armacmp/workflows/R-CMD-check/badge.svg)](https://github.com/dirkschumacher/armacmp/actions)
<!-- badges: end -->

The goal of `armacmp` is to create a DSL to formulate linear algebra
code in R that is compiled to C++ using the Armadillo Template Library.
It also offers an mathematical optimization that uses `RcppEnsmallen` to
optimize functions in C++.

The scope of the package is linear algebra and Armadillo. It is not
meant to evolve into a general purpose R to C++ transpiler.

It has three main functions:

-   `compile` compiles an R function to C++ and makes that function
    again avaliable in your R session.
-   `translate` translates an R function to C++ and returns the code as
    text.
-   `compile_optimization_problem` uses `RcppEnsmallen` and the
    functions above to compile continuous mathematical optimizations
    problems to C++.

This is currently an *experimental prototype* with most certainly bugs
or unexpected behaviour. However I would be happy for any type of
feedback, alpha testers, feature requests and potential use cases.

Potential use cases:

-   Speed up your code :)
-   Quickly estimate `Rcpp` speedup gain for linear algebra code
-   Learn how R linear algebra code can be expressed in C++ using
    `translate` and use the code as a starting point for further
    development.
-   Mathematical optimization with `optimize`
-   …

## Installation

``` r
remotes::install_github("dirkschumacher/armacmp")
```

## Caveats and limitations

-   *speed*: R is already really fast when it comes to linear algebra
    operations. So simply compiling your code to C++ might not give you
    a *significant and relevant* speed boost. The best way to check is
    to measure it yourself and see for your specific use-case, if
    compiling your code to C++ justifies the additional complexity.
-   *NAs*: there is currently no NA handling. In fact everything is
    assumed to be double (if you use matrices/vectors).
-   *numerical stability*: Note that your C++ code might produce
    different results in certain situations. Always validate before you
    use it for important applications.

## Example

You can compile R like code to C++. Not all R functions are supported.

``` r
library(armacmp)
```

Takes a matrix and returns its transpose.

``` r
trans <- compile(function(X) {
  return(t(X))
})
trans(matrix(1:10))
#>      [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
#> [1,]    1    2    3    4    5    6    7    8    9    10
```

Or a slightly larger example using QR decomposition

``` r
# from Arnold, T., Kane, M., & Lewis, B. W. (2019). A Computational Approach to Statistical Learning. CRC Press.
lm_cpp <- compile(function(X, y = type_colvec()) {
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

## API

`armacmp` always compiles functions. Every function needs to have a
`return` statement with an optional type argument.

``` r
my_fun <- compile(function(X, y = type_colvec())) {
  return(X %*% y, type = type_colvec())
}
```

A lot of linear algebra functions/operators are defined as well some
control flow (for loops and if/else). Please take a look at the
[function reference
article](https://dirkschumacher.github.io/armacmp/articles/function-reference.html)
for more details what can be expressed.

### Optimization of arbitrary and differentiable functions using `ensmallen`

The package now also supports optimization of functions using
`RcppEnsmallen`. Find out more at
[ensmallen.org](https://ensmallen.org/).

All code is compiled to C++. During the optimization there is no context
switch back to R.

#### Arbitrary function

Here we minimize `2 * norm(x)^2` using simulated annealing.

``` r
# taken from the docs of ensmallen.org
optimize <- compile_optimization_problem(
  data = list(),
  evaluate = function(x) {
    return(2 * norm(x)^2)
  },
  optimizer = optimizer_SA()
)

# should be roughly 0
optimize(matrix(c(1, -1, 1), ncol = 1))
#>               [,1]
#> [1,] -9.930631e-04
#> [2,]  1.696781e-05
#> [3,]  1.891890e-03
```

Optimizers:

-   Simulated Annealing through `optimizer_SA`
-   Conventional Neural Evolution `optimizer_CNE`
-   …

#### Differentiable functions

Here solve a linear regression problem using L-BFGS.

``` r
optimize_lbfgs <- compile_optimization_problem(
  data = list(design_matrix = type_matrix(), response = type_colvec()),
  evaluate = function(beta) {
    return(norm(response - design_matrix %*% beta)^2)
  },
  gradient = function(beta) {
    return(-2 %*% t(design_matrix) %*% (response - design_matrix %*% beta))
  },
  optimizer = optimizer_L_BFGS()
)

# this example is taken from the RcppEnsmallen package
# https://github.com/coatless/rcppensmallen/blob/master/src/example-linear-regression-lbfgs.cpp
n <- 1e6
beta <- c(-2, 1.5, 3, 8.2, 6.6)
p <- length(beta)
X <- cbind(1, matrix(rnorm(n), ncol = p - 1))
y <- X %*% beta + rnorm(n / (p - 1))

# Run optimization with lbfgs fullly in C++
optimize_lbfgs(
  design_matrix = X,
  response = y,
  beta = matrix(runif(p), ncol = 1)
)
#>           [,1]
#> [1,] -1.997780
#> [2,]  1.500975
#> [3,]  2.999542
#> [4,]  8.200816
#> [5,]  6.600444
```

Optimizers:

-   L-BFGS through `optimizer_L_BFGS`
-   Gradient Descent through `optimizer_GradientDescent`
-   …

### When does `armacmp` improve performance?

It really depends on the use-case and your code. In general Armadillo
can combine linear algebra operations. For example the addition of 4
matrices `A + B + C + D` can be done in a single for loop. Armadillo can
detect that and generates efficient code.

So whenever you combine many different operations, `armacmp` *might* be
helpful in speeding things up.

We gather some examples on the wiki to further explore if compiling
linear algebra code to C++ actually makes sense for pure speed reasons.

### Related projects

-   [nCompiler](https://github.com/nimble-dev/nCompiler) - Code-generate
    C++ from R. Inspired the approach to compile R functions directly
    instead of just a code block as in the initial version.

### Contribute

`armacmp` is experimental and has a volatile codebase. The best way to
contribute is to write issues/report bugs/propose features and test the
package with your specific use-case.

### Code of conduct

Please note that the ‘armacmp’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

### References

-   Conrad Sanderson and Ryan Curtin. Armadillo: a template-based C++
    library for linear algebra. Journal of Open Source Software, Vol. 1,
    pp. 26, 2016.
-   S. Bhardwaj, R. Curtin, M. Edel, Y. Mentekidis, C. Sanderson.
    ensmallen: a flexible C++ library for efficient function
    optimization. Workshop on Systems for ML and Open Source Software at
    NIPS 2018.
-   Dirk Eddelbuettel, Conrad Sanderson (2014). RcppArmadillo:
    Accelerating R with high-performance C++ linear algebra.
    Computational Statistics and Data Analysis, Volume 71, March 2014,
    pages 1054-1063. URL <http://dx.doi.org/10.1016/j.csda.2013.02.005>
-   Dirk Eddelbuettel and Romain Francois (2011). Rcpp: Seamless R and
    C++ Integration. Journal of Statistical Software, 40(8), 1-18. URL
    <https://www.jstatsoft.org/v40/i08/>.
