library(armacmp)
# Arnold, T., Kane, M., & Lewis, B. W. (2019). A Computational Approach to Statistical Learning. CRC Press.
# logistic regression using the Newton-Raphson algorithm
log_reg <- armacmp(function(X, y = type_colvec()) {
  beta <- rep.int(0, ncol(X))
  for (i in seq_len(25)) {
    b_old <- beta
    alpha <- X %*% beta
    p <- 1 / (1 + exp(-alpha))
    W <- p * (1 - p)
    XtX <- crossprod(X, diag(W) %*% X)
    score <- t(X) %*% (y - p)
    delta <- solve(XtX, score)
    beta <- beta + delta
  }
  return(beta)
})

log_reg_r_v1 <- function(X, y) {
  beta <- rep.int(0, ncol(X))
  for (i in seq_len(25)) {
    b_old <- beta
    alpha <- X %*% beta
    p <- 1 / (1 + exp(-alpha))
    W <- as.numeric(p * (1 - p))
    XtX <- crossprod(X, diag(W) %*% X)
    score <- t(X) %*% (y - p)
    delta <- solve(XtX, score)
    beta <- beta + delta
  }
  return(beta)
}

# a changed suggested by @JohnOrmerod that speeds up the R code
# significantly
log_reg_r_v2 <- function(X, y) {
  beta <- rep.int(0, ncol(X))
  for (i in seq_len(25)) {
    b_old <- beta
    alpha <- X %*% beta
    p <- 1 / (1 + exp(-alpha))
    W <- as.numeric(p * (1 - p))
    XtX <- t(X * W) %*% X
    score <- t(X) %*% (y - p)
    delta <- solve(XtX, score)
    beta <- beta + delta
  }
  beta
}

set.seed(50)
n <- 1000 ; p <- 50
true_beta <- rnorm(p)
X <- cbind(1, matrix(rnorm(n * (p - 1)), ncol = p - 1))
y <- runif(n) < plogis(X %*% true_beta)

# to see that it actually does logistic regression
all.equal(
  as.numeric(log_reg(X, y)),
  as.numeric(log_reg_r_v1(X, y))
)
#> [1] TRUE
all.equal(
  as.numeric(log_reg_r_v2(X, y)),
  as.numeric(coef(glm.fit(X, y, family = binomial())))
)
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> [1] TRUE


bench::mark(
  log_reg(X, y),
  log_reg_r_v1(X, y),
  log_reg_r_v2(X, y)
)
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 3 x 6
#>   expression              min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>         <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 log_reg(X, y)       54.49ms  61.18ms    15.7      10.8KB     0
#> 2 log_reg_r_v1(X, y)    1.53s    1.53s     0.654   211.8MB     3.27
#> 3 log_reg_r_v2(X, y)  77.45ms  82.86ms    11.9      30.6MB     9.93
