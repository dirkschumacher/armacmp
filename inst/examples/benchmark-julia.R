library(armacmp)

# some of julia's microbenchmarks translated to C++
# https://github.com/JuliaLang/Microbenchmarks/blob/master/perf.R
# The MIT License (MIT)
#
# Copyright (c) 2009-2018 Jeff Bezanson, Stefan Karpinski, Viral B. Shah,
# and other contributors.
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
#   The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

fib_cpp <- armacmp(function(n = type_scalar_int()) {
  fib_rec <-  function(nr = type_scalar_int()) {
    if (nr < 2) {
      return(nr, type = type_scalar_int())
    } else {
      return(fib_rec(nr-1) + fib_rec(nr-2), type = type_scalar_int())
    }
  }
  return(fib_rec(n), type = type_scalar_int())
})
stopifnot(fib_cpp(20) == 6765)
fib_r <- function(n) {
  fib_rec <-  function(nr) {
    if (nr < 2) {
      return(nr)
    } else {
      return(fib_rec(nr-1) + fib_rec(nr-2))
    }
  }
  return(fib_rec(n))
}
stopifnot(fib_r(20) == 6765)

microbenchmark::microbenchmark(
  fib_cpp(20),
  fib_r(20)
)
#> Unit: microseconds
#>         expr      min       lq        mean    median         uq       max
#>  fib_cpp(20)   67.005   68.797    80.96013   78.7445    81.9375   159.208
#>    fib_r(20) 7784.130 8337.068 10444.88176 9735.8380 11110.0570 30726.564
#>  neval
#>    100
#>    100

pisum_cpp <- armacmp(function() {
  t <- 0.0
  for (j in seq_len(500L)) {
    t <- 0.0
    for (k in seq_len(10000L)) {
      t <- t + 1.0/(k*k)
    }
  }
  return(t, type = type_scalar_numeric())
})
stopifnot(abs(pisum_cpp()-1.644834071848065) < 1e-12)

pisum_r <- function() {
  t <- 0.0
  for (j in seq_len(500L)) {
    t <- 0.0
    for (k in seq_len(10000L)) {
      t <- t + 1.0/(k*k)
    }
  }
  return(t)
}
stopifnot(abs(pisum_r()-1.644834071848065) < 1e-12)

microbenchmark::microbenchmark(
  pisum_cpp(),
  pisum_r()
)
#> Unit: milliseconds
#>         expr        min         lq       mean     median         uq
#>  pisum_cpp()   7.405665   7.564064   8.359741   7.681684   8.138816
#>    pisum_r() 234.937687 254.256766 335.728802 272.915697 347.099214
#>        max neval
#>   19.02528   100
#>  938.94758   100

# julia microbenchmark qsort
qsort_r = function(a) {
  qsort_kernel = function(lo, hi) {
    i = lo
    j = hi
    while (i < hi) {
      pivot = a[floor((lo+hi)/2)]
      while (i <= j) {
        while (a[i] < pivot) i = i + 1
        while (a[j] > pivot) j = j - 1
        if (i <= j) {
          t = a[i]
          a[i] <<- a[j]
          a[j] <<- t
          i = i + 1;
          j = j - 1;
        }
      }
      if (lo < j) qsort_kernel(lo, j)
      lo = i
      j = hi
    }
  }
  qsort_kernel(1, length(a))
  return(a)
}

qsort_cpp <- armacmp(function(vec = type_colvec()) {
  qsort_kernel <- function(lo = type_scalar_int(), hi = type_scalar_int()) {
    i <- lo
    j <- hi
    while (i < hi) {
      pivot <- vec[floor((lo+hi)/2)]
      while (i <= j) {
        while (vec[i] < pivot) i <- i + 1
        while (vec[j] > pivot) j <- j - 1
        if (i <= j) {
          t <- vec[i]
          vec[i] <- vec[j]
          vec[j] <- t
          i <- i + 1
          j <- j - 1
        }
      }
      if (lo < j) qsort_kernel(lo, j)
      lo <- i
      j <- hi
    }
  }
  qsort_kernel(1, length(vec))
  return(vec, type = type_colvec())
})

x <- runif(10000)
all.equal(
  as.numeric(qsort_cpp(x)),
  qsort_r(x)
)
#> [1] TRUE
microbenchmark::microbenchmark(
  as.numeric(qsort_cpp(x)),
  qsort_r(x)
)
#> Unit: microseconds
#>                      expr       min        lq      mean     median
#>  as.numeric(qsort_cpp(x))   780.364   813.185  1017.428   892.2985
#>                qsort_r(x) 35739.019 38130.693 53444.287 44246.4470
#>         uq        max neval
#>   1107.884   2464.288   100
#>  56029.359 392169.433   100
