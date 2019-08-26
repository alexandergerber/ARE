
##
bench::mark(
 sample(1:1e6, 1e6, replace = T),
 sample.int(1e6, 1e6, replace = T),
 check = F
)

##
x <- rnorm(1e5)
y <- rnorm(1e5)

addition <- function(a, b) {
  n <- length(a)
  if (n != length(b)) stop("incompatible arguments")
  
  out <- numeric(n)
  for (i in 1:n) {
    out[i] <- a[i] + b[i]
  }
  return(out)
}

bench::mark(
  x + y,
  addition(x, y)
)

##

X <- matrix(rnorm(10000), ncol = 100)
ones <- rep_len(1, 100)

rsums <- function(x) {
  n <- nrow(x)
  m <- ncol(x)
  out <- numeric(n)
  for(i in 1:n) {
    rs <- 0
    for(j in 1:m) {
      rs <- rs + X[i, j]  
    }
    out[i] <- rs
  }
  return(out)
}

bench::mark(
  rsums(X),
  rowSums(X),
  c(tcrossprod(ones, X)),
  check = F
)
