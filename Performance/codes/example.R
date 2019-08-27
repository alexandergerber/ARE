
X <- matrix(rnorm(1000), ncol = 100)

X_list <- split(X, col(X))

colmax <- function(x) {
  out <- numeric(ncol(x))
  for(j in 1:ncol(x)) {
    out[j] <- max(x[,j])
    }
  return(out)
}

bench::mark(
  sapply(X_list, max),
  colmax(X),
  apply(X, 2, max),
  check = F
)


X <- matrix(rnorm(1e7), ncol = 1e4)

system.time(
  parallel::mclapply(
    1:1e3,
    function(a) sum(X),
    mc.cores = 8
  )  
)

system.time(
  lapply(1:1e3, function(a) sum(X)),
)
  




