add_nonvec <- function(x, y) {
  z <- numeric(length(x))
  for(i in seq_along(x)) {
    z[i] <- x[i] + y[i]
  }
  return(z)
}

add_vec <- function(x, y) {
  return(x + y)
}

bench::mark(
  add_nonvec(1:10000, 1:10000),
  add_vec(1:10000, 1:10000)
)







