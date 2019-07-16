i <- function() {
  replicate(5, 
            mean(
              rnorm(1e6)
              )
            )
}
