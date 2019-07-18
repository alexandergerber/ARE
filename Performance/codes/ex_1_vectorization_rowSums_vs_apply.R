results <- bench::press(
  dimensions = c(1e0, 1e1, 1e2, 1e3, 0.5e4, 1e4),
  {
    X <- tcrossprod(rnorm(dimensions), rnorm(dimensions))
    bench::mark(
      apply = rowSums(X),
      rowSums = apply(X, 1, sum),
      filter_gc = F,
      iterations = 50
    )
  }
)

p <- plot(results, type = "violin")
ggsave("Performance/img/rowSums_vs_apply.png", p, width = 10, height = 7)
