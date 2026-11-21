
# 测试 RemoveSeasonal() 函数
test_that("RemoveSeasonal 可以正确去除季节变化", {
  y <- c(10, 12, 20, 22)
  t_mmdd <- c(101, 101, 102, 102)

  result <- RemoveSeasonal(y, t_mmdd)

  expect_equal(result$mu, c(11, 21))
  expect_equal(result$anomaly, c(-1, 1, -1, 1))
})


# 测试 RemoveSeasonal() 对缺失值的处理
test_that("RemoveSeasonal 可以处理 NA", {
  y <- c(10, NA, 20, 22)
  t_mmdd <- c(101, 101, 102, 102)

  result <- RemoveSeasonal(y, t_mmdd)

  expect_equal(result$mu, c(10, 21))
  expect_equal(result$anomaly, c(0, NA, -1, 1))
})


# 测试 lm_solve() 函数
test_that("lm_solve 可以正确计算线性回归", {
  x <- 1:4
  y <- 2 + 3 * x
  design <- cbind(1, x)

  result <- lm_solve(design, y)

  expect_equal(as.numeric(result$coef), c(2, 3),
               tolerance = 1e-8)
  expect_equal(result$fitted, as.numeric(y))
  expect_equal(result$SSE, 0, tolerance = 1e-8)
}


# 测试 LS_OneBreak() 函数
test_that("LS_OneBreak 可以正确计算单断点回归", {
  x <- 1:6
  y <- 2 + 3 * x + c(0, 0, 0, 5, 5, 5)

  result <- LS_OneBreak(Y = y, X = x, Ic = 3)

  expect_equal(as.numeric(result$coef), c(2, 3, 5),
               tolerance = 1e-8)
  expect_equal(result$SSE, 0, tolerance = 1e-8)
})


# 测试 LS_MultiBreaks() 函数
test_that("LS_MultiBreaks 可以正确计算多断点回归", {
  x <- 1:6
  y <- 2 + 3 * x + c(0, 0, 0, 5, 5, 5)

  result <- LS_MultiBreaks(
    Y = y,
    X = x,
    Ips = c(0, 3, 6)
  )

  expect_equal(as.numeric(result$coef), c(2, 3, 5),
               tolerance = 1e-8)
  expect_equal(result$SSE, 0, tolerance = 1e-8)
})
