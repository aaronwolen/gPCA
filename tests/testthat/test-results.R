context("Check results")

data(caseDat)
nperm <- 250
out <- gPCA.batchdetect(caseDat$data, caseDat$batch, nperm = nperm)

test_that("reported dimensions match input data", {
  expect_equal(out$n, nrow(caseDat$data))
  expect_equal(out$p, ncol(caseDat$data))
  expect_equal(out$b, length(unique(caseDat$batch)))
})

test_that("delta matches expected value", {
  expect_equal(out$delta, 0.5723599)
})

test_that("cumulative variance from guided PCA matches expected values", {
  expect_equivalent(round(out$cumulative.var.g, 3), c(0.612, 0.959, 1))
})

test_that("delta values are reported for each perm", {
  expect_equal(length(out$delta.p), nperm)
})