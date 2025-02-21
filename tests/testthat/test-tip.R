test_that("Tip works", {
  mat <- matrix(round(rnorm(10000), 2), 100, 100,
    dimnames = list(1:100, 1:100)
  )
  r <- 15
  c <- 5
  x <- mat[1:r, 1:c]

  expect_equal(tip(mat, r, c), x)
})
