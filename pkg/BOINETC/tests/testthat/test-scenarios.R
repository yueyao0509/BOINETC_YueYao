test_that("scenario helper returns expected scenario", {
  sc <- get_boinetc_scenario(1)
  expect_equal(sc$id, 1)
  expect_equal(dim(sc$pt.true.mat), c(2, 3))
  expect_equal(dim(sc$pe.true.mat), c(2, 3))
})
