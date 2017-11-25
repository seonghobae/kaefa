# testing 1
  library('kaefa')
  library('mirt')
  context("Model Estimation (single level)")

  mod1 <- kaefa::aefa(mirt::Science)

  testthat::test_that("number of factors", {
    expect_equal(mod1$estModelTrials[NROW(mod1$estModelTrials)]@Model$nfact, 2)
  })
