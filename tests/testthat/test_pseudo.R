

context("Weighted pseudo observations")

testthat("Pseudo values with weights one give same values as original", {

  ckdat <- generate_data(n = 150, "A")

  test1 <- pseudoci.weighted(ckdat$Tout, ckdat$delta, tmax = 26.5, weights = rep(1, nrow(ckdat)))
  test2 <- pseudoci(ckdat$Tout, ckdat$delta, tmax = 26.5)

  expect_equal(mean(abs(test1$pseudo$cause1 - test2$pseudo$cause1)), 0.0, tolerance = .01)
  expect_equal(mean(abs(test1$pseudo$cause2 - test2$pseudo$cause2)), 0.0, tolerance = .01)

})