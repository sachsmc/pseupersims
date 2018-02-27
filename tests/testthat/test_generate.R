context("Data generation")

testthat("Generated data has approximately 25% cumulative incidence at 26.5 weeks", {

  ckdat <- generate_data(n = 15000)
  expect_equal(mean(ckdat$trueP), .2, tolerance = .05)
  expect_equal(1 - summary(survfit(Surv(Tout, delta) ~ 1, data = ckdat), times = 26.5)$surv, .2, tolerance = .05)

})