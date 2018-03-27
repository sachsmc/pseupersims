context("Data generation")

testthat("Generated data has approximately 20% cumulative incidence at 26.5 weeks", {

  ckdat <- generate_data(n = 15000, "A")

  expect_equal(mean(ckdat$trueP), .2, tolerance = .05)
  expect_equal(1 - summary(survfit(Surv(Tout, delta == 1) ~ 1, data = ckdat), times = 26.5)$surv, .2, tolerance = .05)

  ckdat <- generate_data(n = 15000, "0")
  expect_equal(mean(ckdat$trueP), .2, tolerance = .05)

  ckdat <- generate_data(n = 15000, "B")
  expect_equal(mean(ckdat$trueP), .2, tolerance = .075)

  ckdat <- generate_data(n = 15000, "C")
  expect_equal(mean(ckdat$trueP), .2, tolerance = .1)


  ckdat <- generate_data(n = 2000, "0")
  psuo <- pseudoci(ckdat$Tout, event = ckdat$delta, tmax = 26.5)
  expect_equal(mean(psuo$pseudo$cause1[, 1]), .2, tolerance = .05)
  expect_equal(mean(psuo$pseudo$cause2[, 1]), .07, tolerance = .025)

})



testthat("Generated data has approximately 10% censoring", {

  ckdat <- generate_data(n = 15000, "A")
  expect_equal(mean(ckdat$delta == 0), .1, tolerance = .075)
  ckdat$binY <- with(ckdat, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))
  expect_equal(mean(is.na(ckdat$binY)), .1, tolerance = .05)

  ckdat <- generate_data(n = 15000, "A", missing.p = .5)
  expect_equal(mean(ckdat$delta == 0), .5, tolerance = .05)
  ckdat$binY <- with(ckdat, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))
  expect_equal(mean(is.na(ckdat$binY)), .5, tolerance = .05)


  ckdat <- generate_data(n = 15000, "0")
  expect_equal(mean(ckdat$delta == 0), .1, tolerance = .05)
  ckdat$binY <- with(ckdat, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))
  expect_equal(mean(is.na(ckdat$binY)), .1, tolerance = .05)

  ckdat <- generate_data(n = 15000, "B", missing.p = .5)
  expect_equal(mean(ckdat$delta == 0), .5, tolerance = .05)
  ckdat$binY <- with(ckdat, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))
  expect_equal(mean(is.na(ckdat$binY)), .5, tolerance = .05)

  ckdat <- generate_data(n = 15000, "C")
  expect_equal(mean(ckdat$delta == 0), .1, tolerance = .05)
  ckdat$binY <- with(ckdat, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))
  expect_equal(mean(is.na(ckdat$binY)), .1, tolerance = .05)



})