
#' Run one replicate of the experiment
#'
#' Generates data, analyzes the results in different ways, runs the perturbation algorithm,
#' and returns the output in a sensible manner.
#'
#' @param seed random seed (integer)
#' @param scenario data generation scenario, 0, A, B, C, D, or E
#' @param missing.p proportion censoring overall
#' @param output path to output file
#' @param n sample size
#'
#' @export

run_one_replicate <- function(seed, scenario = "A", missing.p = .2,  output = "repouput.rds", n = 500) {

  if(!missing(seed)) set.seed(seed)

  indat <- generate_data(n = n, scenario = scenario, missing.p = missing.p)
  validat <- subset(add_pseudo_obs(generate_data(n = n, scenario = scenario, missing.p = missing.p)), time == 26.5)

  indat2 <- add_pseudo_obs(indat)

  slearn.fit <- superlearner_estimate(indat2, Y = "cause1.pseudo",
                                      X = c("time", paste0("X", 1:20)), Y2 = "cause2.pseudo")

  slearn.fit.single <- superlearner_estimate(subset(indat2, time == "26.5"),
                                             Y = "cause1.pseudo", X = paste0("X", 1:20), Y2 = "cause2.pseudo")

  preder <- predict(slearn.fit, validat[, c("time", paste0("X", 1:20))])

   if(any(is.na(preder$pred[,1]))) {
     predres <- preder$library.predict[, which.min(slearn.fit$cvRisk)]
   } else {
     predres <- preder$pred[, 1]
   }

  preder.single <- predict(slearn.fit.single, validat[, paste0("X", 1:20)])

  if(any(is.na(preder.single$pred[,1]))) {
    predres.single <- preder.single$library.predict[, which.min(slearn.fit.single$cvRisk)]
  } else {
    predres.single <- preder.single$pred[, 1]
  }


  validat$predres.pseudo <- predres
  validat$predres.pseudo.single <- predres.single


  indat2$binY <- with(indat2, ifelse(Tout > 26.5, 0, ifelse(delta == 0, NA, ifelse(delta == 1, 1, 0))))

  indat3 <- subset(indat2, time == 26.5)

  cwefit <- survfit(Surv(Tout, delta == 0) ~ 1, data = indat3)

  wts <- sapply(indat3$Tout, function(x) summary(cwefit, times = min(26.5, x))$surv)
  indat3$censW <- 1 / wts

  slearn.binfit <- superlearner_binaryestimate(subset(indat3, !is.na(binY)), Y = "binY", X = paste0("X", 1:20))

  bin.predres <-  predict(slearn.binfit, validat[, paste0("X", 1:20)], type = "response")$pred[, 1]

  validat$predres.binary <- bin.predres

  ## binder model

  binder.fit <- CoxBoost(indat$Tout, indat$delta, as.matrix(indat[, paste0("X", 1:20)]))
  binder.est <- predict(binder.fit, newdata = as.matrix(validat[,  paste0("X", 1:20)]), type = "CIF", times = 26.5)[, 1]

  validat$predres.binder <- binder.est

  # survival random forests


  ishwarn.fit <- rfsrc(Surv(Tout, delta) ~ ., data = indat[, c("Tout", "delta", paste0("X", 1:20))])
  ishwarn.est <- predict(ishwarn.fit, newdata = validat)

  validat$predres.rfsrc <- ishwarn.est$cif[, max(which(ishwarn.est$time.interest <= 26.5)), 1]


  saveRDS(validat, file = output)


}
