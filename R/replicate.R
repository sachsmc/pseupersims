
#' Run one replicate of the experiment
#'
#' Generates data, analyzes the results in different ways, runs the perturbation algorithm,
#' and returns the output in a sensible manner.
#'
#' @param seed random seed (integer)
#' @param scenario data generation scenario, 0, A, B, or C
#' @param missing.p proportion missing the binary outcome
#' @param output path to output file
#'
#' @export

run_one_replicate <- function(seed, scenario = "A", missing.p = .2,  output = "repouput.rds") {

  if(!missing(seed)) set.seed(seed)

  indat <- generate_data(n = 500, scenario = scenario, missing.p = missing.p)
  validat <- subset(add_pseudo_obs(generate_data(scenario = scenario, missing.p = missing.p)), time == 26.5)

  indat2 <- add_pseudo_obs(indat)

  slearn.fit <- superlearner_estimate(indat2, Y = "cause1.pseudo", X = c("time", paste0("X", 1:20)), Y2 = "cause2.pseudo")

  #slearn.fit <- stupidlearner_estimate(indat2, Y = "cause1.pseudo", X = paste0("X", 1:20), y0, y1)

  preder <- predict(slearn.fit, validat[, c("time", paste0("X", 1:20))])

   if(any(is.na(preder$pred[,1]))) {
     predres <- preder$library.predict[, which.min(slearn.fit$cvRisk)]
   } else {
     predres <- preder$pred[, 1]
   }

  #predres <- preder
  ## back transform

  validat$predres.pseudo <- predres #(exp(predres) * y1 + y0) / (exp(predres) + 1)

  # plot(trueP ~ predres.pseudo, validat)
  # abline(0, 1)


  indat2$binY <- with(indat2, ifelse(Tout > 26.5, 0, ifelse(delta == 0, NA, ifelse(delta == 1, 1, 0))))
#
#   indat2$binY <- rbinom(500, 1, indat$trueP)
#   indat2$binY[which(indat2$binY == 1)[1:100]]  <- NA
  indat3 <- subset(indat2, time == 26.5)

  cwefit <- survfit(Surv(Tout, delta == 0) ~ 1, data = indat3)
  wts <- summary(cwefit, times = 26.5)$surv
  indat3$censW <- 1 / wts

  slearn.binfit <- superlearner_binaryestimate(subset(indat3, !is.na(binY)), Y = "binY", X = paste0("X", 1:20))

  bin.predres <-  predict(slearn.binfit, validat[, paste0("X", 1:20)], type = "response")$pred[, 1]

  validat$predres.binary <- bin.predres

  ## cox model strawman


  fit.cox <- cv.glmnet(as.matrix(indat2[, paste0("X", 1:20)]), cbind(time = indat2$Tout, status = 1.0 * indat2$delta == 1),
                       family = "cox")


  est.cox <- predict(fit.cox, newx = as.matrix(validat[, paste0("X", 1:20)]))[, 1]
  validat$predres.cox <- est.cox

  # pstest <- with(validat, calc_roc(predres.pseudo, trueT, cause2.pseudo))
  # plot(tpf ~ fpf, data = pstest, col = "blue", type = "l")
  # bitest <- with(validat, calc_roc(predres.binary, trueT, cause2.pseudo))
  # lines(tpf ~ fpf, data = bitest)
  # trtest <- with(validat, calc_roc(trueP, trueT, cause2.pseudo))
  # lines(tpf ~ fpf, data = trtest, col = "red")
  # coxtst <- with(validat, calc_roc(predres.cox, trueT, cause2.pseudo))
  # lines(tpf ~ fpf, data = coxtst, col = "green")

  saveRDS(validat, file = output)


}
