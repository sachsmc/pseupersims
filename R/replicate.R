
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

run_one_replicate <- function(seed, scenario = "A", missing.p = .1,  output = "repouput.rds") {

  if(!missing(seed)) set.seed(seed)

  indat <- generate_data(n = 500, scenario = scenario, missing.p = missing.p)
  validat <- add_pseudo_obs(generate_data(scenario = scenario, missing.p = missing.p))

  indat2 <- add_pseudo_obs(indat)

  y0 <- min(indat2$cause1.pseudo) - abs(min(indat2$cause1.pseudo) * .05)
  y1 <- max(indat2$cause1.pseudo) * 1.05

  #slearn.fit <- superlearner_estimate(indat2, Y = "Y", X = paste0("X", 1:20), y0, y1)

  slearn.fit <- stupidlearner_estimate(indat2, Y = "cause1.pseudo", X = paste0("X", 1:20), y0, y1)

  preder <- predict(slearn.fit, validat[, paste0("X", 1:20)])

  # if(any(is.na(preder$pred[,1]))) {
  #   predres <- preder$library.predict[, which.min(slearn.fit$cvRisk)]
  # } else {
  #   predres <- preder$pred[, 1]
  # }

  predres <- preder
  ## back transform

  validat$predres.pseudo <- predres #(exp(predres) * y1 + y0) / (exp(predres) + 1)

  indat2$binY <- with(indat2, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))
#
#   indat2$binY <- rbinom(500, 1, indat$trueP)
#   indat2$binY[which(indat2$binY == 1)[1:100]]  <- NA

  slearn.binfit <- stupidlearner_binaryestimate(subset(indat2, !is.na(binY)), Y = "binY", X = paste0("X", 1:20))

  bin.predres <-  predict(slearn.binfit, validat[, paste0("X", 1:20)], type = "response")#$pred[, 1]

  validat$predres.binary <- bin.predres

  # pstest <- with(validat, calc_roc(predres.pseudo, trueT))
  # plot(tpf ~ fpf, data = pstest, col = "blue", type = "l")
  # bitest <- with(validat, calc_roc(predres.binary, trueT))
  # lines(tpf ~ fpf, data = bitest)
  # trtest <- with(validat, calc_roc(trueP, trueT))
  # lines(tpf ~ fpf, data = trtest, col = "red")


  saveRDS(validat, file = output)


}