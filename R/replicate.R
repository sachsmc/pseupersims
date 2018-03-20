
#' Run one replicate of the experiment
#'
#' Generates data, analyzes the results in different ways, runs the perturbation algorithm,
#' and returns the output in a sensible manner.
#'
#' @param seed random seed (integer)
#' @param scenario data generation scenario, 0, A, B, or C
#' @param output path to output file
#'
#' @export

run_one_replicate <- function(seed, scenario = "A", output = "repouput.rds") {

  if(!missing(seed)) set.seed(seed)

  indat <- generate_data(n = 500, scenario = scenario)
  validat <- add_pseudo_obs(generate_data(scenario = scenario))

  indat2 <- add_pseudo_obs(indat)

  slearn.fit <- superlearner_estimate(indat2, Y = "cause1.pseudo", X = paste0("X", 1:20))


  preder <- predict(slearn.fit, validat[, paste0("X", 1:20)])
  if(any(is.na(preder$pred[,1]))) {
    predres <- preder$library.predict[, which.min(slearn.fit$cvRisk)]
  } else {
    predres <- preder$pred[, 1]
  }

  ## back transform

  predres <- (exp(predres) * 2 - 1) / (exp(predres) + 1)

  mse.pseudo <- mean((validat$trueP - predres)^2)

  indat2$binY <- with(indat2, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))
  slearn.binfit <- superlearner_binaryestimate(subset(indat2, !is.na(binY)), Y = "binY", X = paste0("X", 1:20))

  mse.binary <- mean((validat$trueP - predict(slearn.binfit, validat[, paste0("X", 1:20)])$pred[, 1])^2)

  c(mse.pseudo = mse.pseudo, mse.binary = mse.binary)


}