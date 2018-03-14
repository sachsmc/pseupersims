
#' Run one replicate of the experiment
#'
#' Generates data, analyzes the results in different ways, runs the perturbation algorithm,
#' and returns the output in a sensible manner.
#'
#' @param seed random seed (integer)
#' @param output path to output file
#'
#' @export

run_one_replicate <- function(seed = floor(runif(1 * 1000)), output = "repouput.rds") {

  indat <- generate_data(n = 500, scenario = "A")
  validat <- add_pseudo_obs(generate_data(scenario = "A"))

  indat2 <- add_pseudo_obs(indat)

  slearn.fit <- superlearner_estimate(indat2, Y = "cause1.pseudo", X = paste0("X", 1:20))


  plot(validat$trueP ~ predict(slearn.fit, validat[, paste0("X", 1:20)])$pred[, 1])
  plot(indat$trueP ~ predict(slearn.fit)$pred[, 1])


  indat2$binY <- with(indat2, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))



}