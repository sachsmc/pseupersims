
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

  indat <- generate_data()

  indat2 <- add_pseudo_obs(indat, time = 26.5)

  slearn.fit <- superlearner_estimate(indat2)
  cox.fit <- pcox_estimate(indat2)

  pturb.est <- local_slope(slearn.fit)



}