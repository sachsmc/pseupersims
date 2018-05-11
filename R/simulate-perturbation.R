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

run_one_perturb <- function(seed, scenario = "A", missing.p = .2,  output = "repouput.rds") {

  if(!missing(seed)) set.seed(seed)

  indat <- generate_data(n = 500, scenario = scenario, missing.p = missing.p)
  validat <- subset(add_pseudo_obs(generate_data(scenario = scenario, missing.p = missing.p)), time == 26.5)

  indat2 <- add_pseudo_obs(indat)

  slearn.fit <- superlearner_estimate(indat2, Y = "cause1.pseudo", X = c("time", paste0("X", 1:20)),  Y2 = "cause2.pseudo")


  inlim <- subset(indat2[, c("time", paste0("X", 1:20))], time == 26.5)
  inlim <- inlim[sample(1:nrow(inlim), 100),]
  lime_est <- pseudo_lime(slearn.fit, X = inlim)$predup[, -1]
  validat <- colMeans(lime_est)

  saveRDS(lime_est, file = output)


}
