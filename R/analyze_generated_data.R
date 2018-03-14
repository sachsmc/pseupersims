#' Run the superlearner model with pseudo observations as the outcome
#'
#' @param formula A formula describing the model
#' @param data A data frame with pseudo observations and a set of predictors
#'
#' @return A fitted model object
#'
#' @export



superlearner_estimate <- function(data, Y = "cause1.pseudo", X = paste0("X", 1:20)) {

  XX <- as.data.frame(lapply(data[, X], standardize))
  YY <- data[[Y]]


  SL.library <-  list("SL.glm", "SL.randomForest", "SL.gam",  "SL.ksvm", "SL.cforest",
                      "SL.rpart", "SL.xgboost", "SL.glmnet","SL.polymars")

  sl.full <- SuperLearner(Y = YY, X = XX, SL.library = SL.library,
                          verbose = FALSE, method = "method.pseudoAUC")


  sl.full

}


superlearner_binaryestimate <- function(data, Y = "binY", X = paste0("X", 1:20)) {

  XX <- as.data.frame(lapply(data[, X], standardize))
  YY <- data[[Y]]


  SL.library <-  list("SL.glm", "SL.randomForest", "SL.gam",  "SL.ksvm", "SL.cforest",
                      "SL.rpart", "SL.xgboost", "SL.glmnet","SL.polymars")

  sl.full <- SuperLearner(Y = YY, X = XX, SL.library = SL.library,
                          verbose = FALSE, method = "method.pseudoAUC")


  sl.full

}


#' Run the a penalized cox regression model
#'
#' @param formula A formula describing the model
#' @param data A data frame with a survival outcome and a set of predictors
#'
#' @return A fitted model object
#'
#' @export



pcox_estimate <- function(data, Y = "cause1.pseudo", X = paste0("X", 1:20)) {


}
