#' Perturbation algorithm to get local slopes
#'
#' Given a fitted model object with a predict method, run a perturbation algorithm
#' that estimates the local slope of the predicted outcome for each predictor in the
#' model and for each subject in the dataset.
#'
#' @param fitted The fitted model object, e.g. from superlearner_estimate
#' @param data A dataframe to be perturbed
#' @param predictors the predictors used the the model
#'
#' @return A data frame with the local slopes for each variable (columns) and each subject (rows)
#'
#' @export


local_slope <- function(fitted, data, predictors) {



}