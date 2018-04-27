#' Perturbation algorithm to get local slopes
#'
#' Given a fitted model object with a predict method, run a perturbation algorithm
#' that estimates the local slope of the predicted outcome for each predictor in the
#' model and for each subject in the dataset.
#'
#' @param SLobject The fitted model object, e.g. from superlearner_estimate
#' @param X A dataframe to be perturbed
#' @param predictor the predictors used the the model
#'
#' @return A data frame with the local slopes for each variable (columns) and each subject (rows)
#'
#' @export
#'
shift_pred  <- function(SLobject, X, predictor) {

  Xu <- X
  Xd <- X

  chekit <- table(X[, predictor])

  if(length(chekit) > 2){
    sd <- sd(X[, predictor])

    Xd[, predictor] <- X[, predictor] - sd
    Xu[, predictor] <- X[, predictor] + sd
  } else if(length(chekit) == 2){

    cands <- sort(unique(X[, predictor]))

    Xd[, predictor] <- factor(ifelse(XX[, predictor]==cands[1], cands[2], cands[1]))
    Xu[, predictor] <- factor(ifelse(XX[, predictor]==cands[1], cands[2], cands[1]))
  }

  pu <- predict(SLobject, Xu)$pred
  pd <- pu

  p <- list(pu = pu, pd = pd)

}



pseudo_lime <- function(SLobject, X) {
  Xnumcols <- dim(X)[2]
  # Xnumcols <- 1
  Xnames <- names(X)

  predup <- as.data.frame(matrix(NA, dim(X)[1], dim(X)[2]))
  colnames(predup) <- Xnames
  preddown <- as.data.frame(matrix(NA, dim(X)[1], dim(X)[2]))
  colnames(preddown) <- Xnames

  orig_pred <- predict(SLobject, X)

  for (i in 1:Xnumcols) {
    sp <- shift_pred(SLobject, X, i)
    predup[, i] <- sp[["pu"]] - orig_pred$pred
    preddown[, i] <- sp[["pd"]] - orig_pred$pred
  }

  pred <- list(predup = predup, preddown = preddown, orig_pred = orig_pred)
}

