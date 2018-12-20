

standardize <- function(x) {

  (x - mean(x)) / sd(x)

}


#' Compute the time-varying cause-specific ROC curve using pseudo-observations
#'
#' @param X Vector of predictions
#' @param pseu Pseudo observations at a fixed time for the cause of interest
#' @param pseu2 Pseudo observations for the other cause
#'
#' @export

calc_roc <-
  function(X, pseu, pseu2) {
    ## compute roc curve using pseudo values

    cuts <- sort(unique(X))
    cutin <- cuts[-c(1, length(cuts))]
    res.0 <- rbind(c(1, 1), t(sapply(cutin, function(cth) {
      c(sum(pseu * as.numeric(X > cth)) / sum(pseu),
        sum((1 - pseu - pseu2) * as.numeric(X > cth)) / sum(1 - pseu - pseu2))
    })), c(0, 0))
    res <- as.data.frame(res.0)
    res <- data.frame(lapply(res, function(x)
      pmax(pmin(x, 1), 0)))
    colnames(res) <- c("tpf", "fpf")
    res

  }


timedep_roc <- function(delta, Tout, MM, t.star = 26.5){

  d.cox <- 1.0 * (delta == 1)
  fit.m <- coxph(Surv(Tout, d.cox) ~ MM)

  R.t <- Tout > t.star
  risk <- predict(fit.m, type = "risk")
  W.t <- sum(R.t * risk)
  pi.k <- (R.t * risk) / W.t

  cc.all <- sort(MM)

  TP.t <- sapply(cc.all, function(cc) {

    sum((MM > cc) * pi.k)

  })

  FP.t <- sapply(cc.all, function(cc) {

    sum((MM > cc) * R.t) / sum(R.t)

  })

  data.frame(tpf = TP.t, fpf = FP.t)

}


calc_auc <- function(fpf, tpf) {
  n <- length(fpf)
  dx <- fpf[-n] - fpf[-1]
  mid.y <- (tpf[-n] + tpf[-1]) / 2
  sum(dx * mid.y)

}

calc_pAUC <- function(predictions, labels, pseu2) {

  if(length(unique(predictions)) < 3) return(.5)
  inroc <- calc_roc(predictions, labels, pseu2)
  calc_auc(inroc$fpf, inroc$tpf)

}


#' Optimize the time varying AUC
#'
#' @param Z Matrix of predictions
#' @param Y Pseudo values for the cumulative incidence for the competing event of interest at a fixed time
#' @param pseu2 Matrix of pseudo values for the cumulative incidences for all other competing events

optimize_auc <-
  function(Z, Y, pseu2) {
    ## Z is a matrix of predictions, Y are pseudo-observations
    ## pseu2 are the pseudo observations for the competing risk
    ## compute the linear combination of Z that maximizes the AUC


    tomax <- function(beta) {

      beta <- beta
      Xin <- ( Z %*% beta)
      roc <- calc_roc(Xin, Y, pseu2)
      (1 - (calc_auc(roc$fpf, roc$tpf))) + 100 * sum(abs(beta))

    }

    trag <- optim(rep(0.01, ncol(Z)), tomax, method = "BFGS")

    if (trag$convergence == 0) {
      trag$par / sum(trag$par)

    } else {

      stop("Optimization of AUC failed to converge")
    }

  }

my.SuperLearner.control <- function(saveFitLibrary = TRUE, trimLogit = 0.001, ...) {
  if(trimLogit > 0.5) {
    warning('trimLogit must be less than 0.5, will replace with trimLogit = 0.001')
    trimLogit <- 0.001
  }
  list(saveFitLibrary = saveFitLibrary, trimLogit = trimLogit, ...)
}

assignInNamespace("SuperLearner.control", my.SuperLearner.control, "SuperLearner")

#' Pseudo AUC SuperLearner method
#'
#' To be passed to the SuperLearner function
#'
#' @export

method.pseudoAUC <- function() {
  out <- list(
    # require allows you to pass a character vector with required packages
    # use NULL if no required packages
    require = NULL,
    # computeCoef is a function that returns a list with three elements:
    # 1) coef: the weights (coefficients) for each algorithm
    # 2) cvRisk: the V-fold CV risk for each algorithm
    # 3) optimizer: (optional) the result object from the optimization of the weights.
    computeCoef = function(Z,
                           Y,
                           libraryNames,
                           obsWeights,
                           control,
                           verbose,
                           ...) {
      cvRisk <- apply(Z[control$timedex, ], 2, function(x) {
        rocin <- calc_roc(x, Y[control$timedex], control$pseu2[control$timedex])
        1 - (calc_auc(rocin$fpf, rocin$tpf))
      })

      coef <- tryCatch(optimize_auc(Z[control$timedex, ], Y[control$timedex], control$pseu2[control$timedex]), error = function(e) rep(NA, ncol(Z)),
                       warning = function(e) rep(NA, ncol(Z)))
      out <- list(cvRisk = cvRisk,
                  coef = coef,
                  optimizer = NULL)
      return(out)

    },
    # computePred is a function that takes the weights and the predicted values from each algorithm in the library and combines them based on the model to output the super learner predicted values
    computePred = function(predY, coef, control, ...) {
      out <- crossprod(t(predY), coef)
      return(out)
    }
  )
  invisible(out)
}



logit <- function(x) log(x / (1 - x))

retrans <- function(po, theta) {
  nn <- length(po)

  theta.i <- (theta * nn - po) / (nn - 1)

  nn * logit(theta) - (nn - 1) * logit(theta.i)

}