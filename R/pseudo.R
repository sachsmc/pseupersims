#' Pseudo observations for the weighted cumulative incidence function
#'
#' Computes pseudo-observations for modeling competing risks based on the possibly weighted cumulative incidence function
#'
#' @param time follow up time
#' @param event cause indicator, use 0 as censoring code and integers for other causes
#' @param tmax a vector of time points at which pseudo-observations are to be computed. If missing, uses each event time
#' @param weights a vector of case weights, i.e., 1/probabilities
#'
#' @return A list containing the following objects
#' \describe{
#'   \item{time}{The ordered time points at which the pseudo-observations are evaluated.}
#'   \item{cause}{The ordered codes for different causes.}
#'   \item{pseudo}{A list of matrices - a matrix for each of the causes, ordered by codes. Each row of a matrix belongs to one individual (ordered as in the original data set), each column presents a time point (ordered in time).}
#' }
#' @export
#' @import survival

pseudoci.weighted <- function (time, event, tmax, weights = rep(1, length(time)))
{
  if (any(is.na(time)))
    stop("missing values in 'time' vector")
  if (any(time < 0))
    stop("'time' must be nonnegative")
  if (any(is.na(event)))
    stop("missing values in 'event' vector")
  if (any(event < 0))
    stop("Events should be denoted by integer values, 0 is used for censoring")
  if (missing(tmax))
    tmax <- unique(time[event != 0])
  if (any(is.na(tmax)))
    stop("missing values in 'tmax' vector")
  if (sum(tmax > max(time)) > 0)
    stop("tmax greater than largest observation time")
  tmax <- sort(tmax)
  ltmax <- length(tmax)
  howmany <- length(time)
  causes <- sort(unique(event[event != 0]))
  ncauses <- length(causes)
  pseudo <- data.frame(id = 1:howmany, time = time, event = event)
  pseudo <- pseudo[order(pseudo$time, -pseudo$event), ]
  tu <- unique(pseudo$time[pseudo$event != 0])
  ltu <- length(tu)
  tu <- matrix(tu, byrow = TRUE, ncol = ltu, nrow = ltmax)
  tgiven <- matrix(tmax, byrow = FALSE, ncol = ltu, nrow = ltmax)
  inx <- apply(tgiven >= tu, 1, sum)
  pseu <- ci.omit.weighted(pseudo, causes = causes, weights = weights)
  pseut <- ci.tot.weighted(pseudo, causes = causes, weights = weights)
  out <- NULL
  out$time <- tmax
  out$cause <- causes
  out$pseudo <- list(rep(NA, ncauses))
  for (jt in 1:ncauses) {
    ci <- pseu[[jt]][, inx, drop = FALSE]
    citot <- matrix(pseut[[jt]][inx], byrow = TRUE, ncol = ncol(ci),
                    nrow = nrow(ci))
    ps <- as.data.frame(howmany * citot - (howmany - 1) *
                          ci)
    row.names(ps) <- pseudo$id
    names(ps) <- paste("time", tmax, sep = ".")
    out$pseudo[[jt]] <- as.matrix(ps[order(pseudo$id), ])
  }
  names(out$pseudo) <- paste("cause", causes, sep = "")
  out
}


ci.tot.weighted <- function (pseudo, tmax, causes, weights) {

  sfit <- survfit(Surv(pseudo$time, pseudo$event, type = "mstate") ~ 1, data = NULL,
          weights = weights, id = pseudo$id)

  ncauses <- length(causes)
  CI <- list(rep(NA, ncauses))
  if(!missing(tmax))ciall <- summary(sfit, time = tmax) else {
    ciall <- summary(sfit)
  }
  for(j in 1:ncauses) {

    CI[[j]] <- ciall$pstate[, j]

  }
  CI
}


ci.omit.weighted <- function (pseudo, tmax, causes, weights) {

  tmpdat <- cbind(pseudo, weights)
  omitdat <- lapply(1:nrow(tmpdat), function(i) tmpdat[-i, ])
  maxflag <- !missing(tmax)
  sfitout <- survfit(Surv(time, event, type = "mstate") ~ 1, data = tmpdat,
                    weights = weights, id = id)
  etimes <- summary(sfitout)$time

  omit.fits <- lapply(omitdat, function(dat) {

    sfitin <- survfit(Surv(time, event, type = "mstate") ~ 1, data = dat,
            weights = weights, id = id)

    if(maxflag){
      summary(sfitin, time = tmax)$pstate
      } else {
      summary(sfitin, time = etimes)$pstate
    }
  })

  ncauses <- length(causes)
  CI <- list(rep(NA, ncauses))
  for(j in 1:ncauses) {

    CI[[j]] <- do.call(rbind, lapply(omit.fits, function(x) x[, j]))

  }
  CI

}

