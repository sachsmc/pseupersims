#' Generate survival data for analysis
#'
#' 500 subjects, 20 variables, about 20% cumulative incidence. 5 variables truly associated,
#' some linear, some wonky associations, an interaction, and some variables correlated.
#'
#' @param n Sample size
#' @param scenario Character designating the simulation scenario: 0 = NULL, no association, A = simple, one variable
#'                 is linearly associated with IRIS,
#'                 B = simple multivariate, 5 variables are linearly associated,
#'                 C = wonky, interactions, nonlinearities, etc.
#'
#' @return A data frame with X variables, censored survival times (competing risk w death), and true cumulative incidence at 26.5 weeks
#'
#' @export


generate_data <- function(n = 500, scenario = "A") {

  X5 <- matrix(rnorm(n * 5, mean = 1, sd = .1), ncol = 5)
  X52 <- X5 %*% matrix(0, nrow = 5, ncol = 5) + matrix(rnorm(n * 5, mean = 1, sd = .1), ncol = 5)
  X53 <- X52 %*% matrix(0, nrow = 5, ncol = 5) + matrix(rnorm(n * 5, mean = 1, sd = .5), ncol = 5)
  X54 <- X53 %*% matrix(.05, nrow = 5, ncol = 5) + matrix(rnorm(n * 5, mean = 1, sd = .65), ncol = 5)

  X <- cbind(X5, X52, X53, X54)

  if(scenario == "0") {

    g1 <- exp(1)

  } else if(scenario == "A") {

    g1 <- exp(2 * X[, 1])

  } else if(scenario == "B") {

    beta.b <- c(1.1, 1.1, 1.01, -3.02, 2.03) / 3

    g1 <- exp(X[, c(1, 6, 11, 16, 20)] %*% beta.b)


  } else if(scenario == "C") {

    X2 <- X[, c(1, 6, 11, 16, 20)]
    X2 <- cbind(X2, X2[, 1] * X2[, 2],
                cos(X2[, 3] / .1),
                X2[, 4] * ifelse(X2[, 4] < median(X2[, 4]), 0, 1))


    beta.c <- c(2.1, 2.4, -4.1, -1.2, -4.3, 1.5, 6.7, 1.5) / 3

    g1 <- exp(X2 %*% beta.c)

  }

  k2 <- 2.5
  g2 <- 3.5
  k1 <- exp(.5 * X[, 1])

  # ensure that g1 satisfies pweibull(26.5) ~= .20
  # ensure that k1 satisfies pweibull(26.5) ~= .07

  rescl <- (26.5 / ((-log(.8)) ^ (1/g2)))
  reskl <- (26.5 / ((-log(1 - .07)) ^ (1 / k2)))

  g1 <- g1 * mean(rescl / (g1))
  k1 <- k1 * mean(reskl / (k1))

  Y <- rweibull(n, scale = g1, shape = g2)
  Y2 <- rweibull(n, scale = k1, shape = k2)

  Cen <- runif(n, quantile(c(Y, Y2), .5), max(c(Y, Y2)))

  Tout <- pmin(Y, Y2, Cen)
  delta <- ifelse(Cen < Y & Cen < Y2, 0,
                  ifelse(Y < Y2, 1, 2))

  trueP <- pweibull(26.5, scale = g1, shape = g2)

  data.frame(Tout, delta, X, trueT = Y < 26.5,
             trueP, Cen, Y, Y2)



}

#' Add pseudo observations to a data frame
#'
#' Allow arbitrary names
#'
#' @param data A data frame with censored survival times for 2 outcomes
#' @param weights Vector of probability weights
#' @return The data frame with pseudo observations added for t = 26.5 weeks
#'
#'
#' @export

add_pseudo_obs <- function(data, weights = rep(1, nrow(data))) {

  psuo <- pseudoci.weighted(data$Tout, event = data$delta, tmax = 26.5, weights = weights)

  data$cause1.pseudo <- psuo$pseudo$cause1[, 1]  # this is the one we're analyzing
  data$cause2.pseudo <- psuo$pseudo$cause2[, 1]

  data

}
