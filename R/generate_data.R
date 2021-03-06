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
#'                 D = same as B but censoring depends on covariates
#' @param missing.p Proportion of missing binary Y values (random censoring)
#'
#' @return A data frame with X variables, censored survival times (competing risk w death), and true cumulative incidence at 26.5 weeks
#'
#' @export


generate_data <- function(n = 500, scenario = "A", missing.p = .2) {

  X5 <- matrix(rnorm(n * 5, mean = 0, sd = .1), ncol = 5)
  X52 <- X5 %*% matrix(.25, nrow = 5, ncol = 5) + matrix(rnorm(n * 5, mean = 0, sd = .1), ncol = 5)
  X53 <- X52 %*% matrix(.15, nrow = 5, ncol = 5) + matrix(rnorm(n * 5, mean = 0, sd = .5), ncol = 5)
  X54 <- X53 %*% matrix(.05, nrow = 5, ncol = 5) + matrix(rnorm(n * 5, mean = 0, sd = .65), ncol = 5)

  X <- cbind(X5, X52, X53, X54)

  if(scenario == "0") {

    g1 <- exp(rnorm(n, mean = 0, sd = .35))

  } else if(scenario == "A") {

    g1 <- exp(-2 + 2.5 * X[, 1])

  } else if(scenario == "B") {

    beta.b <- c(.75, .5, 6.1, 1.02, -2.03, 1, 2, 1, .6, .1, 3)

    x.in <- cbind(X[, c(1, 6)], X[, 1] * X[, 6], bs(X[, 6], df = 4, degree = 3), bs(X[, 1], df = 4, degree = 3))

    g1 <- sqrt(exp(6 + x.in %*% beta.b ))#+ cos(X[, 4] / .1) * X[, 3]) + X[, 4]^3 * X[, 1])


  } else if(scenario == "C") {

    X2 <- X[, c(1, 6, 11, 16, 20)]
    X2 <- cbind(X2, X2[, 1] * X2[, 2],
                cos(X2[, 3] / .1),
                X2[, 4] * ifelse(X2[, 4] < median(X2[, 4]), 0, 1))


    beta.c <- c(1.1, 1.4, -2.1, -1.2, -2.3, -1.5, 6.7, .5) / 4

    g1 <- sqrt(exp(X2 %*% beta.c))

  } else if(scenario == "D") { # model in which censoring depends on a covariate

    X2 <- X[, c(1, 6, 11, 16, 20)]
    X2 <- cbind(X2, X2[, 1] * X2[, 2],
                cos(X2[, 3] / .1),
                X2[, 4] * ifelse(X2[, 4] < median(X2[, 4]), 0, 1))


    beta.c <- c(1.1, 1.4, -2.1, -1.2, -2.3, -1.5, 6.7, .5) / 4

    g1 <- sqrt(exp(X2 %*% beta.c))

    beta.b <- beta.c[1:5] * 4

    censb1 <- sqrt(exp( X[, c(1, 6, 11, 16, 20)] %*% beta.b / 3))
    cskl <- (26.5 / ((-log(1 - .14)) ^ (1)))
    censb1 <- censb1 * mean(cskl / censb1)

  } else if(scenario == "E") { # sampling from real data example

    crohnraw <- readRDS("data/crohnraw.rds")
    colnames(crohnraw)[4:23] <- paste0("X", 1:20)

    outrows <- sample(1:nrow(crohnraw), n, replace =TRUE)

    return(crohnraw[outrows, ])

  }

  if(scenario != "E") {

    k2 <- 2.5
    g2 <- 3.5
    k1 <- exp(2.5 * X[, 2])

  }
  # ensure that g1 satisfies pweibull(26.5) ~= .20
  # ensure that k1 satisfies pweibull(26.5) ~= .07

  rescl <- (26.5 / ((-log(.8)) ^ (1/g2)))
  reskl <- (26.5 / ((-log(1 - .07)) ^ (1 / k2)))

  g1 <- g1 * mean(rescl / (g1))
  k1 <- k1 * mean(reskl / (k1))

  Y <- rweibull(n, scale = g1, shape = g2)
  Y2 <- rweibull(n, scale = k1, shape = k2)

  Cen <- runif(n, quantile(c(Y, Y2), .05), max(c(Y, Y2)))

  if(scenario == "D"){

    Cen <- rweibull(n, scale = censb1, shape = 1)

  }

  Tout <- pmin(Y, Y2, Cen)
  delta <- ifelse(Cen < Y & Cen < Y2, 0,
                  ifelse(Y < Y2, 1, 2))

  ## addtional random censoring
  makeup <- missing.p - mean(delta == 0)
  if(makeup > .01) {
    randcens <- sample(which(delta != 0), size = floor(sum(delta != 0) * makeup))
    delta[randcens] <- 0
    Tout[randcens] <- runif(length(randcens), 0, 26.5)
  }

  #return(mean(delta[Tout < 26.5] == 0))

  trueP <- sapply(1:n, function(i) {
  integrate(f = function(x){
    (pweibull(x, scale = g1[i], shape = g2)) *
              dweibull(x, scale = k1[i], shape = k2)
    },
            -Inf, 26.5)$value
  }) +
    sapply(1:n, function(i) {
      integrate(f = function(x){
        (pweibull(26.5, scale = g1[i], shape = g2)) *
          dweibull(x, scale = k1[i], shape = k2)
      },
      26.5, Inf)$value
    })


  X <- apply(X, MAR = 2, standardize)

  data.frame(id = 1:length(Tout), Tout, delta, X, trueT = Y < 26.5 & Y < Y2,
             trueP, Cen, Y, Y2)



}

#' Add pseudo observations to a data frame
#'
#' Allow arbitrary names
#'
#' @param data A data frame with censored survival times for 2 outcomes
#' @param tme Vector of times at which to compute pseudo values
#' @return The data frame with pseudo observations added for t = 26.5 weeks
#'
#'
#' @export



add_pseudo_obs <- function(data, tme = c(17.5, 20, 26.5, 35)) {


  psuo <- pseudoci(data$Tout, event = data$delta, tmax = tme)

  data <- do.call(rbind, lapply(1:length(tme), function(x) cbind(data, cause1.pseudo = psuo$pseudo$cause1[, x],
                                                         cause2.pseudo = psuo$pseudo$cause2[, x], time = tme[x])))

  data

}


