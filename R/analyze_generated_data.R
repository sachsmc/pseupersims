#' Run the superlearner model with pseudo observations as the outcome
#'
#' @param data A data frame with pseudo observations and a set of predictors
#' @param Y character string for the outcome variable in data
#' @param X character vector for the predictor variables in data
#' @param Y2 character string for the pseudo-observations of the competing outcome
#' @param SL.library optional list of SL library methods
#'
#' @return A fitted model object
#'
#' @export



superlearner_estimate <- function(data, Y = "cause1.pseudo", X = paste0("X", 1:20), Y2 = "cause2.pseudo", SL.library = NULL) {

  XX <- data[, X]
  YY <- data[[Y]] #log((data[[Y]] - y0) / (y1 - data[[Y]]))

  Pseu2 <- data[[Y2]]

  tune = list(ntrees = c(200),
              max_depth = 2,
              shrinkage = c(0.01, 0.1, .2))

  # Set detailed names = T so we can see the configuration for each function.
  # Also shorten the name prefix.
  learners = create.Learner("SL.xgboost", tune = tune, detailed_names = T, name_prefix = "xgb")

  # 36 configurations - not too shabby.
  length(learners$names)

  if(is.null(SL.library)) {

  SL.library <-  append(list(c("SL.glm", "screen.corP"), "SL.step", c("SL.gam", "screen.corP"),  "SL.ksvm", "SL.ranger",
                      "SL.rpart", "SL.glmnet" , c("SL.polymars", "screen.corP")), learners$names)

  }

  sl.full <- SuperLearner(Y = YY, X = XX, SL.library = SL.library, id = data$id,
                          verbose = FALSE, method = "method.pseudoAUC", control = list(pseu2 = Pseu2, timedex = data$time == 26.5))


  sl.full

}


superlearner_binaryestimate <- function(data, Y = "binY", X = paste0("X", 1:20)) {

  XX <- data[, X]
  YY <- data[[Y]]

  tune = list(ntrees = c(200),
              max_depth = 2,
              shrinkage = c(0.01, 0.1, .2))
  # Set detailed names = T so we can see the configuration for each function.
  # Also shorten the name prefix.
  learners = create.Learner("SL.xgboost", tune = tune, detailed_names = T, name_prefix = "xgb")

  # 36 configurations - not too shabby.
  length(learners$names)


  SL.library <-  c("SL.glm",  "SL.gam",  "SL.ksvm", "SL.ranger",
                   "SL.rpart", "SL.glmnet","SL.polymars", learners$names)


  sl.full <- SuperLearner(Y = YY, X = XX, SL.library = SL.library, family = binomial(), obsWeights = data$censW, id = data$id,
                          verbose = FALSE, method = "method.NNloglik")


  sl.full

}




stupidlearner_estimate <- function(data, Y = "cause1.pseudo", X = paste0("X", 1:20), y0, y1) {

  XX <- data[, X]
  YY <- data[[Y]] #log((data[[Y]] - y0) / (y1 - data[[Y]]))

  tmpd <- data.frame(Y = YY, XX)
  fit0 <- lm(Y ~ ., data = tmpd)

  fit1 <- step(fit0, trace = 0)
  fit1

}


stupidlearner_binaryestimate <- function(data, Y = "binY", X = paste0("X", 1:20), y0, y1) {

  XX <- data[, X]
  YY <- data[[Y]] #log((data[[Y]] - y0) / (y1 - data[[Y]]))

  tmpd <- data.frame(Y = YY, XX)
  fit0 <- glm(Y ~ ., data = tmpd, family = "binomial")

  fit1 <- step(fit0, trace = 0)
  fit1

}



