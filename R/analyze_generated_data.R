#' Run the superlearner model with pseudo observations as the outcome
#'
#' @param formula A formula describing the model
#' @param data A data frame with pseudo observations and a set of predictors
#'
#' @return A fitted model object
#'
#' @export



superlearner_estimate <- function(data, Y = "cause1.pseudo", X = paste0("X", 1:20), y0, y1, Y2 = "cause2.pseudo") {

  XX <- data[, X]
  YY <- data[[Y]] #log((data[[Y]] - y0) / (y1 - data[[Y]]))

  Pseu2 <- data[[Y2]]

  tune = list(ntrees = c(100, 200),
              max_depth = 1:3,
              shrinkage = c(0.001, 0.01, 0.1))

  # Set detailed names = T so we can see the configuration for each function.
  # Also shorten the name prefix.
  learners = create.Learner("SL.xgboost", tune = tune, detailed_names = T, name_prefix = "xgb")

  # 36 configurations - not too shabby.
  length(learners$names)


  SL.library <-  append(list(c("SL.glm", "screen.corP"), "SL.step", "SL.randomForest", c("SL.gam", "screen.corP"),  "SL.ksvm", "SL.cforest",
                      "SL.rpart", "SL.glmnet" , c("SL.polymars", "screen.corP")), learners$names)

  sl.full <- SuperLearner(Y = YY, X = XX, SL.library = SL.library,
                          verbose = FALSE, method = "method.pseudoAUC", control = list(pseu2 = Pseu2))


  sl.full

}


superlearner_binaryestimate <- function(data, Y = "binY", X = paste0("X", 1:20)) {

  XX <- data[, X]
  YY <- data[[Y]]

  tune = list(ntrees = c(100, 200),
              max_depth = 1:3,
              shrinkage = c(0.001, 0.01, 0.1))

  # Set detailed names = T so we can see the configuration for each function.
  # Also shorten the name prefix.
  learners = create.Learner("SL.xgboost", tune = tune, detailed_names = T, name_prefix = "xgb")

  # 36 configurations - not too shabby.
  length(learners$names)


  SL.library <-  c("SL.glm", "SL.randomForest", "SL.gam",  "SL.ksvm", "SL.cforest",
                   "SL.rpart", "SL.glmnet","SL.polymars", learners$names)


  sl.full <- SuperLearner(Y = YY, X = XX, SL.library = SL.library, family = binomial(),
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



