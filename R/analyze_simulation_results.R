analyze_sim <- function(scenario) {

  resem <- do.call(rbind, lapply(1:200, function(i) {

    fnam <- sprintf("data/simres%s-%03d.rds", scenario, i)
    res.tmp <- readRDS(fnam)
    out.tmp <- res.tmp[, -c(3:22)]
    out.tmp$replicate <- i
    out.tmp

  }))
  resem$binY <- with(resem, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))
  resem$trueY <- 1.0 * (resem$Y < 26.5)

  ### calculate roc using binary, roc using time dependent, and mse comparing to true probability

  res.auc <- NULL
  for(j in 1:200) {

    indat <- subset(resem, replicate == j)
    true.auc <- with(indat, c(performance(prediction(predres.pseudo, trueY), "auc")@y.values[[1]],
                              performance(prediction(predres.binary, trueY), "auc")@y.values[[1]]))

    bauc.pseudo <- with(subset(indat, !is.na(binY)),  performance(prediction(predres.pseudo, binY), "auc"))@y.values[[1]]
    bauc.binary <- with(subset(indat, !is.na(binY)),  performance(prediction(predres.binary, binY), "auc"))@y.values[[1]]

    pauc.pseudo <- with(indat, calc_pAUC(predres.pseudo, cause1.pseudo))
    pauc.binary <- with(indat, calc_pAUC(predres.binary, cause1.pseudo))

    bias.pseudo <- with(indat, mean((trueP - predres.pseudo)))
    bias.binary <- with(indat, mean(trueP - predres.binary))
    sd.pseudo <- with(indat, sd(predres.pseudo))
    sd.binary <- with(indat, sd(predres.binary))

    res.auc <- rbind(res.auc, data.frame(true.auc = true.auc,
                                         bauc = c(bauc.pseudo, bauc.binary),
                                         pauc = c(pauc.pseudo, pauc.binary),
                                         bias = c(bias.pseudo, bias.binary),
                                         sd = c(sd.pseudo, sd.binary),
                                         model = c("pseudo", "binary")))

  }

  res.auc

}




