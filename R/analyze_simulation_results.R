
analyze_sim <- function(scenario, folder, n = 200) {

  resem <- do.call(rbind, lapply(1:n, function(i) {

    fnam <- sprintf("data/%s/simres%s-%03d.rds", folder, scenario, i)
    res.tmp <- readRDS(fnam)
    out.tmp <- res.tmp[, -c(3:22)]
    out.tmp$replicate <- i
    out.tmp

  }))
  resem$binY <- with(resem, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))
  resem$trueY <- 1.0 * resem$trueT

  ### calculate roc using binary, roc using time dependent, and mse comparing to true probability

  res.auc <- NULL
  for(j in 1:n) {

    indat <- subset(resem, replicate == j)
    true.auc <- with(indat, c(performance(prediction(predres.pseudo, trueY), "auc")@y.values[[1]],
                              performance(prediction(predres.binary, trueY), "auc")@y.values[[1]]))

    td.auc <- with(indat, c(with(timedep_roc(delta, Tout, predres.pseudo), calc_auc(fpf, tpf)),
                            with(timedep_roc(delta, Tout, predres.binary), calc_auc(fpf, tpf))))

    bauc.pseudo <- with(subset(indat, !is.na(binY)),  performance(prediction(predres.pseudo, binY), "auc"))@y.values[[1]]
    bauc.binary <- with(subset(indat, !is.na(binY)),  performance(prediction(predres.binary, binY), "auc"))@y.values[[1]]

    pauc.pseudo <- with(indat, calc_pAUC(predres.pseudo, cause1.pseudo, cause2.pseudo))
    pauc.binary <- with(indat, calc_pAUC(predres.binary, cause1.pseudo, cause2.pseudo))

    bias.pseudo <- with(indat, mean((trueP - predres.pseudo)))
    bias.binary <- with(indat, mean(trueP - predres.binary))
    sd.pseudo <- with(indat, sd(predres.pseudo))
    sd.binary <- with(indat, sd(predres.binary))

    p1 <- lm(trueY ~ predres.pseudo, data = indat)
    b1 <- lm(trueY ~ predres.binary, data = indat)

    res.auc <- rbind(res.auc, data.frame(true.auc = true.auc,
                                        # td.auc = td.auc,
                                        # bauc = c(bauc.pseudo, bauc.binary),
                                         pauc = c(pauc.pseudo, pauc.binary),
                                        cal.int = c(p1$coefficients[1], b1$coefficients[1]),
                                        cal.slp = c(p1$coefficients[2], b1$coefficients[2]),
                                         bias = c(bias.pseudo, bias.binary),
                                         sd = c(sd.pseudo, sd.binary),
                                         model = c("pseudo", "binary")))

  }

  res.auc

}




analyze_sim_ppv <- function(scenario, folder, n = 200) {

  resem <- do.call(rbind, lapply(1:n, function(i) {

    fnam <- sprintf("data/%s/simres%s-%03d.rds", folder, scenario, i)
    res.tmp <- readRDS(fnam)
    out.tmp <- res.tmp[, -c(3:22)]
    out.tmp$replicate <- i
    out.tmp

  }))
  resem$binY <- with(resem, ifelse(Tout > 26.5, 0, ifelse(delta == 1, 1, NA)))
  resem$trueY <- 1.0 * resem$trueT

  ### calculate predictiveness curve using binary, using time dependent, and mse comparing to true probability

  res.auc <- NULL
  for(j in 1:n) {

    indat <- subset(resem, replicate == j)

    fitps <- glm(trueY ~ predres.pseudo, data = indat, family = "binomial")
    fitps2 <- glm(trueY ~ predres.binary, data = indat, family = "binomial")
    plot(predict(fitps, newdata = data.frame(predres.pseudo = sort(indat$predres.pseudo)), type = "response") ~ sort(indat$predres.pseudo) )
    lines(predict(fitps2, newdata = data.frame(predres.binary = sort(indat$predres.binary)), type = "response") ~ sort(indat$predres.binary) )

    true.auc <- with(indat, c(performance(prediction(predres.pseudo, trueY), "ppv")@y.values[[1]],
                              performance(prediction(predres.binary, trueY), "ppv")@y.values[[1]]))

    td.auc <- with(indat, c(with(timedep_roc(delta, Tout, predres.pseudo), calc_auc(fpf, tpf)),
                            with(timedep_roc(delta, Tout, predres.binary), calc_auc(fpf, tpf))))

    bauc.pseudo <- with(subset(indat, !is.na(binY)),  performance(prediction(predres.pseudo, binY), "auc"))@y.values[[1]]
    bauc.binary <- with(subset(indat, !is.na(binY)),  performance(prediction(predres.binary, binY), "auc"))@y.values[[1]]

    pauc.pseudo <- with(indat, calc_pAUC(predres.pseudo, cause1.pseudo))
    pauc.binary <- with(indat, calc_pAUC(predres.binary, cause1.pseudo))

    bias.pseudo <- with(indat, mean((trueP - predres.pseudo)))
    bias.binary <- with(indat, mean(trueP - predres.binary))
    sd.pseudo <- with(indat, sd(predres.pseudo))
    sd.binary <- with(indat, sd(predres.binary))

    res.auc <- rbind(res.auc, data.frame(true.auc = true.auc,
                                         td.auc = td.auc,
                                         bauc = c(bauc.pseudo, bauc.binary),
                                         pauc = c(pauc.pseudo, pauc.binary),
                                         bias = c(bias.pseudo, bias.binary),
                                         sd = c(sd.pseudo, sd.binary),
                                         model = c("pseudo", "binary")))

  }

  res.auc

}



