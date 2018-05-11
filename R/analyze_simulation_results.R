
analyze_sim <- function(scenario, folder, n = 200) {

  resem <- do.call(rbind, lapply(1:n, function(i) {

    fnam <- sprintf("data/%s/simres%s-%03d.rds", folder, scenario, i)
    if(!file.exists(fnam)) return(NULL)
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
    if(nrow(indat) == 0) next
    true.auc <- with(indat, c(performance(prediction(predres.pseudo, trueY), "auc")@y.values[[1]],
                              performance(prediction(predres.binary, trueY), "auc")@y.values[[1]],
                              performance(prediction(trueP, trueY), "auc")@y.values[[1]]))


    pauc.pseudo <- with(indat, calc_pAUC(predres.pseudo, cause1.pseudo, cause2.pseudo))
    pauc.binary <- with(indat, calc_pAUC(predres.binary, cause1.pseudo, cause2.pseudo))
    pauc.cox <- with(indat, calc_pAUC(trueP, cause1.pseudo, cause2.pseudo))

    bias.pseudo <- with(indat, mean((trueP - predres.pseudo)))
    bias.binary <- with(indat, mean(trueP - predres.binary))
    bias.cox <- with(indat, mean(trueP - trueP))
    sd.pseudo <- with(indat, sd(predres.pseudo))
    sd.binary <- with(indat, sd(predres.binary))
    sd.cox <- with(indat, sd(exp(trueP)))

    p1 <- lm(trueY ~ predres.pseudo, data = indat)
    b1 <- lm(trueY ~ predres.binary, data = indat)
    c1 <- lm(trueY ~ exp(trueP), data = indat)

    res.auc <- rbind(res.auc, data.frame(true.auc = true.auc,
                                          pauc = c(pauc.pseudo, pauc.binary, pauc.cox),
                                        cal.int = c(p1$coefficients[1], b1$coefficients[1], c1$coefficients[1]),
                                        cal.slp = c(p1$coefficients[2], b1$coefficients[2], c1$coefficients[2]),
                                         bias = c(bias.pseudo, bias.binary, bias.cox),
                                         sd = c(sd.pseudo, sd.binary, sd.cox),
                                         model = c("pseudo", "binary", "true")))

  }

  res.auc

}




