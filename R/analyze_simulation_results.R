#' Analyze simulation results
#'
#' @export


analyze_sim <- function(scenario, folder, n = 200) {

  resem <- do.call(rbind, lapply(1:n, function(i) {

    fnam <- sprintf("data/%s/simres%s-%03d.rds", folder, scenario, i)
    if(!file.exists(fnam)) return(NULL)
    res.tmp <- readRDS(fnam)
    out.tmp <- res.tmp[, -c(4:23)]
    out.tmp$replicate <- i
    out.tmp

  }))
  resem$binY <- with(resem, ifelse(Tout > 26.5, 0, ifelse(delta == 0, NA, ifelse(delta == 1, 1, 0))))
  resem$trueY <- 1.0 * resem$trueT
  if(scenario == "E"){
	resem$trueY <- rbinom(nrow(resem), 1, .4)
	resem$trueP <- rnorm(nrow(resem))
	}
  ### calculate roc using binary, roc using time dependent, and mse comparing to true probability

  res.auc <- NULL
  for(j in 1:n) {

    indat <- subset(resem, replicate == j)
    if(nrow(indat) == 0) next
    true.auc <- with(indat, c(performance(prediction(predres.pseudo, trueY), "auc")@y.values[[1]],
                              performance(prediction(predres.pseudo.single, trueY), "auc")@y.values[[1]],
                              performance(prediction(predres.binary, trueY), "auc")@y.values[[1]],
                              performance(prediction(predres.binder, trueY), "auc")@y.values[[1]],
                              performance(prediction(predres.rfsrc, trueY), "auc")@y.values[[1]],
                              performance(prediction(trueP, trueY), "auc")@y.values[[1]]))


    pauc.pseudo <- with(indat, calc_pAUC(predres.pseudo, cause1.pseudo, cause2.pseudo))
    pauc.pseudo.single <- with(indat, calc_pAUC(predres.pseudo.single, cause1.pseudo, cause2.pseudo))
    pauc.binary <- with(indat, calc_pAUC(predres.binary, cause1.pseudo, cause2.pseudo))
    pauc.binder <- with(indat, calc_pAUC(predres.binder, cause1.pseudo, cause2.pseudo))
    pauc.rfsrc <- with(indat, calc_pAUC(predres.rfsrc, cause1.pseudo, cause2.pseudo))
    pauc.true <- with(indat, calc_pAUC(trueP, cause1.pseudo, cause2.pseudo))

    bias.pseudo <- with(indat, mean((trueP - predres.pseudo)))
    bias.pseudo.single <- with(indat, mean((trueP - predres.pseudo.single)))
    bias.binary <- with(indat, mean(trueP - predres.binary))
    bias.binder <- with(indat, mean(trueP - predres.binder))
    bias.rfsrc <- with(indat, mean(trueP - predres.rfsrc))
    bias.true <- with(indat, mean(trueP - trueP))


    sd.pseudo <- with(indat, sd(predres.pseudo))
    sd.pseudo.single <- with(indat, sd(predres.pseudo.single))
    sd.binary <- with(indat, sd(predres.binary))
    sd.binder <- with(indat, sd(predres.binder))
    sd.rfsrc <- with(indat, sd(predres.rfsrc))
    sd.true <- with(indat, sd(exp(trueP)))



    res.auc <- rbind(res.auc, data.frame(true.auc = true.auc,
                                         pauc = c(pauc.pseudo, pauc.pseudo.single, pauc.binary, pauc.binder, pauc.rfsrc, pauc.true),
                                         bias = c(bias.pseudo, bias.pseudo.single, bias.binary, bias.binder, bias.rfsrc, bias.true),
                                         sd = c(sd.pseudo, sd.pseudo.single, sd.binary, sd.binder, sd.rfsrc, sd.true),
                                         model = c("pseudo", "pseudo.single", "binary", "binder", "rfsrc", "true")))

  }

  res.auc

}




