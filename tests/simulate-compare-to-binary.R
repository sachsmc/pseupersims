library(parallel)
library(dplyr)

cl <- makeCluster(getOption("cl.cores", 4))

clusterEvalQ(cl, devtools::load_all())

ree.scen <- function(scen, mp, ptb =FALSE) {

res.X <- clusterApplyLB(cl, 1:200, function(x){
  noret <- run_one_replicate(scenario = scen, missing.p = mp, output = sprintf("data/missing%.2f/simres%s-%03d.rds", mp, scen, x))
  if(ptb) {
  noret2 <- run_one_perturb(scenario = scen, missing.p = mp, output = sprintf("data/missing%.2f/pturb%s-%03d.rds", mp, scen, x))
  }
  })

}

# ree.scen("0", .2, ptb = FALSE)
 ree.scen("A", .2)
 ree.scen("B", .2)
 ree.scen("C", .2)
 ree.scen("D", .2)
ree.scen("E", .2)

#ree.scen("0", .5, ptb = FALSE)
ree.scen("A", .5)
ree.scen("B", .5)
ree.scen("C", .5)
ree.scen("D", .5)
ree.scen("E", .5)

tabres <- function(res) {

  res %>% group_by(model) %>% summarize(mean.tbauc = mean(true.auc), sd.tbauc = sd(true.auc),
                                        mean.pauc = mean(pauc), sd.pauc = sd(pauc),
                                  #      mean.int = mean(cal.int), sd.int = sd(cal.int),
                                  #      mean.slp = mean(cal.slp, na.rm = TRUE), sd.slp = sd(cal.slp, na.rm = TRUE),
                                        mean.bias = mean(bias), sd.prob = mean(sd),
                                        mse = 100 * mean(bias^2)
                                        )

}



#res.0 <- analyze_sim("0", "missing0.20", 200)
res.A <- analyze_sim("A", "missing0.20", 200)
res.B <- analyze_sim("B", "missing0.20", 200)
res.C <- analyze_sim("C", "missing0.20", 200)
res.D <- analyze_sim("D", "missing0.20", 200)
res.E <- analyze_sim("E", "missing0.20", 200)



knitr::kable(rbind(
tabres(res.A),
tabres(res.B),
tabres(res.C),
tabres(res.D),
tabres(res.E)), digits = 3)

knitr::kable(rbind(tabres(res.A),
                   tabres(res.B),
                   tabres(res.C),
                   tabres(res.D),
                   tabres(res.E)), digits = 3, "latex") %>% gsub("\\hline", "", ., fixed = TRUE)


#res.0 <- analyze_sim("0", "missing0.50", 200)
res.A <- analyze_sim("A", "missing0.50", 200)
res.B <- analyze_sim("B", "missing0.50", 200)
res.C <- analyze_sim("C", "missing0.50", 200)
res.D <- analyze_sim("D", "missing0.50", 200)
res.E <- analyze_sim("E", "missing0.50", 200)


knitr::kable(rbind(tabres(res.A),
                   tabres(res.B),
                   tabres(res.C),
                   tabres(res.D),
                   tabres(res.E)), digits = 3, "latex") %>% gsub("\\hline", "", ., fixed = TRUE)


