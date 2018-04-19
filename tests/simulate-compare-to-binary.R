library(parallel)
library(dplyr)

res.0 <- mclapply(1:200, function(x) run_one_replicate(scenario = "0", output = sprintf("data/simres0-%03d.rds", x)))
res.A <- mclapply(1:200, function(x) run_one_replicate(scenario = "A", output = sprintf("data/simresA-%03d.rds", x)))
res.B <- mclapply(1:200, function(x) run_one_replicate(scenario = "B", output = sprintf("data/simresB-%03d.rds", x)))
res.C <- mclapply(1:200, function(x) run_one_replicate(scenario = "C", output = sprintf("data/simresC-%03d.rds", x)))

res.0 <- mclapply(1:200, function(x) run_one_replicate(scenario = "0", missing.p = .5,
                                                       output = sprintf("data/missing.50/simres0-%03d.rds", x)))
res.A <- mclapply(1:200, function(x) run_one_replicate(scenario = "A", missing.p = .5,
                                                       output = sprintf("data/missing.50/simresA-%03d.rds", x)))
res.B <- mclapply(1:200, function(x) run_one_replicate(scenario = "B", missing.p = .5,
                                                       output = sprintf("data/missing.50/simresB-%03d.rds", x)))
res.C <- mclapply(1:200, function(x) run_one_replicate(scenario = "C", missing.p = .5,
                                                       output = sprintf("data/missing.50/simresC-%03d.rds", x)))

res.0 <- mclapply(1:20, function(x) run_one_replicate(scenario = "0", output = sprintf("data/stupid/simres0-%03d.rds", x)))
res.A <- mclapply(1:20, function(x) run_one_replicate(scenario = "A", output = sprintf("data/stupid/simresA-%03d.rds", x)))
res.B <- mclapply(1:20, function(x) run_one_replicate(scenario = "B", output = sprintf("data/stupid/simresB-%03d.rds", x)))
res.C <- mclapply(1:20, function(x) run_one_replicate(scenario = "C", output = sprintf("data/stupid/simresC-%03d.rds", x)))

tabres <- function(res) {

  res %>% group_by(model) %>% summarize(mean.tbauc = mean(true.auc), sd.tbauc = sd(true.auc),
                                        mean.int = mean(cal.int), sd.int = sd(cal.int),
                                        mean.slp = mean(cal.slp, na.rm = TRUE), sd.slp = sd(cal.slp, na.rm = TRUE),
                                        mean.pauc = mean(pauc), sd.pauc = sd(pauc),
                                        mean.bias = mean(bias), sd.prob = mean(sd),
                                        mse = mean(bias^2)
                                        )

}



res.0 <- analyze_sim("0", "missing.50", 200)
res.A <- analyze_sim("A", "missing.50", 200)
res.B <- analyze_sim("B", "missing.50", 200)
res.C <- analyze_sim("C", "missing.50", 200)


lapply(list(tabres(res.0),
tabres(res.A),
tabres(res.B),
tabres(res.C)), knitr::kable, digits = 3)
