library(parallel)

res.0 <- mclapply(1:50, function(x) run_one_replicate(scenario = "0"))
res.A <- mclapply(1:50, function(x) run_one_replicate(scenario = "A"))
res.B <- mclapply(1:50, function(x) run_one_replicate(scenario = "B"))
res.C <- mclapply(1:50, function(x) run_one_replicate(scenario = "C"))

save(res.0, res.A, res.B, res.C, file = "C:/Users/micsac/Box Sync/iris sims/sim-resultstrans.RData")


summary(as.data.frame(do.call(rbind, res.0)))

with(as.data.frame(do.call(rbind, res.A)), plot(mse.pseudo ~ mse.binary, xlim = c(0, .5), ylim = c(0, .5)))
abline(0, 1)

