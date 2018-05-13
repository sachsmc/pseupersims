library(parallel)
library(dplyr)

cl <- makeCluster(getOption("cl.cores", 8))

clusterEvalQ(cl, devtools::load_all())

ree.scen <- function(scen, mp, ptb =TRUE) {

res.X <- clusterApplyLB(cl, 1:200, function(x){
  noret <- run_one_replicate(scenario = scen, missing.p = mp, output = sprintf("data/missing%.2f/simres%s-%03d.rds", mp, scen, x))
  if(ptb) {
  noret2 <- run_one_perturb(scenario = scen, missing.p = mp, output = sprintf("data/missing%.2f/pturb%s-%03d.rds", mp, scen, x))
  }
  })

}

ree.scen("0", .2, ptb = FALSE)
ree.scen("A", .2)
ree.scen("B", .2)
ree.scen("C", .2)

ree.scen("0", .5, ptb = FALSE)
ree.scen("A", .5)
ree.scen("B", .5)
ree.scen("C", .5)

tabres <- function(res) {

  res %>% group_by(model) %>% summarize(mean.tbauc = mean(true.auc), sd.tbauc = sd(true.auc),
                                        mean.pauc = mean(pauc), sd.pauc = sd(pauc),
                                  #      mean.int = mean(cal.int), sd.int = sd(cal.int),
                                  #      mean.slp = mean(cal.slp, na.rm = TRUE), sd.slp = sd(cal.slp, na.rm = TRUE),
                                        mean.bias = mean(bias), sd.prob = mean(sd),
                                        mse = 100 * mean(bias^2)
                                        )

}



res.0 <- analyze_sim("0", "missing0.20", 200)
res.A <- analyze_sim("A", "missing0.20", 200)
res.B <- analyze_sim("B", "missing0.20", 200)
res.C <- analyze_sim("C", "missing0.20", 200)


knitr::kable(rbind(tabres(res.0),
tabres(res.A),
tabres(res.B),
tabres(res.C)), digits = 3, "latex")


res.0 <- analyze_sim("0", "missing0.50", 200)
res.A <- analyze_sim("A", "missing0.50", 200)
res.B <- analyze_sim("B", "missing0.50", 200)
res.C <- analyze_sim("C", "missing0.50", 200)

knitr::kable(rbind(tabres(res.0),
                   tabres(res.A),
                   tabres(res.B),
                   tabres(res.C)), digits = 3, "latex")

knitr::kable(rbind(tabres(res.0),
                   tabres(res.A),
                   tabres(res.B),
                   tabres(res.C)), digits = 3)

library(tidyr)

loadup <- function(scenario, folder, n = 200) {
  resem <- do.call(rbind, lapply(1:n, function(i) {

    fnam <- sprintf("data/%s/pturb%s-%03d.rds", folder, scenario, i)
    if(!file.exists(fnam)) return(NULL)
    res.tmp <- readRDS(fnam)
    out.tmp <- res.tmp
    out.tmp$replicate <- i
    out.tmp$scenario <- scenario
    gather(out.tmp , key = "variable", value = "slope", X1:X20)

  }))
  resem
}



pta <- rbind(loadup("A", "missing0.20"),
             loadup("B", "missing0.20"),
             loadup("C", "missing0.20"))

pta$variable <- factor(pta$variable, levels = paste0("X", 1:20), ordered = TRUE)
pta$inmod <- 0
pta$inmod[with(pta, scenario  == "A" & variable == "X1")] <- 1
pta$inmod[with(pta, scenario  == "B" & variable %in% c("X1", "X6", "X11", "X16", "X20"))] <- 1
pta$inmod[with(pta, scenario  == "C" & variable %in% c("X1", "X6", "X11", "X16", "X20"))] <- 1

library(ggplot2)


ggplot(pta, aes(x = variable, y = slope)) + facet_wrap(~ scenario, ncol = 1, labeller = label_both, scales = "free_y") + theme_bw() +
   geom_boxplot() +
  geom_vline(data = subset(pta, scenario == "A"), aes(xintercept = 1), size = 5, alpha = .3) +
  geom_vline(data = subset(pta, scenario %in% c("B", "C")), aes(xintercept = 1), size = 5, alpha = .3) +
  geom_vline(data = subset(pta, scenario %in% c("B", "C")), aes(xintercept = 6), size = 5, alpha = .3) +
  geom_vline(data = subset(pta, scenario %in% c("B", "C")), aes(xintercept = 11), size = 5, alpha = .3) +
  geom_vline(data = subset(pta, scenario %in% c("B", "C")), aes(xintercept = 16), size = 5, alpha = .3) +
  geom_vline(data = subset(pta, scenario %in% c("B", "C")), aes(xintercept = 20), size = 5, alpha = .3)

ggsave("pertub-plot.png", width = 6.5, height = 6, units = "in")


## crossing scenario 1 replicate

indat <- generate_data(n = 500, scenario = "D", missing.p = .05)
validat <- subset(add_pseudo_obs(generate_data(scenario = "D", missing.p = .2)), time == 26.5)
indat2 <- add_pseudo_obs(indat)

slearn.fit <- superlearner_estimate(indat2, Y = "cause1.pseudo", X = c("time", paste0("X", 19:20)), Y2 = "cause2.pseudo")


library(pdp)
library(data.table)
library(ggplot2)
pseudo_pdp <- function(sltest, icetrain) {

  outres <- NULL
  for(var.in in colnames(icetrain)[-1]) {

    grid.in <- unique(icetrain[, var.in, drop = FALSE])
    grid.in <- grid.in[order(grid.in[[var.in]]), , drop = FALSE]
    pdp_est <- partial(sltest, pred.var = var.in, pred.grid = grid.in,
                       pred.fun = predict.mysl, train = icetrain, type = "regression")

    sd.pdp <- sd(pdp_est$yhat)
    if(nrow(grid.in) == 2) {
      inres <- data.frame(y.hat = sapply(grid.in[[var.in]], function(xx){
        mean(pdp_est[pdp_est[[var.in]] == xx, "yhat"])
      }), xxx = grid.in[[var.in]], variable = var.in, sd.yhat = sd.pdp)

    } else {
      le <- loess(as.formula(paste("yhat ~ ", var.in)), data = pdp_est)
      inres <- data.frame(y.hat = predict(le, newdata = grid.in), xxx = grid.in[[var.in]], variable = var.in, sd.yhat = sd.pdp)
    }

    outres <- rbind(outres, inres)
  }
  outres

}

predict.mysl <- function(object, newdata) {
  predict(object, newdata)$pred[, 1]
}


dexi <- sample(1:nrow(indat2), 200)

pdp_est <- pseudo_pdp(slearn.fit, indat2[dexi, c("time", paste0("X", 19:20))])
lime_est <- pseudo_lime(slearn.fit, indat2[dexi, c("time", paste0("X", 19:20))])

labord <- unique(pdp_est[, c("variable", "sd.yhat")])
labord$variable <- ifelse(labord$variable == "X19", "X2", "X1")
pdp_est <- data.table(pdp_est)
(pdp_est)[, binary := .N == 2, by = "variable"]

pdp_est$variable <- ifelse(pdp_est$variable == "X19", "X2", "X1")

pdp_est$lab2 <- factor(pdp_est$variable, levels = labord$variable[order(labord$sd.yhat, decreasing = TRUE)], ordered = TRUE)

p1 <- ggplot(pdp_est, aes(x = xxx, y = y.hat))  +
  facet_wrap(~ lab2, scales = "free_x") +
  geom_line(data = subset(pdp_est, !binary)) +
  geom_bar(data = subset(pdp_est, binary), stat = "identity") +
  xlab("variable value") + ylab("Predicted pseudo-observation") + theme_bw()



library(ggridges)

loneup <- melt(data.table(lime_est$predup), id.vars = "time")
lone.order <- loneup[, mean(value), by = variable]

loneup$variable <- ifelse(loneup$variable == "X19", "X2", "X1")
lone.order$variable <- ifelse(lone.order$variable == "X19", "X2", "X1")

loneup$lab.org <- factor(loneup$variable, levels = lone.order[order(lone.order$V1, decreasing = FALSE),]$variable, ordered = TRUE)
p2 <- ggplot(loneup, aes(y = value, x = lab.org)) + geom_boxplot() + coord_flip() + scale_y_continuous(limits = c(-.5, .5)) + ylab("Change in y.hat for an increase in variable") + xlab("Variable") + theme_bw()

library(patchwork)

png("interscenpdp.png", width = 6.5, height = 7, units = "in", res = 200)
grid.arrange(p1, p2)
dev.off()


