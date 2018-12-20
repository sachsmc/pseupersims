library(rio)

cdsurg <- import("dataraw/cdsurg-prediction.csv")
invisible(lapply(grep("date", colnames(cdsurg), value = TRUE), function(x) cdsurg[[x]] <<- as.Date(cdsurg[[x]], format = "%Y-%m-%d")))

cdsurg <- within(cdsurg, {
  last_date <- pmin(date_latest_emig, deathdate, na.rm = TRUE)

  end_date <- pmin(as.Date("2014-12-31"), surg_date, last_date, na.rm = TRUE)

  years_to_surg <- as.numeric(end_date - date_ibd1) / 365.25
})

cdsurg$surg <- !is.na(cdsurg$surg_date)
cdsurg <- subset(cdsurg, end_date > date_ibd2 & age_ibd1 >= 18& date_ibd1 >= as.Date("2003-01-01"))

cdsurg$cause_indi <- 0
cdsurg$cause_indi[with(cdsurg, !is.na(surg_date))] <- 1
cdsurg$cause_indi[with(cdsurg, !is.na(deathdate)  & (deathdate < surg_date | is.na(surg_date)))] <- 2
cdsurg$cause_indi[with(cdsurg, !is.na(date_latest_emig)  & (date_latest_emig < surg_date | is.na(surg_date)))] <- 3

cdsurg$parisB1 <- cdsurg$parisB == "B1"
cdsurg$parisL3 <- cdsurg$parisL == "L3/LX"
cdsurg$parisP.plus <- cdsurg$parisP == "P+"

cdsurg <- within(cdsurg, {
  parisB1[is.na(parisB1)] <- FALSE
  parisL3[is.na(parisL3)] <- FALSE
  parisP.plus[is.na(parisP.plus)] <- FALSE
})

cdsurg <- subset(cdsurg, cause_indi != 3)
pred.vars <- c("age_ibd1", "male", "inpatient", "year_ibd1", colnames(cdsurg)[c(12:21,35:38)],
               "parisB1", "parisL3")
X <- cdsurg[, pred.vars]

crohnraw <- data.frame(id = cdsurg$lpnr, Tout = cdsurg$years_to_surg * 5, delta = cdsurg$cause_indi,
                       X, trueT = NA, trueP = NA, Cen = NA, Y = NA, Y2 = NA)

saveRDS(crohnraw, file = "data/crohnraw.rds")