library(data.table)
library(future.apply)
library(caret)
library(C50)


# read
dt.orig <- fread("D:\\dataset_readmissions_datathon_HUS_2022_upd.csv")[order(patientID, timestamp_in)]

# remove dead
dt <- dt.orig[mors!=1]

# clean ICD mess
dt[, icdChapt:=gsub(".*?(\\w\\d+n?-\\w\\d+n?).*", "\\1", mainDiagICD10Chapter)]
dt[, icdBlock:=gsub(".*?(\\w\\d+n?-\\w\\d+n?).*", "\\1", mainDiagBlock)]

# take first 10% of rows to speed things up
dt <- head(dt, 24000)

# factorise some columns
dcols <- grep("diagnosis_code", colnames(dt), value=TRUE)
fcols <- c("gender", "hosp_unique_ID", dcols, "age_group", "icdChapt", "icdBlock") #, "readmission" "degree_of_urgency", 
dt[, (fcols):=lapply(.SD, as.factor), .SDcols=fcols]

# split by patient, split by admission+readmission, collapse multiple rows that represent the same (re)admission event
# plan(multiprocess) # enable if multithreaded and replace single lapply with future_lapply in the row below
dt.sum <- rbindlist(lapply(split(dt, dt$patientID), function (d) {
  all_diags <- c(); ndiags <- c(); ndays <- c(); days_since <- c(); last_out <- -1
  # d <- split(dt, dt$patientID)$"2260" # this one has events to be collapsed, explore to see
  # d <- split(dt, dt$patientID)$"2273" # this one has events not to be collapsed, explore to see
  # calculate number of unique diagnoses so far, per each row
  for (i in seq(nrow(d))) {
    all_diags <- unique(c(all_diags, as.numeric(d[i, dcols, with=FALSE])))
    ndiags <- c(ndiags, length(all_diags)-1)
  }
  d$ndiags <- ndiags
  # calculate number of days since last row
  d$days_since <- c(0, d[-1]$timestamp_in - d[-nrow(d)]$timestamp_out)
  # group here defines admissions[+readmissions] record group (0 followed by 1)
  d$group <- cumsum(as.numeric(c(FALSE, d[-1]$readmission < d[-nrow(d)]$readmission)))
  # split by admissions[+readmissions] record group
  s <- rbindlist(lapply(split(d, d$group), function (g) {
    # g <- split(d, d$group)$"0" # event groups with some rows to collapse
    # g <- split(d, d$group)$"10"
    # agroup here defines several events to be collapsed together because
    # they represent a single (re)admission event
    g$agroup <- cumsum(as.numeric(c(FALSE, g[-1]$days_since > 1)))
    # split by a single (re)admission event
    rbindlist(lapply(split(g, g$agroup), function (ag) {
      # colapse records for a single event
      ag[,
         list(
           age_group=tail(age_group, 1),             # latest recorded age group
           DRG=sum(unique(DRG)),                     # sum of unique DRG
           timestamp_in=min(timestamp_in),           # when (re)admitted
           timestamp_out=max(timestamp_out),         # when discharged
           degree_of_urgency=min(degree_of_urgency), # min urgency
           nhosp=length(unique(hosp_unique_ID)),     # how many hospital IDs are listed in this single event
           icdChapt=tail(icdChapt, 1),               # latest ICD-10 chapter (at discharge)
           icdBlock=tail(icdBlock, 1),               # latest ICD-10 block (at discharge)
           ndiags=max(ndiags),                       # number of unique diagnoses so far
           days_since=max(days_since)                # days since previous (re)admission event
         ), by=c("agroup", "gender", "patientID", "readmission")]
    }))
  }))
  # add "cumulative number of admissions so far" and "bed days during this (re)admission"
  s[, `:=` (nadm=seq(nrow(s)), bed_days=timestamp_out-timestamp_in+1)]
  # add "cumulative bed days at hospital so far"
  s[, bed_days_sf:=cumsum(bed_days)]
  s
}))

# add variable for the outcome  "followed by readmission"
dt.sum$followed_by_readm <- c(dt.sum$readmission[-1], 0)

# factorise columns - but I didn't check if some factors need to be refreshed
fcols <- c("degree_of_urgency", "followed_by_readm")
dt.sum[, (fcols):=lapply(.SD, as.factor), .SDcols=fcols]

# clean data from unneeded columns
ccols <- c("gender", "age_group", "DRG", "degree_of_urgency", "nhosp", "icdChapt", "icdBlock", "ndiags", "days_since", "nadm", "bed_days", "bed_days_sf")
# outcome
outc <- "followed_by_readm"

# remove readmission records because they cannot be followed by readmission
dt.clean <- dt.sum[readmission!=1, c(ccols, outc), with=FALSE]

# save clean data
fwrite(dt.clean, file="D:\\dt.clean.csv")

# ##############################################################################
# WHAT IS MISSING HERE IS ONE-HOT ENCODING #####################################
# DO IT BEFORE MODELLING!                  #####################################
# ##############################################################################

# training columns
tcols <- c("age_group", "DRG", "degree_of_urgency", "ndiags", "days_since", "nadm", "bed_days", "bed_days_sf")

# split in train/test
set.seed(666)
rtrain <- sample(seq(nrow(dt.clean)), nrow(dt.clean)*0.8)
dt.train <- dt.clean[rtrain]
dt.test <- dt.clean[-rtrain]


# # unneeded balancing #########################################################
# dt.tmp <- dt.train
# dt.train <- rbind(dt.train[followed_by_readm==0], rbindlist( rep(list(dt.train[followed_by_readm==1]), 10)))
# ##############################################################################



# test space ###################################################################
fit <- caret::train(x=dt.train[, tcols, with=FALSE], dt.train$followed_by_readm, method="C5.0")
cmx <- confusionMatrix(table(predict(fit, newdata=dt.test[, tcols, with=FALSE]),
                             unname(unlist(dt.test[, outc, with=FALSE]))))
TN=cmx$table[1,1]; FN=cmx$table[2,1]; FP=cmx$table[1,2]; TP=cmx$table[2,2]
cmx$byClass <- c(cmx$byClass, MCC=(TP*TN-FP*FN) / (sqrt((TP+FP))*sqrt((TP+FN))*sqrt((TN+FP))*sqrt((TN+FN))) )
cmx


# C5.0 #########################################################################
# train
C5.0.fit.params <- list(
  # "start" = list(
  #   dataset = dt.train,
  #   trials  = 30,
  #   control = C5.0Control(minCases=6, winnow=FALSE),
  #   costs   = matrix(c(1,2,5,1), nrow=2, dimnames=list(c("0","1"), c("0","1")))
  # ),
  "test" = list(
    dataset = dt.train[, c(tcols, outc), with=FALSE],
    trials  = 30,
    control = C5.0Control(minCases=6, winnow=TRUE),
    costs   = matrix(c(1,1,1,1), nrow=2, dimnames=list(c("0","1"), c("0","1")))
  )
)

for (paramset in names(C5.0.fit.params)) {
  fit <- C5.0(as.formula(paste0(outc, "~.")), rules=FALSE, 
              data   =C5.0.fit.params[[paramset]][["dataset"]],
              trials =C5.0.fit.params[[paramset]][["trials"]],
              control=C5.0.fit.params[[paramset]][["control"]],
              costs  =C5.0.fit.params[[paramset]][["costs"]])
  imp <- C5imp(fit)
  
  cmx <- confusionMatrix(table(predict(fit, newdata=dt.test[, tcols, with=FALSE]),
                               unname(unlist(dt.test[, outc, with=FALSE]))))
  TN=cmx$table[1,1]; FN=cmx$table[2,1]; FP=cmx$table[1,2]; TP=cmx$table[2,2]
  cmx$byClass <- c(cmx$byClass, MCC=(TP*TN-FP*FN) / (sqrt((TP+FP))*sqrt((TP+FN))*sqrt((TN+FP))*sqrt((TN+FN))) )
  
  C5.0.fit.params[[paramset]][["fit"]] <- fit
  C5.0.fit.params[[paramset]][["cmx"]] <- cmx
  
  print(plot(fit, main=paramset))
  print(t(imp[imp$Overall>0,,drop=FALSE]))
  # print(summary(fit))
  print(cmx)
}


# LMT ##########################################################################
library(RWeka)

LMT.fit.params <- list(
  "start" = list(
    dataset = dt.train,
    control = Weka_control(I=81,P=TRUE)
  )
)

for (paramset in names(LMT.fit.params)) {
  fit <- LMT(as.formula(paste0(outc, "~.")),
             data   =LMT.fit.params[[paramset]][["dataset"]],
             control=LMT.fit.params[[paramset]][["control"]])
  
  cmx <- confusionMatrix(table(predict(fit, newdata=dt.test[, tcols, with=FALSE]),
                               unname(unlist(dt.test[, outc, with=FALSE]))))
  
  LMT.fit.params[[paramset]][["fit"]] <- fit
  LMT.fit.params[[paramset]][["cmx"]] <- cmx
  
  print(fit)
  print(cmx)
}









