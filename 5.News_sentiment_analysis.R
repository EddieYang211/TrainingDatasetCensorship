require(lme4)
require(lmerTest)
require(lmtest)
require(knitr)
require(kableExtra)

setwd("~/Replication_TrainingDatasetCensorship/output")
set.seed(92092)

# Wikipedia vs. Baidubaike
wk <- read.csv("svm_wk.csv")
bd <- read.csv("svm_bd.csv")
svm <- matrix(NA, 10, 2)
colnames(svm) <- c("estimate", "p-value")
rownames(svm) <- c("Freedom", "Democracy", "Election", "Collective Action", "Negative Figures", 
                    "Social Control", "Surveillance", "CCP", "Historical Events", "Positive Figures")

for (i in 1:10){
  out <- cbind(c(na.omit(wk[,i])/2, na.omit(bd[,i])/2), rep(c(1,2), c(length(na.omit(wk[,i])), length(na.omit(bd[,i])))))
  out <- as.data.frame(out)
  out[,"id"] <- as.factor(c(na.omit(wk[,i+10]), na.omit(bd[,i+10])))
  out[,2] <- as.factor(out[,2])
  out <- na.omit(out)
  a <- coef(summary(lmer(V1 ~ V2  + (1 | id), data=out, REML=F)))
  svm[i, 1] <- a[2,"Estimate"]
  svm[i, 2] <- a[2,"Pr(>|t|)"]
}


wk <- read.csv("nb_wk.csv")
bd <- read.csv("nb_bd.csv")
nb <- matrix(NA, 10, 2)
colnames(nb) <- c("estimate", "p-value")
rownames(nb) <- c("Freedom", "Democracy", "Election", "Collective Action", "Negative Figures", 
                    "Social Control", "Surveillance", "CCP", "Historical Events", "Positive Figures")

for (i in 1:10){
  out <- cbind(c(na.omit(wk[,i])/2, na.omit(bd[,i])/2), rep(c(1,2), c(length(na.omit(wk[,i])), length(na.omit(bd[,i])))))
  out <- as.data.frame(out)
  out[,"id"] <- as.factor(c(na.omit(wk[,i+10]), na.omit(bd[,i+10])))
  out[,2] <- as.factor(out[,2])
  out <- na.omit(out)
  a <- coef(summary(lmer(V1 ~ V2  + (1 | id), data=out, REML=F)))
  nb[i, 1] <- a[2,"Estimate"]
  nb[i, 2] <- a[2,"Pr(>|t|)"]
}


wk <- matrix(0, 4999, 20)
for (i in 1:10){
  f <- paste0("wk", i, ".csv")
  tmp <- read.csv(f)
  wk[,1:10] <- wk[,1:10]+as.matrix(tmp[,1:10])
  wk[,11:20] <- as.matrix(tmp[,11:20])
}
wk[,1:10] <- wk[,1:10]/10

bd <- matrix(0, 4999, 20)
for (i in 1:10){
  f <- paste0("bd", i, ".csv")
  tmp <- read.csv(f)
  bd[,1:10] <- bd[,1:10]+as.matrix(tmp[,1:10])
  bd[,11:20] <- as.matrix(tmp[,11:20])
}
bd[,1:10] <- bd[,1:10]/10

cnn <- matrix(NA, 10, 2)
colnames(cnn) <- c("estimate", "p-value")
rownames(cnn) <- c("Freedom", "Democracy", "Election", "Collective Action", "Negative Figures", 
                    "Social Control", "Surveillance", "CCP", "Historical Events", "Positive Figures")

for (i in 1:10){
  out <- cbind(c(na.omit(wk[,i])/2, na.omit(bd[,i])/2), rep(c(1,2), c(length(na.omit(wk[,i])), length(na.omit(bd[,i])))))
  out <- as.data.frame(out)
  out[,"id"] <- as.factor(c(na.omit(wk[,i+10]), na.omit(bd[,i+10])))
  out[,2] <- as.factor(out[,2])
  out <- na.omit(out)
  a <- coef(summary(lmer(V1 ~ V2  + (1 | id), data=out, REML=F)))
  cnn[i, 1] <- a[2,"Estimate"]
  cnn[i, 2] <- a[2,"Pr(>|t|)"]
}

kable(round(cbind(nb, svm, cnn), 2), "latex", booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Naive Bayes" = 2, "SVM" = 2, "TextCNN" = 2))


# Wikipedia vs. People's Daily
wk <- read.csv("svm_wk.csv")
rm <- read.csv("svm_rm.csv")
svm <- matrix(NA, 10, 2)
colnames(svm) <- c("estimate", "p-value")
rownames(svm) <- c("Freedom", "Democracy", "Election", "Collective Action", "Negative Figures", 
                    "Social Control", "Surveillance", "CCP", "Historical Events", "Positive Figures")

for (i in 1:10){
  out <- cbind(c(na.omit(wk[,i])/2, na.omit(rm[,i])/2), rep(c(1,2), c(length(na.omit(wk[,i])), length(na.omit(rm[,i])))))
  out <- as.data.frame(out)
  out[,"id"] <- as.factor(c(na.omit(wk[,i+10]), na.omit(rm[,i+10])))
  out[,2] <- as.factor(out[,2])
  out <- na.omit(out)
  a <- coef(summary(lmer(V1 ~ V2  + (1 | id), data=out, REML=F)))
  svm[i, 1] <- a[2,"Estimate"]
  svm[i, 2] <- a[2,"Pr(>|t|)"]
}


wk <- read.csv("nb_wk.csv")
rm <- read.csv("nb_rm.csv")
nb <- matrix(NA, 10, 2)
colnames(nb) <- c("estimate", "p-value")
rownames(nb) <- c("Freedom", "Democracy", "Election", "Collective Action", "Negative Figures", 
                    "Social Control", "Surveillance", "CCP", "Historical Events", "Positive Figures")

for (i in 1:10){
  out <- cbind(c(na.omit(wk[,i])/2, na.omit(rm[,i])/2), rep(c(1,2), c(length(na.omit(wk[,i])), length(na.omit(rm[,i])))))
  out <- as.data.frame(out)
  out[,"id"] <- as.factor(c(na.omit(wk[,i+10]), na.omit(rm[,i+10])))
  out[,2] <- as.factor(out[,2])
  out <- na.omit(out)
  a <- coef(summary(lmer(V1 ~ V2  + (1 | id), data=out, REML=F)))
  nb[i, 1] <- a[2,"Estimate"]
  nb[i, 2] <- a[2,"Pr(>|t|)"]
}


wk <- matrix(0, 4999, 20)
for (i in 1:10){
  f <- paste0("wk", i, ".csv")
  tmp <- read.csv(f)
  wk[,1:10] <- wk[,1:10]+as.matrix(tmp[,1:10])
  wk[,11:20] <- as.matrix(tmp[,11:20])
}
wk[,1:10] <- wk[,1:10]/10

rm <- matrix(0, 4999, 20)
for (i in 1:10){
  f <- paste0("rm", i, ".csv")
  tmp <- read.csv(f)
  rm[,1:10] <- rm[,1:10]+as.matrix(tmp[,1:10])
  rm[,11:20] <- as.matrix(tmp[,11:20])
}
rm[,1:10] <- rm[,1:10]/10

cnn <- matrix(NA, 10, 2)
colnames(cnn) <- c("estimate", "p-value")
rownames(cnn) <- c("Freedom", "Democracy", "Election", "Collective Action", "Negative Figures", 
                    "Social Control", "Surveillance", "CCP", "Historical Events", "Positive Figures")

for (i in 1:10){
  out <- cbind(c(na.omit(wk[,i])/2, na.omit(rm[,i])/2), rep(c(1,2), c(length(na.omit(wk[,i])), length(na.omit(rm[,i])))))
  out <- as.data.frame(out)
  out[,"id"] <- as.factor(c(na.omit(wk[,i+10]), na.omit(rm[,i+10])))
  out[,2] <- as.factor(out[,2])
  out <- na.omit(out)
  a <- coef(summary(lmer(V1 ~ V2  + (1 | id), data=out, REML=F)))
  cnn[i, 1] <- a[2,"Estimate"]
  cnn[i, 2] <- a[2,"Pr(>|t|)"]
}

kable(round(cbind(nb, svm, cnn), 2), "latex", booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Naive Bayes" = 2, "SVM" = 2, "TextCNN" = 2))
