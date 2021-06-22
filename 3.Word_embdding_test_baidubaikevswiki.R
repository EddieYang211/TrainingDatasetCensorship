#rm(list=ls(all=TRUE))
library(lsa)
library(knitr)
library(kableExtra)
library(magrittr)

# set working directory before runing 
setwd("Replication_TrainingDatasetCensorship")
load("output/3.scaled_baidubaike_vswiki.Rdata")
load("output/3.scaled_wiki_vsbaidubaike.Rdata")
baidu <- b
cnwiki <- c

# sanity check
identical(rownames(baidu), rownames(cnwiki)) # make sure vocab is the same
#===============================
# extract word from corpus
#===============================
# Some of the words below will be dropped, because they do not appear in the vocabulary
source("auxiliary/Words.R")

set.seed(92092)

# sentiment dictionary
data <- read.csv("auxiliary/ANTUSD/opinion_word_simplified.csv", stringsAsFactors=FALSE)
data[,1] <- as.character(data[,1])
data <- data[!duplicated(data[,1]),]
colnames(data) <- c("word", "score", "pos", "neu", "neg", "nonop", "non")
data[,"label"] <- NA

# procedure in table 2 of ANTUSD paper
for (i in 1:nrow(data)){
  if (data[i, "non"]>0 || data[i, "nonop"]>0){
    data[i, "label"] <- NA
  }else if (data[i, "pos"]>0 && data[i, "neg"]==0){
    data[i, "label"] <- 1
  }else if(data[i, "neg"]>0 && data[i, "pos"]==0){
    data[i, "label"] <- -1
  }else if (data[i, "neg"]==0 && data[i, "pos"]==0 && data[i, "neu"]>0){
    data[i, "label"] <- 0
  }else{
    data[i, "label"] <- NA
  }
}
data <- na.omit(data)
  
data <- data[,c("word", "label")]
sen.pos <- data[data[,"label"]==1,]
sen.neg <- data[data[,"label"]==-1,]

# extract
full <- c(freedom, democracy, election, collective.action, negative.figure, 
          social.control, surveillance, party, history, positive.figure, 
          pos.subst, neg.subst, sen.pos[,"word"], sen.neg[,"word"])

baidu <- baidu[which(rownames(baidu)%in%full),]
cnwiki <- cnwiki[which(rownames(cnwiki)%in%full),]


# extract target words
target <- c(freedom, democracy, election, collective.action, negative.figure, 
            social.control, surveillance, party, history, positive.figure)
tar <- which(target%in%rownames(baidu))
target <- target[tar]

tar.bd <- matrix(NA, length(target), ncol(baidu))
for (i in 1:length(target)){
  tar.bd[i,] <- baidu[which(rownames(baidu)==target[i]),]
}
tar.wk <- matrix(NA, length(target), ncol(cnwiki))
for (i in 1:length(target)){
  tar.wk[i,] <- cnwiki[which(rownames(cnwiki)==target[i]),]
}

# extract positive substantive words
pos.subst.bd <- baidu[rownames(baidu)%in%pos.subst,]
pos.subst.wk <- cnwiki[rownames(cnwiki)%in%pos.subst,]

# extract negative substantive words
neg.subst.bd  <- baidu[rownames(baidu)%in%neg.subst,]
neg.subst.wk  <- cnwiki[rownames(cnwiki)%in%neg.subst,]

# extract positive sentiment
pos.sen.bd <- baidu[rownames(baidu)%in%sen.pos[,"word"],]
pos.sen.wk  <- cnwiki[rownames(cnwiki)%in%sen.pos[,"word"],]

# extract negative sentiment
neg.sen.bd <- baidu[rownames(baidu)%in%sen.neg[,"word"],]
neg.sen.wk  <- cnwiki[rownames(cnwiki)%in%sen.neg[,"word"],]


#====================
# cosine similarity
#====================
# adjective/propaganda
#====================
sign <- c(1, 1, 1, 1, 1, 0, 0, 0, 0, 0) # 1: wiki more favorable
adj <- matrix(NA, 10, 3)
colnames(adj) <- c("estimate", "effect size", "p-value")#, "conf.low", "conf.high")
rownames(adj) <- c("Freedom", "Democracy", "Election", "Collective Action", "Negative Figures", 
                    "Social Control", "Surveillance", "CCP", "Historical Events", "Positive Figures")
cat <- list(
  free <- c(1:22),
  demo <- c(23:37),
  elect <- c(38:51),
  col <- c(52:66),
  neg_l <- c(67:83),
  con <- c(84:95),
  sur <- c(96:113),
  party <- c(114:121),
  hist <- c(122:140),
  pos_l <- c(141:183))

for (k in 1:length(cat)){ # loop over words in each category, and over all attribute words for each target word
  tar <- cat[[k]]
  # wiki
  subst.pos.score.wk <- rep(NA, length(tar))
  subst.neg.score.wk <- rep(NA, length(tar))

  out <- rep(NA, nrow(pos.subst.wk))
  counter <- 1
  for (i in tar){
    for (j in 1:nrow(pos.subst.wk)){
      out[j] <- cosine(unlist(tar.wk[i,]), unlist(pos.subst.wk[j,]))
    }
    subst.pos.score.wk[counter] <- mean(out)
    out <- rep(NA, nrow(pos.subst.wk))
    counter <- counter + 1
  }
  
  out <- rep(NA, nrow(neg.subst.wk))
  counter <- 1
  for (i in tar){
    for (j in 1:nrow(neg.subst.wk)){
      out[j] <- cosine(unlist(tar.wk[i,]), unlist(neg.subst.wk[j,]))
    }
    subst.neg.score.wk[counter] <- mean(out)
    out <- rep(NA, nrow(pos.subst.wk))
    counter <- counter + 1
  }
  
  res.wk.subst <- subst.pos.score.wk - subst.neg.score.wk
  
  
  # baidu
  subst.pos.score.bd <- rep(NA, length(tar))
  subst.neg.score.bd <- rep(NA, length(tar))

  out <- rep(NA, nrow(pos.subst.bd))
  counter <- 1
  for (i in tar){
    for (j in 1:nrow(pos.subst.bd)){
      out[j] <- cosine(unlist(tar.bd[i,]), unlist(pos.subst.bd[j,]))
    }
    subst.pos.score.bd[counter] <- mean(out)
    out <- rep(NA, nrow(pos.subst.bd))
    counter <- counter + 1
  }
  
  out <- rep(NA, nrow(neg.subst.bd))
  counter <- 1
  for (i in tar){
    for (j in 1:nrow(neg.subst.bd)){
      out[j] <- cosine(unlist(tar.bd[i,]), unlist(neg.subst.bd[j,]))
    }
    subst.neg.score.bd[counter] <- mean(out)
    out <- rep(NA, nrow(pos.subst.bd))
    counter <- counter + 1
  }
  
  res.bd.subst <- subst.pos.score.bd - subst.neg.score.bd
  
  # permutation test
  perm <- rep(NA, 100000)
  s <- c(res.bd.subst, res.wk.subst)
  for (p in 1:length(perm)){
    t <- rbinom(length(res.bd.subst), 1, 0.5)
    t <- c(t, abs(t-1))
    dat <- cbind(s, t)
    perm[p] <- mean(dat[dat[,"t"]==1,"s"]) - mean(dat[dat[,"t"]==0,"s"])
  }
  
  adj[k,1] <- mean(res.bd.subst)-mean(res.wk.subst)
  adj[k,2] <- adj[k,1]/sd(s)
  adj[k,3] <- ifelse(sign[k]==1, sum(perm < adj[k,1])/length(perm), sum(perm > adj[k,1])/length(perm))
}

round(adj, 4)


#====================
# cosine similarity
#====================
# sentiment/evaluative
#====================
sent <- matrix(NA, 10, 3)
colnames(sent) <- c("estimate", "effect size", "p-value")
rownames(sent) <- c("Freedom", "Democracy", "Election", "Collective Action", "Negative Figures", 
                    "Social Control", "Surveillance", "CCP", "Historical Events", "Positive Figures")
cat <- list(
  free <- c(1:22),
  demo <- c(23:37),
  elect <- c(38:51),
  col <- c(52:66),
  neg_l <- c(67:83),
  con <- c(84:95),
  sur <- c(96:113),
  party <- c(114:121),
  hist <- c(122:140),
  pos_l <- c(141:183))

for (k in 1:length(cat)){     
  tar <- cat[[k]]
  # wiki
  sen.pos.score.wk <- rep(NA, length(tar))
  sen.neg.score.wk <- rep(NA, length(tar))

  out <- rep(NA, nrow(pos.sen.wk))
  counter <- 1
  for (i in tar){
    for (j in 1:nrow(pos.sen.wk)){
      out[j] <- cosine(unlist(tar.wk[i,]), unlist(pos.sen.wk[j,]))
    }
    sen.pos.score.wk[counter] <- mean(out)
    out <- rep(NA, nrow(pos.sen.wk))
    counter <- counter + 1
  }
  
  out <- rep(NA, nrow(neg.sen.wk))
  counter <- 1
  for (i in tar){
    for (j in 1:nrow(neg.sen.wk)){
      out[j] <- cosine(unlist(tar.wk[i,]), unlist(neg.sen.wk[j,]))
    }
    sen.neg.score.wk[counter]<-mean(out)
    out <- rep(NA, nrow(neg.sen.wk))
    counter <- counter + 1
  }
  
  res.wk.sen <- sen.pos.score.wk - sen.neg.score.wk
  

  # baidu
  sen.pos.score.bd <- rep(NA, length(tar))
  sen.neg.score.bd <- rep(NA, length(tar))

  out <- rep(NA, nrow(pos.sen.bd))
  counter <- 1
  for (i in tar){
    for (j in 1:nrow(pos.sen.bd)){
      out[j] <- cosine(unlist(tar.bd[i,]), unlist(pos.sen.bd[j,]))
    }
    sen.pos.score.bd[counter] <- mean(out)
    out <- rep(NA, nrow(pos.sen.bd))
    counter <- counter + 1
  }
  
  out <- rep(NA, nrow(neg.sen.bd))
  counter <- 1
  for (i in tar){
    for (j in 1:nrow(neg.sen.bd)){
      out[j] <- cosine(unlist(tar.bd[i,]), unlist(neg.sen.bd[j,]))
    }
    sen.neg.score.bd[counter] <- mean(out)
    out <- rep(NA, nrow(neg.sen.bd))
    counter <- counter + 1
  }
  
  res.bd.sen <- sen.pos.score.bd - sen.neg.score.bd
  
  # permutation test
  perm <- rep(NA, 100000)
  s <- c(res.bd.sen, res.wk.sen)
  for (p in 1:length(perm)){
    t <- rbinom(length(res.bd.sen), 1, 0.5)
    t <- c(t, abs(t-1))
    dat <- cbind(s, t)
    perm[p] <- mean(dat[dat[,"t"]==1,"s"]) - mean(dat[dat[,"t"]==0,"s"])
  }
  
  sent[k,1] <- mean(res.bd.sen)-mean(res.wk.sen)
  sent[k,2] <- sent[k,1]/sd(s)
  sent[k,3] <- ifelse(sign[k]==1, sum(perm < sent[k,1])/length(perm), sum(perm > sent[k,1])/length(perm))
}

round(sent, 4)

kable(round(cbind(adj[,2:3], sent[,2:3]), 2), "latex", booktabs = T) %>%
  kable_styling() %>%
  add_header_above(c(" ", "Adjective Words" = 2, "Sentiment Words" = 2))

write.csv(adj, "output/word_embedding_test_adj_baiduvswiki.csv")
write.csv(sent, "output/word_embedding_test_sent_baiduvswiki.csv")

