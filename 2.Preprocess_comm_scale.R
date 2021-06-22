#=================
# Pre-processing
#=================
# Baidubaike
setwd("Replication_TrainingDatasetCensorship/output")
baidu <- read.csv("1.tocsv_baidubaike_word.csv", header=FALSE, stringsAsFactors=FALSE)
baidu <- na.omit(baidu)
baidu <- baidu[!duplicated(baidu[,1]),]
baidu <- baidu[!is.na(baidu[,1]),]
rownames(baidu) <- baidu[,1]
baidu <- baidu[,-1]

# Add tokens '<UNK>' and '<PAD>' that will be used in the TextCNN classification models
set.seed(92092)
unk <- rnorm(300, mean=0, sd=0.01)
baidu[nrow(baidu)+1,] <- unk
rownames(baidu)[nrow(baidu)] <- '<UNK>'
baidu[nrow(baidu)+1,] <- 0
rownames(baidu)[nrow(baidu)] <- '<PAD>'
baidu <- as.matrix(baidu)
write.csv(baidu, file="2.dropna_baidubaike_clean.csv", fileEncoding = "UTF-8", row.names = TRUE)


# Wikipedia
cnwiki <- read.csv("1.tocsv_wiki_word.csv", header=FALSE, stringsAsFactors=FALSE)
cnwiki <- na.omit(cnwiki)
cnwiki <- cnwiki[!duplicated(cnwiki[,1]),]
cnwiki <- cnwiki[!is.na(cnwiki[,1]),]
rownames(cnwiki) <- cnwiki[,1]
cnwiki <- cnwiki[,-1]

set.seed(92092)
unk <- rnorm(300, mean=0, sd=0.01)
cnwiki[nrow(cnwiki)+1,] <- unk
rownames(cnwiki)[nrow(cnwiki)] <- '<UNK>'
cnwiki[nrow(cnwiki)+1,] <- 0
rownames(cnwiki)[nrow(cnwiki)] <- '<PAD>'
cnwiki <- as.matrix(cnwiki)
write.csv(cnwiki, file="2.dropna_wiki_clean.csv", fileEncoding = "UTF-8", row.names = TRUE)


# People's Daily
renmin <- read.csv("1.tocsv_renmin_word.csv", header=FALSE, stringsAsFactors=FALSE)
renmin <- na.omit(renmin)
renmin <- renmin[!duplicated(renmin[,1]),]
renmin <- renmin[!is.na(renmin[,1]),]
rownames(renmin) <- renmin[,1]
renmin <- renmin[,-1]

set.seed(92092)
unk <- rnorm(300, mean=0, sd=0.01)
renmin[nrow(renmin)+1,] <- unk
rownames(renmin)[nrow(renmin)] <- '<UNK>'
renmin[nrow(renmin)+1,] <- 0
rownames(renmin)[nrow(renmin)] <- '<PAD>'
renmin <- as.matrix(renmin)
write.csv(renmin, file="2.dropna_renmin_clean.csv", fileEncoding = "UTF-8", row.names = TRUE)


# Subset by common voocabulary and scale

# baidu vs. wiki
comm <- intersect(rownames(baidu), rownames(cnwiki))

b <- baidu[comm,]
c <- cnwiki[comm,]

b <- scale(b)
c <- scale(c)

save(b, file="3.scaled_baidubaike_vswiki.Rdata")
save(c, file="3.scaled_wiki_vsbaidubaike.Rdata")

# renmin vs. wiki
comm <- intersect(rownames(renmin), rownames(cnwiki))

r <- renmin[comm,]
c <- cnwiki[comm,]

r <- scale(r)
c <- scale(c)

save(r, file="3.scaled_renmin_vswiki.Rdata")
save(c, file="3.scaled_wiki_vsrenmin.Rdata")



