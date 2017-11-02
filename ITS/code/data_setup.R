setwd('~/git_repos/Root_ITS')

metadata <- read.table("data/Bickford.count.summary", sep = "\t", header = F, stringsAsFactors = F)

colnames(metadata) <- c("sampleID", "n_seqs")

metadata <-data.frame(metadata, Site= c("BL", "BL","BL", "BL","BL", "BL","BL", "BL","BL", "BL","BL", "BL", "CB", "CB", "CB", "CB", "CB", "CB", "CH", "CH", "CH", "CH", "CH", "CH", "CH", "CH", "CM", "CM", "CM", "CM", "CM", "CM", "CR", "CR", "CR", "CR", "CR", "PLB", "PLB", "Rt2", "Rt2", "Rt2", "Rt2", "Rt2", "Rt2", "SB", "SB"))
metadata <-data.frame(metadata, Lineage= c("Inv", "Inv","Inv", "Inv","Inv", "Inv","Nat", "Nat","Nat", "Nat","Nat", "Nat", "Inv", "Inv", "Inv", "Nat", "Nat", "Nat", "Inv", "Inv", "Inv", "Inv", "Nat", "Nat", "Nat", "Nat", "Inv", "Inv", "Inv", "Nat", "Nat", "Nat", "Inv", "Inv", "Inv", "Nat", "Nat", "Inv", "Nat", "Inv", "Inv", "Inv", "Nat", "Nat", "Nat", "Inv", "Nat"))
metadata <-data.frame(metadata, Rep= c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,1,2,3,1,3,4,5,2,3,4,5,1,2,3,1,2,3,1,2,3,1,3,1,1,1,2,3,1,2,3,1,1))

rownames(metadata) <- metadata$sampleID

write.table(metadata, "data/Bickford.metadata.txt")

summary(metadata)
metadata$Site <- as.factor(metadata$Site)
metadata$Lineage <- as.factor(metadata$Lineage)
