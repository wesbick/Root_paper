library(permute); library(vegan); library("dplyr")

source("data_setup.R")
shared <- read.table(file= "data/Bickford.subsample.shared", header = T, stringsAsFactors = F, row.names = 2)
shared <- shared[ ,-c(1,2)]

permfungi <- adonis(shared ~ Site * Lineage, data=metadata, method = "bray")
permfungi

NMDS <- metaMDS(shared, try = 20, trymax = 300, k=2)
NMDS_scores <- scores(NMDS)

NMDS1 <- NMDS_scores[ ,1]
NMDS2 <- NMDS_scores[ ,2]

NMDS_scores <- as.data.frame(NMDS_scores)





