source("data_setup.R")

shared <- read.table(file="data/Bickford.subsample.shared", header=T, stringsAsFactors=F, row.names=2)
shared <- shared[,-c(1,2)]

rel_abund <- shared / apply(shared, 1, sum)
mean_rel_abund <- apply(rel_abund, 2, mean)
otu_order <- order(mean_rel_abund, decreasing=T)
rel_abund <- rel_abund[,otu_order]

stopifnot(rownames(shared) == metadata$sampleID)

mean_rel_abund <- apply(rel_abund, 2, mean)
abundant <- mean_rel_abund > 0.001
rel_abund_subset <- rel_abund[, abundant]
shared_abundant <- shared[ , abundant]

total_rel_abund <- apply(rel_abund_subset, 1, sum)
range(total_rel_abund)

otu_test1 <- kruskal.test(rel_abund_subset[,1] ~ metadata$Site)
otu_test1

str(otu_test1)

test <- function(rabund, site){
  otu_test <- kruskal.test(rabund ~ site)
  return(otu_test$p.value)
}

p_values <- apply(rel_abund_subset, 2, test, site=metadata$Site)
sum(p_values < 0.05)
p_values_adjusted <- p.adjust(p_values, method="BH")
min(p_values_adjusted) # no individual otu relabunds are significantly different by site

otu_test2 <- kruskal.test(rel_abund_subset[,1] ~ metadata$Lineage)
otu_test2

str(otu_test1)

test <- function(rabund, site){
  otu_test <- kruskal.test(rabund ~ site)
  return(otu_test$p.value)
}

p_values2 <- apply(rel_abund_subset, 2, test, site=metadata$Lineage)
sum(p_values2 < 0.05)
p_values_adjusted2 <- p.adjust(p_values2, method="BH")
min(p_values_adjusted2) # no individual otu relabunds are significantly different by Lineage

shared_abundant <- shared[ , abundant]

permabundant <- adonis(shared_abundant ~ Site * Lineage, data = metadata)
permabundant

