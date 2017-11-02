source("data_setup.R")
source("for_funguild.R")
source("community_by_guild.R")

trophic_shared

rel_abund <- trophic_shared / apply(trophic_shared, 1, sum)
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

otu_test1 <- kruskal.test(rel_abund_subset[, "Symbiotroph"] ~ metadata$Site)
otu_test1

str(otu_test1)

test <- function(rabund, site){
  otu_test <- kruskal.test(rabund ~ site)
  return(otu_test$p.value)
}

p_values <- apply(rel_abund_subset, 2, test, site=metadata$Site)
sum(p_values < 0.05)
p_values_adjusted <- p.adjust(p_values, method="BH")
min(p_values_adjusted) # Post-hoc: no individual trophic mode relabunds are significantly different by site

otu_test2 <- kruskal.test(rel_abund_subset[, "Symbiotroph"] ~ metadata$Lineage)
otu_test2 # pre-planned: Symbiotroph rel abundances differ between lineages

sig_relabund <- rel_abund_subset[ , "Symbiotroph"]

median_sig_relabund <- aggregate(sig_relabund, by=list(metadata$Lineage), median)

par(mar=c(5,8,0.5,0.5))
plot(NA, xlim=c(0,0.25), ylim=c(19,1), xlab="Relative Abundance (%)", ylab="", axes=F)


stripchart(sig_relabund ~ metadata$Lineage, vertical=F, method="jitter", jitter=0.3, axes=F, col=c("gray", "blue"), pch=19)
  

axis(2, labels="Symbiotrophs", at=c(1.5), tick=F, las=2)
axis(1, at=seq(0,1, 0.2), label=seq(0,100, 20), las=1)
box()

legend(y=2.6, x=0.19, legend=c("Non-Native", "Native"), col=c("gray", "blue"), pch=19)


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

#doing same analysis with growth morphology

growth_form_shared

rel_abund <- growth_form_shared / apply(growth_form_shared, 1, sum)
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

otu_test1 <- kruskal.test(rel_abund_subset[,3] ~ metadata$Site)
otu_test1

str(otu_test1)

test <- function(rabund, site){
  otu_test <- kruskal.test(rabund ~ site)
  return(otu_test$p.value)
}

p_values <- apply(rel_abund_subset, 2, test, site=metadata$Site)
sum(p_values < 0.05)
p_values_adjusted <- p.adjust(p_values, method="BH")
min(p_values_adjusted) # no individual growth form relabunds are significantly different by site

otu_test2 <- kruskal.test(rel_abund_subset[,"Dark Septate Endophyte"] ~ metadata$Lineage)
otu_test2 # this says that the relative abundance of dark septate endophytes is not different by Lineage

p.adjust(otu_test2$p.value, method = "BH")

str(otu_test2)

test <- function(rabund, site){
  otu_test <- kruskal.test(rabund ~ site)
  return(otu_test$p.value)
}

p_values2 <- apply(rel_abund_subset, 2, test, site=metadata$Lineage)
sum(p_values2 < 0.05)
p_values2[p_values2 < 0.05]

p_values_adjusted2 <- p.adjust(p_values2, method="BH")
min(p_values_adjusted2) # no individual otu relabunds are significantly different by Lineage

