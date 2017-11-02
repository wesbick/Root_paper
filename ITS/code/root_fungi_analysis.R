library(permute); library(vegan); library("dplyr")

source("data_setup.R")
shared <- read.table("data/Bickford.subsample.shared", header = T, sep = "\t")
rownames(shared) <- shared[,2]
shared <- shared[ , -c(1:3)]

permfungi <- adonis(shared ~ Site * Lineage, data=metadata)
permfungi

NMDS <- metaMDS(shared, try = 100)
NMDS_scores <- scores(NMDS)

NMDS1 <- NMDS_scores[ ,1]
NMDS2 <- NMDS_scores[ ,2]


fig <- ordiplot(NMDS, choices = c(1,2), type = "none", xlim = c(-1,1), ylim = c(-1.5,1.5) )
clrs <- c(BL = "blue", CB = "red", CH = "dark green", CM = "coral", CR = "black", PLB = "orange", Rt2 = "gray", SB = "purple")
pchs <- c(Nat = 21, Inv = 23)
points(fig, "sites", pch = pchs[as.character(metadata$Lineage)], 
       col = "black", bg = clrs[as.character(metadata$Site)], cex = 1.5)
ordiellipse(NMDS, metadata$Site == "BL", show.groups = TRUE, kind = "sd", col = "blue", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Site == "CB", show.groups = TRUE, kind = "sd", col = "red", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Site == "CH", show.groups = TRUE, kind = "sd", col = "dark green", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Site == "CM", show.groups = TRUE, kind = "sd", col = "coral", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Site == "CR", show.groups = TRUE, kind = "sd", col = "black", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Site == "Rt2", show.groups = TRUE, kind = "sd", col = "gray", lty = 1, lwd = 2.0)

legend_text <- c("Bullard Lake", "Cecil Bay", "Chelsea", "Cheboygan Marsh", "Castle Rock", "Point LeBarb", "Route 2", "Sturgeon Bay")
legend("topright", legend_text, pch = 15, 
       col = c("blue","red","dark green","coral","black","orange","gray","purple"))

legend_text2 <- c("Native", "Non-Native")
legend("topleft", legend_text2, pch = c(1,5))

library(ggplot2)

ggplot(data = NMDS_scores, aes(x = NMDS1, y = NMDS2, size = pop, color = metadata$Site)) +
  geom_point()


lin_fig <- ordiplot(NMDS, choices = c(1,2), type = "none", xlim = c(-1,1), ylim = c(-1,1) )
clrs <- c(Nat = "blue", Inv = "red")
points(lin_fig, "sites", pch = 19, 
       col = clrs[as.character(metadata$Lineage)])
ordiellipse(NMDS, metadata$Lineage == "Nat", show.groups = TRUE, kind = "sd", col = "blue", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Lineage == "Inv", show.groups = TRUE, kind = "sd", col = "red", lty = 1, lwd = 2.0)


