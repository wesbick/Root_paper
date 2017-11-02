library(permute); library(vegan); library("dplyr")

source("community analysis.R")

## This is an NMDS by site and lineage. Permanova showed a significant difference by site, but not Lineage
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


## this is an NMDS by lineage across all sites. No significant difference by lineage (PerManova)
lin_fig <- ordiplot(NMDS, choices = c(1,2), type = "none", xlim = c(-1,1), ylim = c(-1,1) )
clrs <- c(Nat = "blue", Inv = "red")
points(lin_fig, "sites", pch = 19, 
       col = clrs[as.character(metadata$Lineage)])
ordiellipse(NMDS, metadata$Lineage == "Nat", show.groups = TRUE, kind = "sd", col = "blue", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Lineage == "Inv", show.groups = TRUE, kind = "sd", col = "red", lty = 1, lwd = 2.0)

#same plot as first, but using ggplot

library(ggplot2)

ggplot(data = NMDS_scores, aes(x = NMDS1, y = NMDS2, shape = metadata$Lineage, color = metadata$Site)) +
  geom_point()


# using quartz
summary(NMDS_scores)

quartz.options(height=5, width=5)
plot.new() 
par(oma = c(1, 1, 1, 1))
par(mar = c( 5, 5, 0, 0 ))
plot.window(xlim = c(-1,1), ylim = c(-1,1))
axis(side = 1)
axis(side = 2, las = 1)
points(x = NMDS_scores$NMDS1, y = NMDS_scores$NMDS2, pch = pchs[as.character(metadata$Lineage)], 
      bg = clrs[as.character(metadata$Site)], cex = 1.5)

mtext(side = 1, line = 2, text = "NMDS1")
mtext(side = 2, line = 3, text = "NMDS2" )

ordiellipse(NMDS, metadata$Site == "BL", show.groups = TRUE, kind = "sd", col = "blue", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Site == "CB", show.groups = TRUE, kind = "sd", col = "red", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Site == "CH", show.groups = TRUE, kind = "sd", col = "dark green", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Site == "CM", show.groups = TRUE, kind = "sd", col = "coral", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Site == "CR", show.groups = TRUE, kind = "sd", col = "black", lty = 1, lwd = 2.0)
ordiellipse(NMDS, metadata$Site == "Rt2", show.groups = TRUE, kind = "sd", col = "gray", lty = 1, lwd = 2.0)
