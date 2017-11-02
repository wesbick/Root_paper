library(vegan)

source("for_funguild.R")

perm_trophic <- adonis(trophic_shared ~ Site * Lineage, data = metadata)
perm_trophic

trophicNMDS <- metaMDS(trophic_shared, try = 20, trymax = 300, k=2)
trophic_scores <- scores(trophicNMDS)

trophic_scores <- as.data.frame(trophic_scores)

quartz.options(height=5, width=5)
plot.new() 
par(oma = c(1, 1, 1, 1))
par(mar = c( 5, 5, 0, 0 ))
plot.window(xlim = c(-1,1), ylim = c(-1,1))
axis(side = 1)
axis(side = 2, las = 1)
clrs <- c(BL = "blue", CB = "red", CH = "dark green", CM = "coral", CR = "black", PLB = "orange", Rt2 = "gray", SB = "purple")
points(x = trophic_scores$NMDS1, y = trophic_scores$NMDS2, pch = pchs[as.character(metadata$Lineage)], 
       bg = clrs[as.character(metadata$Site)], cex = 1.5)

mtext(side = 1, line = 2, text = "NMDS1")
mtext(side = 2, line = 3, text = "NMDS2" )

ordiellipse(trophicNMDS, metadata$Site == "BL", show.groups = TRUE, kind = "sd", col = "blue", lty = 1, lwd = 2.0)
ordiellipse(trophicNMDS, metadata$Site == "CB", show.groups = TRUE, kind = "sd", col = "red", lty = 1, lwd = 2.0)
ordiellipse(trophicNMDS, metadata$Site == "CH", show.groups = TRUE, kind = "sd", col = "dark green", lty = 1, lwd = 2.0)
ordiellipse(trophicNMDS, metadata$Site == "CM", show.groups = TRUE, kind = "sd", col = "coral", lty = 1, lwd = 2.0)
ordiellipse(trophicNMDS, metadata$Site == "CR", show.groups = TRUE, kind = "sd", col = "black", lty = 1, lwd = 2.0)
ordiellipse(trophicNMDS, metadata$Site == "Rt2", show.groups = TRUE, kind = "sd", col = "gray", lty = 1, lwd = 2.0)

legend_text <- c("Bullard Lake", "Cecil Bay", "Chelsea", "Cheboygan Marsh", "Castle Rock", "Point LeBarb", "Route 2", "Sturgeon Bay")
legend("topright", legend_text, pch = 15, 
       col = c("blue","red","dark green","coral","black","orange","gray","purple"))

legend_text2 <- c("Native", "Non-Native")
legend("topleft", legend_text2, pch = c(1,5))

perm_guild <- adonis(all_guild_shared ~ Site * Lineage, data = metadata)
perm_guild

perm_gform <- adonis(growth_form_shared ~ Site * Lineage, data = metadata)
perm_gform
