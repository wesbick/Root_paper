library(dplyr)
library(scales)

source("data_setup.R")

alpha <- read.table(file = "data/Bickford.groups.ave-std.summary", header=T)

alpha_mean <- alpha[alpha$method == "ave", ]
alpha_mean$group <- as.character(alpha_mean$group)

meta_alpha <- inner_join(metadata, alpha_mean, by = c("sampleID"="group"))

# removing PLB and SB from this plot because there are no replicates
meta_alpha_sub <- meta_alpha[meta_alpha$Site != "SB" & meta_alpha$Site != "PLB", ]
meta_alpha_sub$Site <- as.character(meta_alpha_sub$Site)
meta_alpha_sub$Site <- as.factor(meta_alpha_sub$Site)

### By Site and Lineage
clrs <- c(BL = "white", CB = "blue", CH = "red", CM = "coral", CR = "grey", Rt2 = "orange")

boxplot(npshannon ~ Site + Lineage, data = meta_alpha_sub,
        ylab = "Shannon Diversity Index",
        ylim = c(0,5.5), col = clrs[levels(meta_alpha_sub$Site)],
        names = rep(x = unique(meta_alpha_sub$Site), times = 2),
        range = 0)

Lineage_convert <- c(Nat = "Native", Inv = "Non-Native")
mtext(side=1, at=c(3.5,9.5), line=3, text = Lineage_convert[levels(meta_alpha$Lineage)], cex = 1.5, font = 2)

hist((meta_alpha_sub$npshannon), breaks = 10)
hist(meta_alpha$npshannon, breaks = 10)

sitebylin <- summary(aov(npshannon ~ Site * Lineage, data = meta_alpha))
sitebylin_sub <- summary(aov(npshannon ~ Site * Lineage, data = meta_alpha_sub))

#By Site
clrs <- c(BL = "white", CB = "blue", CH = "red", CM = "coral", CR = "grey", Rt2 = "orange")

boxplot(npshannon ~ Site, data = meta_alpha_sub,
        ylab = "Shannon Diversity Index",
        ylim = c(0,5.5), col = clrs[levels(meta_alpha_sub$Site)],
        names = unique(meta_alpha_sub$Site),
        range = 0)

Lineage_convert <- c(Nat = "Native", Inv = "Non-Native")
mtext(side=1, at=c(3.5,9.5), line=3, text = Lineage_convert[levels(meta_alpha$Lineage)], cex = 1.5, font = 2)

##By Lineage

clrs <- c(Nat = "white", Inv = "coral")
Lin_convert <- c(Nat = "Native", Inv = "Non-Native")

boxplot(npshannon ~ Lineage, data = meta_alpha,
        ylab = "Shannon Diversity Index",
        ylim = c(0,5.5), col = clrs[levels(meta_alpha$Lineage)],
        names = Lin_convert[levels(meta_alpha$Lineage)],
        range = 0)

#stripcharts
clrs <- c(Nat = "black", Inv = "coral")
Lin_convert <- c(Nat = "Native", Inv = "Non-Native")

stripchart(npshannon ~ Lineage, data = meta_alpha,
           vertical = T,
           method = "jitter",
           jitter = 0.05,
           ylim = c(0,5),
           ylab = "Shannon Diversity Index",
           pch = 19,
           col = clrs[levels(meta_alpha$Lineage)],
           group.names = Lin_convert[levels(meta_alpha$Lineage)])

# par(mar = c(15,5, 15,5))
# clrs <- c(BL = "black", CB = "blue", CH = "red", CM = "coral", CR = "grey", Rt2 = "orange")
# stripchart(npshannon ~ Site, data = meta_alpha_sub, subset = Lineage=="Inv",
#            vertical = T,
#            method = "jitter",
#            jitter = 0.2,
#            xlim = c(0.5,7),
#            ylim = c(0,5),
#            ylab = "Shannon Diversity Index",
#            pch = 19,
#            col = alpha(clrs[levels(meta_alpha_sub$Site)], alpha = 0.5),
#            axes=F)
# 
# stripchart(npshannon ~ Site, data = meta_alpha_sub, subset = Lineage=="Nat",
#            vertical = T,
#            method = "jitter",
#            jitter = 0.2,
#            pch = 19,
#            col = alpha(clrs[levels(meta_alpha_sub$Site)], alpha = 0.25),
#            add = T,
#            at = c(7.5,8.5,9.5,10.5,11.5,12.5))
# 
# axis(1, labels = rep(unique(meta_alpha_sub$Site), 2), at = c(1,2,3,4,5,6,7.5,8.5,9.5,10.5,11.5,12.5))
# box()
# mtext(side=1, at=c(2,5.5), line=3, text = gender_convert[levels(meta_alpha$Gender)], cex = 1.5, font = 2)
