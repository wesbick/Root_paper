library(scales)
source('data_setup.R')

pca <- read.table(file="data/Bickford.subsample.1.pca.axes", header=T)

#this makes sure the samples are in the same order in both files
stopifnot(pca$group == metadata$sampleID)


plot(x=pca$axis1, y=pca$axis2, xlab="PCA Axis 1", ylab="PCA Axis 2",
     xlim=c(min(pca$axis1), max(pca$axis1)), ylim=c(min(pca$axis2), max(pca$axis2)), 
     pch=16, col="blue", cex=0.7,
     main="PCA")
# I would like to see different treatment types in different colors or symbols
# I would like more information about the axes (percent variability explained)

# this changes the opacity of points so you can tell when points are overlapping
plot(x=pca$axis1, y=pca$axis2, xlab="PCA Axis 1", ylab="PCA Axis 2",
     xlim=c(min(pca$axis1), max(pca$axis1)), ylim=c(min(pca$axis2), max(pca$axis2)), 
     pch=16, col=alpha("blue", alpha = 0.25), cex=0.7,
     main="PCA")

# another way to visualize this is to put a black circle around the points
plot(x=pca$axis1, y=pca$axis2, xlab="PCA Axis 1", ylab="PCA Axis 2",
     xlim=c(min(pca$axis1), max(pca$axis1)), ylim=c(min(pca$axis2), max(pca$axis2)), 
     pch=21, col="black", bg= "red", cex=0.7, lwd = 2,
     main="PCA")

#changing transparency of only bg color
plot(x=pca$axis1, y=pca$axis2, xlab="PCA Axis 1", ylab="PCA Axis 2",
     xlim=c(min(pca$axis1), max(pca$axis1)), ylim=c(min(pca$axis2), max(pca$axis2)), 
     pch=21, col="black", bg= alpha("red", alpha = 0.25), cex=0.7, lwd = 2,
     main="PCA")

## incorporating metadata

# adding colors to indicate treatment

clrs <- c(BL = "red", CB = "blue", CH = "purple", CM = "black", CR = "orange", PLB = "grey", Rt2 = "white", SB = "dark green")
sym <- c(Inv = 21, Nat = 22)
plot(x=pca$axis1, y=pca$axis2, xlab="PCA Axis 1", ylab="PCA Axis 2",
     xlim=c(min(pca$axis1), max(pca$axis1)), ylim=c(min(pca$axis2), max(pca$axis2)), 
     pch=sym[as.character(metadata$Lineage)], col="black", bg= clrs[as.character(metadata$Site)], cex=1, lwd = 1,
     main="PCA by Site and Lineage")

stopifnot(pca$group == metadata$sampleID)
