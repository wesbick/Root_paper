tisNdist <- matrix(NA, nrow = 41, ncol = 41)
rownames(tisNdist) <- rownames(tisma_finite)
colnames(tisNdist) <- rownames(tisma_finite)
ex <- rep(NA, 41)

tisN <- tisma_finite$X15TisN
names(tisN) <- rownames(tisma_finite)

for(ii in 1:length(rownames(tisNdist))){
  x <- 1
  for (i in rownames(tisNdist)){
    ex[x] <- abs(tisN[i] - tisN[ii])
    x <- x + 1
  }
  
  tisNdist[ ,ii] <- ex
}
tisNdiff <- as.data.frame(tisNdist)
tisNdiff_vec <- tisNdiff[ , 1]

for(i in 2:41){
  tisNdiff_vec <- c(tisNdiff_vec, tisNdiff[ , i ])
}
length(tisNdiff_vec)

braydist <- read.csv("data/braydist.csv", row.names = 1, header = T)
braydist_tisN <- braydist[rownames(tisNdiff), colnames(tisNdiff)]

braydist_tisN_vec <- braydist_tisN[, 1]
for(i in 2:41){
  braydist_tisN_vec <- c(braydist_tisN_vec, braydist_tisN[ ,i])
}
length(braydist_tisN_vec)

reg_tisNdiff <- lm(braydist_tisN_vec ~ tisNdiff_vec)
summary(reg_tisNdiff)

quartz.options(height=5, width=5)
plot.new() 
par(oma = c(1, 1, 1, 1))
par(mar = c( 5, 5, 0, 0 ))
plot.window(xlim = c(0,3.5), ylim = c(0,1))
axis(side = 1)
axis(side = 2, las = 1)
points(x = tisNdiff_vec, y = braydist_tisN_vec, pch = 15, 
       bg = "black")
abline(lm(braydist_tisN_vec ~ tisNdiff_vec))
mtext(side = 1, line = 2, text = "Tissue N Anomaly")
mtext(side = 2, line = 3, text = "Bray-Curtis Distance" )


reg_Ndiff <- lm(braydist_N_vec ~ Ndiff_vec)
summary(reg_Ndiff)

#####Tissue P

tisPdist <- matrix(NA, nrow = 41, ncol = 41)
rownames(tisPdist) <- rownames(tisma_finite)
colnames(tisPdist) <- rownames(tisma_finite)
ex <- rep(NA, 41)

tisP <- tisma_finite$X15TisP
names(tisP) <- rownames(tisma_finite)

for(ii in 1:length(rownames(tisPdist))){
  x <- 1
  for (i in rownames(tisPdist)){
    ex[x] <- abs(tisP[i] - tisP[ii])
    x <- x + 1
  }
  
  tisPdist[ ,ii] <- ex
}
tisPdiff <- as.data.frame(tisPdist)
tisPdiff_vec <- tisPdiff[ , 1]

for(i in 2:41){
  tisPdiff_vec <- c(tisPdiff_vec, tisPdiff[ , i ])
}
length(tisPdiff_vec)

braydist <- read.csv("data/braydist.csv", row.names = 1, header = T)
braydist_tisN <- braydist[rownames(tisNdiff), colnames(tisNdiff)]

braydist_tisN_vec <- braydist_tisN[, 1]
for(i in 2:41){
  braydist_tisN_vec <- c(braydist_tisN_vec, braydist_tisN[ ,i])
}
length(braydist_tisN_vec)

reg_tisPdiff <- lm(braydist_tisN_vec ~ tisPdiff_vec)
summary(reg_tisPdiff)

quartz.options(height=5, width=5)
plot.new() 
par(oma = c(1, 1, 1, 1))
par(mar = c( 5, 5, 0, 0 ))
plot.window(xlim = c(0,0.3), ylim = c(0,1))
axis(side = 1)
axis(side = 2, las = 1)
points(x = tisPdiff_vec, y = braydist_tisN_vec, pch = 15, 
       bg = "black")
abline(lm(braydist_tisN_vec ~ tisPdiff_vec), col = "blue")
mtext(side = 1, line = 2, text = "Tissue P Anomaly")
mtext(side = 2, line = 3, text = "Bray-Curtis Distance" )



