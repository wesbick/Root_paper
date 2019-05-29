Ndist <- matrix(NA, nrow = 38, ncol = 38)
rownames(ma_finite) <- ma_finite$Code
rownames(Ndist) <- rownames(ma_finite)
colnames(Ndist) <- rownames(ma_finite)
ex <- rep(NA, 38)

SoilN <- ma_finite$X15SoilN
names(SoilN) <- rownames(ma_finite)

for(ii in 1:length(rownames(Ndist))){
  x <- 1
  for (i in rownames(Ndist)){
    ex[x] <- abs(SoilN[i] - SoilN[ii])
    x <- x + 1
  }
  
  Ndist[ ,ii] <- ex
}
Ndiff <- as.data.frame(Ndist)
Ndiff_vec <- Ndiff[ , 1]

for(i in 2:38){
  Ndiff_vec <- c(Ndiff_vec, Ndiff[ , i ])
}
length(Ndiff_vec)

braydist <- read.csv("data/braydist.csv", row.names = 1, header = T)
braydist_N <- braydist[rownames(Ndiff), colnames(Ndiff)]

braydist_N_vec <- braydist_N[, 1]
for(i in 2:38){
  braydist_N_vec <- c(braydist_N_vec, braydist_N[ ,i])
}
length(braydist_N_vec)

reg_Ndiff <- lm(braydist_N_vec ~ Ndiff_vec)
summary(reg_Ndiff)

quartz.options(height=5, width=5)
plot.new() 
par(oma = c(1, 1, 1, 1))
par(mar = c( 5, 5, 0, 0 ))
plot.window(xlim = c(0,3), ylim = c(0,1))
axis(side = 1)
axis(side = 2, las = 1)
points(x = Ndiff_vec, y = braydist_N_vec, pch = 15, 
       bg = "black")
abline(lm(braydist_N_vec ~ Ndiff_vec))
mtext(side = 1, line = 2, text = "Percent N")
mtext(side = 2, line = 3, text = "Bray-Curtis Distance" )


reg_Ndiff <- lm(braydist_N_vec ~ Ndiff_vec)
summary(reg_Ndiff)
