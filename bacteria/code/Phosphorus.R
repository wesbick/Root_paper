
Pdist <- matrix(NA, nrow = 38, ncol = 38)
rownames(Pdist) <- rownames(ma_finite)
colnames(Pdist) <- rownames(ma_finite)
ex <- rep(NA, 38)

SoilP <- ma_finite$X15SoilP
names(SoilP) <- rownames(ma_finite)

for(ii in 1:length(rownames(Pdist))){
  x <- 1
  for (i in rownames(Pdist)){
    ex[x] <- abs(SoilP[i] - SoilP[ii])
    x <- x + 1
  }
  
  Pdist[ ,ii] <- ex
}
Pdiff_vec <- Pdiff[1, ]

for(i in 2:38){
  Pdiff_vec <- c(Pdiff_vec, Pdiff[i, ])
}
length(Pdiff_vec)

braydist <- read.csv("data/braydist.csv", row.names = 1, header = T)
braydist_P <- braydist[rownames(Pdiff), colnames(Pdiff)]

braydist_P_vec <- braydist_P[1, ]
for(i in 2:38){
  braydist_P_vec <- c(braydist_P_vec, braydist_P[i, ])
}
length(braydist_P_vec)


quartz.options(height=5, width=5)
plot.new() 
par(oma = c(1, 1, 1, 1))
par(mar = c( 5, 5, 0, 0 ))
plot.window(xlim = c(0,90), ylim = c(0,1))
axis(side = 1)
axis(side = 2, las = 1)
points(x = Pdiff_vec, y = braydist_P_vec, pch = 15, 
       bg = "black")

mtext(side = 1, line = 2, text = "Percent N")
mtext(side = 2, line = 3, text = "NMDS1 scores" )

