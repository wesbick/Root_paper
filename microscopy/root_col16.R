library(nortest)
percol16 <- read.csv("percol16.csv", stringsAsFactors = F)
percol16 <- percol16[percol16$Site != "JC", ]
percol16 <- percol16[-40, ]

percol16$Site <- as.factor(percol16$Site)

percol_nova_nt <- aov(percol16$percol ~ percol16$Site * percol16$Lineage)
summary(percol_nova_nt)

in_clear_nova_nt <- aov(percol16$IN_Clear.1 ~ percol16$Site * percol16$Lineage)
summary(in_clear_nova_nt)

in_ds_nova_nt <- aov(percol16$IN_dark_septate.1 ~ percol16$Site * percol16$Lineage)
summary(in_ds_nova_nt)

in_as_nova_nt <- aov(percol16$IN_dark_aseptate.1 ~ percol16$Site * percol16$Lineage)
summary(in_as_nova_nt)

out_clear_nova_nt <- aov(percol16$OUT_clear.1 ~ percol16$Site * percol16$Lineage)
summary(out_clear_nova_nt)

out_ds_nova_nt <- aov(percol16$OUT_dark_septate.1 ~ percol16$Site * percol16$Lineage)
summary(out_ds_nova_nt)

out_as_nova_nt <- aov(percol16$OUT_dark_aseptate.1 ~ percol16$Site * percol16$Lineage)
summary(out_as_nova_nt)

nt_percol_mat <- matrix(NA, nrow = 6, ncol = 4)
rownames(nt_percol_mat) <- unique(percol16$Site)
colnames(nt_percol_mat) <- c("Invasive", "Inv_sterr", "Native", "Nat_sterr")

for(ii in rownames(nt_percol_mat)){
  sub <- percol16[percol16$Site == ii & percol16$Lineage == "Inv", ]
  num <- nrow(sub)
  site <- rep(NA, num)
  for(pp in 1:num){
    site[pp] <- sub$percol[pp]
  }
  nt_percol_mat[ii, 1] <- mean(site)
  nt_percol_mat[ii, 2] <- (sd(site))/(sqrt(num))
  sub <- percol16[percol16$Site == ii & percol16$Lineage == "Nat", ]
  num <- nrow(sub)
  site <- rep(NA, num)
  for(pp in 1:num){
    site[pp] <- sub$percol[pp]
  }
  nt_percol_mat[ii, 3] <- mean(site)
  nt_percol_mat[ii, 4] <- (sd(site))/(sqrt(num))
}
nt_by_site <- as.data.frame(nt_percol_mat)

quartz.options(height=6 , width = 6)
plot.new()
par(oma = c(1, 1, 1, 1))
plot.window(xlim = c(0,0.60), ylim = c(0, 0.6))
axis(1)
axis(2)

points(x = nt_by_site$Native, y = nt_by_site$Invasive, pch = 20)
arrows(x0 = nt_by_site$Native, x1 = nt_by_site$Native + nt_by_site$Nat_sterr, y0 = nt_by_site$Invasive, angle = 0)
arrows(x0 = nt_by_site$Native, x1 = nt_by_site$Native - nt_by_site$Nat_sterr, y0 = nt_by_site$Invasive, angle = 0)
arrows(x0 = nt_by_site$Native, y0 = nt_by_site$Invasive, y1 = nt_by_site$Invasive + nt_by_site$Inv_sterr, angle = 0)
arrows(x0 = nt_by_site$Native, y0 = nt_by_site$Invasive, y1 = nt_by_site$Invasive - nt_by_site$Inv_sterr, angle = 0)
box()
lines(x = c(0,0.5,1), y = c(0,0.5,1), type = "l")
mtext(side = 1, line = 3, text = "Native Percent Colonization")
mtext(side = 2, line = 3, text = "Non-Native Percent Colonization")
text(x = nt_by_site$Native, y = nt_by_site$Invasive, labels = rownames(nt_by_site), adj = c(0,0), cex = 0.7)

nt_out_cl_mat <- matrix(NA, nrow = 6, ncol = 4)
rownames(nt_out_cl_mat) <- unique(percol16$Site)
colnames(nt_out_cl_mat) <- c("Invasive", "Inv_sterr", "Native", "Nat_sterr")

for(ii in rownames(nt_percol_mat)){
  sub <- percol16[percol16$Site == ii & percol16$Lineage == "Inv", ]
  num <- nrow(sub)
  site <- rep(NA, num)
  for(pp in 1:num){
    site[pp] <- sub$OUT_clear.1[pp]
  }
  nt_out_cl_mat[ii, 1] <- mean(site)
  nt_out_cl_mat[ii, 2] <- (sd(site))/(sqrt(num))
  sub <- percol16[percol16$Site == ii & percol16$Lineage == "Nat", ]
  num <- nrow(sub)
  site <- rep(NA, num)
  for(pp in 1:num){
    site[pp] <- sub$OUT_clear.1[pp]
  }
  nt_out_cl_mat[ii, 3] <- mean(site)
  nt_out_cl_mat[ii, 4] <- (sd(site))/(sqrt(num))
}
nt_out_cl_by_site <- as.data.frame(nt_out_cl_mat)

quartz.options(height=6 , width = 6)
plot.new()
par(oma = c(1, 1, 1, 1))
plot.window(xlim = c(0,0.45), ylim = c(0, 0.45))
axis(1)
axis(2)

points(x = nt_out_cl_by_site$Native, y = nt_out_cl_by_site$Invasive, pch = 20)
arrows(x0 = nt_out_cl_by_site$Native, x1 = nt_out_cl_by_site$Native + nt_out_cl_by_site$Nat_sterr, y0 = nt_out_cl_by_site$Invasive, angle = 0)
arrows(x0 = nt_out_cl_by_site$Native, x1 = nt_out_cl_by_site$Native - nt_out_cl_by_site$Nat_sterr, y0 = nt_out_cl_by_site$Invasive, angle = 0)
arrows(x0 = nt_out_cl_by_site$Native, y0 = nt_out_cl_by_site$Invasive, y1 = nt_out_cl_by_site$Invasive + nt_out_cl_by_site$Inv_sterr, angle = 0)
arrows(x0 = nt_out_cl_by_site$Native, y0 = nt_out_cl_by_site$Invasive, y1 = nt_out_cl_by_site$Invasive - nt_out_cl_by_site$Inv_sterr, angle = 0)
box()
lines(x = c(0,0.5,1), y = c(0,0.5,1), type = "l")
mtext(side = 1, line = 3, text = "Native Percent Colonization")
mtext(side = 2, line = 3, text = "Non-Native Percent Colonization")
text(x = nt_out_cl_by_site$Native, y = nt_out_cl_by_site$Invasive, labels = rownames(nt_out_cl_by_site), adj = c(0,0), cex = 0.7)


hist(percol16$percol, breaks = 10)
ad.test(percol16$percol)

hist(percol16$OUT_clear.1, breaks = 10)
ad.test(percol16$OUT_clear.1)

#kruskal.test(percol16$percol ~ percol16$Lineage)

hist((percol16$percol)^(1/3), breaks = 10)
ad.test((percol16$percol)^(1/3))

hist(log(percol16$OUT_clear.1), breaks = 10)
ad.test(log(percol16$OUT_clear.1))


t_percol <- (percol16$percol)^(1/3)
t_outclear <- (log(percol16$OUT_clear.1))
t_percol16 <- data.frame(Cassette = percol16$cassette, Site = percol16$Site, Lineage = percol16$Lineage, Rep = percol16$Rep, percol = t_percol, out_clear = t_outclear) 
t_percol16 <- t_percol16[-40, ]

t_percol16$Site <- as.factor(t_percol16$Site) 
t_percol16$Lineage <- as.factor(t_percol16$Lineage)

percol_nova <- aov(t_percol16$percol ~ t_percol16$Site * t_percol16$Lineage)
summary(percol_nova)

percol_cl_nova <- aov(t_percol16$out_clear ~ t_percol16$Site * t_percol16$Lineage)
summary(percol_cl_nova)

length(unique(t_percol16$Site))

percol_mat <- matrix(NA, nrow = 6, ncol = 4)
rownames(percol_mat) <- unique(t_percol16$Site)
colnames(percol_mat) <- c("Invasive", "Inv_sterr", "Native", "Nat_sterr")

for(ii in rownames(percol_mat)){
  sub <- t_percol16[t_percol16$Site == ii & t_percol16$Lineage == "Inv", ]
  num <- nrow(sub)
  site <- rep(NA, num)
  for(pp in 1:num){
    site[pp] <- sub$percol[pp]
  }
  percol_mat[ii, 1] <- mean(site)
  percol_mat[ii, 2] <- (sd(site))/(sqrt(num))
  sub <- t_percol16[t_percol16$Site == ii & t_percol16$Lineage == "Nat", ]
  num <- nrow(sub)
  site <- rep(NA, num)
  for(pp in 1:num){
    site[pp] <- sub$percol[pp]
  }
  percol_mat[ii, 3] <- mean(site)
  percol_mat[ii, 4] <- (sd(site))/(sqrt(num))
}
by_site <- as.data.frame(percol_mat)

quartz.options(height=6 , width = 6)
plot.new()
par(oma = c(1, 1, 1, 1))
plot.window(xlim = c(0.4,0.85), ylim = c(0.4, 0.85))
axis(1)
axis(2)

points(x = by_site$Native, y = by_site$Invasive, pch = 20)
arrows(x0 = by_site$Native, x1 = by_site$Native + by_site$Nat_sterr, y0 = by_site$Invasive, angle = 0)
arrows(x0 = by_site$Native, x1 = by_site$Native - by_site$Nat_sterr, y0 = by_site$Invasive, angle = 0)
arrows(x0 = by_site$Native, y0 = by_site$Invasive, y1 = by_site$Invasive + by_site$Inv_sterr, angle = 0)
arrows(x0 = by_site$Native, y0 = by_site$Invasive, y1 = by_site$Invasive - by_site$Inv_sterr, angle = 0)
box()
lines(x = c(0,0.5,1), y = c(0,0.5,1), type = "l")
mtext(side = 1, line = 3, text = "Native Percent Colonization (transformed)")
mtext(side = 2, line = 3, text = "Non-Native Percent Colonization (transformed)")
text(x = by_site$Native, y = by_site$Invasive, labels = rownames(by_site), adj = c(0,0), cex = 0.7)

