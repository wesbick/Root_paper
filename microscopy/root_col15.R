
percol15 <- read.csv("root_col_15.csv", stringsAsFactors = F)

percol_nova_nt <- aov(percol15$per_col ~ percol15$Site * percol16$Lineage)
summary(percol_nova_nt)

length(unique(percol15$Site))

nt_percol_mat <- matrix(NA, nrow = 8, ncol = 4)
rownames(nt_percol_mat) <- unique(percol15$Site)
colnames(nt_percol_mat) <- c("Invasive", "Inv_sterr", "Native", "Nat_sterr")

for(ii in rownames(nt_percol_mat)){
  sub <- percol15[percol15$Site == ii & percol15$Lineage == "Inv", ]
  num <- nrow(sub)
  site <- rep(NA, num)
  if(num > 1){
    for(pp in 1:num){
      site[pp] <- sub$per_col[pp]
    }
  }else{
    site <- sub$per_col
  }
  nt_percol_mat[ii, 1] <- mean(site)
  nt_percol_mat[ii, 2] <- (sd(site))/(sqrt(num))
  sub <- percol15[percol15$Site == ii & percol15$Lineage == "Nat", ]
  num <- nrow(sub)
  site <- rep(NA, num)
  if(num > 1){
    for(pp in 1:num){
      site[pp] <- sub$per_col[pp]
    }
  }else{
    site <- sub$per_col
  }
  nt_percol_mat[ii, 3] <- mean(site)
  nt_percol_mat[ii, 4] <- (sd(site))/(sqrt(num))
}
nt_by_site <- as.data.frame(nt_percol_mat)

quartz.options(height=6 , width = 6)
plot.new()
par(oma = c(1, 1, 1, 1))
plot.window(xlim = c(0,0.25), ylim = c(0, 0.45))
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



hist(1/(1 + percol15$per_col), breaks = 10)



ad.test((percol15$per_col))
ad.test(asin(sqrt(percol15$per_col))) # cant make this normal. need a better transformation

percol15$Lineage <- as.factor(percol15$Lineage)
percol15$Site <- as.factor(percol15$Site)

kruskal.test(percol15$per_col ~ percol15$Lineage)
kruskal.test(percol15$per_col ~ percol15$Site)


t_percol <- (percol16$percol)^(1/3)
t_percol16 <- data.frame(Cassette = percol16$cassette, Site = percol16$Site, Lineage = percol16$Lineage, Rep = percol16$Rep, percol = t_percol) 
t_percol16 <- t_percol16[-40, ]

t_percol16$Site <- as.factor(t_percol16$Site) 
t_percol16$Lineage <- as.factor(t_percol16$Lineage)

percol_nova <- aov(t_percol16$percol ~ t_percol16$Site * t_percol16$Lineage)
summary(percol_nova)

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

