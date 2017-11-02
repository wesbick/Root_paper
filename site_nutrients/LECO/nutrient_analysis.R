library(dplyr)
#importing leco file that measured C and N from 15 & 16 soils, and 16 plants
leco_dec16 <- read.table(file="~/git_repos/site_nutrients/LECO/Bickford_Dec16.csv", sep=",", header=F)
#importing leco file that measured C and N from some 15 soils
leco_mar16 <- read.table(file = "~/git_repos/site_nutrients/LECO/soils/LECO_soils_15.csv", sep = ",", header = T, stringsAsFactors = F)

head(leco_dec16)
head(leco_mar16)
colnames(leco_dec16) <- c("Project", "Sample", "Mass", "Percent_N", "Percent_C", "Time")
head(leco_dec16)

colnames(leco_mar16) <- c("Sample", "Mass", "Percent_N", "Percent_C")
head(leco_mar16)

myindex <- grep("15_Soil", leco_dec16$Project)
myindex

Soils15 <- leco_dec16[myindex, ]
head(Soils15)

#remove subsample number from sample ID
Sample <- gsub(pattern = "\\-.*", replacement = "", x = Soils15$Sample)
Soils15$Sample <- Sample
Sample <- gsub(pattern = "\\_\\d", replacement = "", x = leco_mar16$Sample)
leco_mar16$Sample <- Sample

head(Soils15)
head(leco_mar16)

leco_mar16$Sample[41] <- "PLB_Inv1"
leco_mar16$Sample[42] <- "PLB_Inv1"
leco_mar16$Sample[43] <- "PLB_Nat1"

#combine data sets

Sample <- c(Soils15$Sample, leco_mar16$Sample)
Mass <- c(Soils15$Mass, leco_mar16$Mass)
Percent_N <- c(Soils15$Percent_N, leco_mar16$Percent_N)
Percent_C <- c(Soils15$Percent_C, leco_mar16$Percent_C)

Soils15 <- data.frame(Sample = Sample, Mass = Mass, Percent_N = Percent_N, Percent_C = Percent_C, stringsAsFactors = F)

#reorder in alphabetical order
L <- order(Soils15$Sample)
Soils15 <- Soils15[L, ]


#take the mean of the subsamples run in dec 16
N <- tapply(Soils15$Percent_N, Soils15$Sample, FUN = mean)
C <- tapply(Soils15$Percent_C, Soils15$Sample, FUN = mean)
mass <- tapply(Soils15$Mass, Soils15$Sample, FUN = mean)

#re-order the samples in alphabetical order because tapply outputs order them that way
Sample <- unique(Soils15$Sample)
L <- order(Sample)
Sample <- Sample[L]

Site <- gsub(pattern = "\\_.*", replacement = "", x = Sample)
Lin_rep <- gsub(pattern = ".*_", replacement = "", x = Sample)
Lineage <- gsub(pattern = "\\d", replacement = "", x = Lin_rep)
rep <- gsub(pattern = "Nat|Inv", replacement = "", x = Lin_rep)
Sample <- gsub(pattern = "\\_", replacement = "", x = Sample)

Soils15 <- data.frame(Sample = Sample, Site, Lineage, Rep = rep, Mass = mass, Percent_N = N, Percent_C = C, stringsAsFactors = F)
head(Soils15)

sample_meta <- c(Soils15$Sample, "BLInv4", "BLInv5","BLInv6","BLNat4", "BLNat5","BLNat6")
site_meta <- c(Soils15$Site, "BL","BL","BL","BL","BL","BL")
lin_meta <- c(Soils15$Lineage, "Inv","Inv","Inv","Nat","Nat","Nat")
rep_meta <- c(Soils15$Rep, 4,5,6,4,5,6)
mass_meta <- c(Soils15$Mass, NA,NA,NA,NA,NA,NA)
N_meta <- c(Soils15$Percent_N, NA,NA,NA,NA,NA,NA)
C_meta <- c(Soils15$Percent_C, NA,NA,NA,NA,NA,NA)

soils15_wnulls <- data.frame(Sample = sample_meta, Site = site_meta, 
                             Lineage = lin_meta, Rep = rep_meta, 
                             Mass = mass_meta, Percent_N = N_meta, 
                             Percent_C = C_meta, stringsAsFactors = F)

sample_meta <- c(soils15_wnulls$Sample, "CHInv4", "CHInv5","CHInv6","CHNat4", "CHNat5","CHNat6","Rt2Nat1")
site_meta <- c(soils15_wnulls$Site, "CH","CH","CH","CH","CH","CH", "Rt2")
lin_meta <- c(soils15_wnulls$Lineage, "Inv","Inv","Inv","Nat","Nat","Nat","Nat")
rep_meta <- c(soils15_wnulls$Rep, 4,5,6,4,5,6,1)
mass_meta <- c(soils15_wnulls$Mass, NA,NA,NA,NA,NA,NA,NA)
N_meta <- c(soils15_wnulls$Percent_N, NA,NA,NA,NA,NA,NA,NA)
C_meta <- c(soils15_wnulls$Percent_C, NA,NA,NA,NA,NA,NA,NA)



soils15_wnulls <- data.frame(Sample = sample_meta, Site = site_meta, 
                             Lineage = lin_meta, Rep = rep_meta, 
                             Mass = mass_meta, Percent_N = N_meta, 
                             Percent_C = C_meta, stringsAsFactors = F)



o <- order(soils15_wnulls$Sample)
soils15_wnulls <- soils15_wnulls[o, ]

rownames(soils15_wnulls) <- soils15_wnulls$Sample

source("~/git_repos/Root_ITS/code/data_setup.R")
soils_only_in_shared <- soils15_wnulls[rownames(shared), ]
soils_only_in_shared <- soils_only_in_shared[(rownames(soils_only_in_shared)) != "NA.1", ]

meta_soils <- soils_only_in_shared

myindex <- grep("16_Soil", leco_dec16$Project)
myindex

Soils16 <- leco_dec16[myindex, ]
Soils16

Sample <- gsub(pattern = "\\-.*", replacement = "", x = Soils16$Sample)
Soils16$Sample <- Sample

#reorder in alphabetical order
L <- order(Soils16$Sample)
Soils16 <- Soils16[L, ]

index <- grep("Cond", Soils16$Sample)
index

Soils16 <- Soils16[-index, ]


#take the mean of the subsamples run in dec 16
N <- tapply(Soils16$Percent_N, Soils16$Sample, FUN = mean)
C <- tapply(Soils16$Percent_C, Soils16$Sample, FUN = mean)
mass <- tapply(Soils16$Mass, Soils16$Sample, FUN = mean)

#re-order the samples in alphabetical order because tapply outputs order them that way
Sample <- unique(Soils16$Sample)
L <- order(Sample)
Sample <- Sample[L]

Site <- gsub(pattern = "\\_.*", replacement = "", x = Sample)
Lin_rep <- gsub(pattern = ".*_", replacement = "", x = Sample)
Lineage <- gsub(pattern = "\\d", replacement = "", x = Lin_rep)
rep <- gsub(pattern = "Nat|Inv", replacement = "", x = Lin_rep)
Sample <- gsub(pattern = "\\_", replacement = "", x = Sample)

Soils16 <- data.frame(Sample = Sample, Site, Lineage, Rep = rep, Mass = mass, Percent_N = N, Percent_C = C, stringsAsFactors = F)
head(Soils16)

source("~/git_repos/Root_ms/root_col16.R")
percol_sample <- rep(0, length(percol16$cassette))
for(i in 1:length(percol16$cassette)){
  percol_sample[i] <- paste(percol16$Site[i], percol16$Lineage[i], percol16$Rep[i], sep = "")
  
}



myindex <- Soils16$Sample %in% percol_sample
myindex

Soils16_v_percol <- Soils16[myindex, ]

myindex <- percol_sample %in% Soils16$Sample
myindex

percol_v_soils <- percol16[myindex, ]


glm(Soils16_v_percol$Percent_N ~ percol16$percol)
