tissue_15 <- read.table("~/git_repos/site_nutrients/LECO/tissue/leaf/leco_7_28_withcodes.csv", header = F, sep = ",", stringsAsFactors = F)
tissue_15 <- tissue_15[ , -c(10:13)]
tissue_15 <- tissue_15[ , -c(3,4,6)]

colnames(tissue_15) <- c("Project", "Code", "Sample", "Mass", "Percent_N", "Percent_C")
tissue_15 <- tissue_15[tissue_15$Project == "Bickford_2016", ]
tissue_15 <- tissue_15[is.finite(tissue_15$Percent_N), ]

N <- tapply(tissue_15$Percent_N, tissue_15$Sample, FUN = mean)
C <- tapply(tissue_15$Percent_C, tissue_15$Sample, FUN = mean)
mass <- tapply(tissue_15$Mass, tissue_15$Sample, FUN = mean)

Sample <- unique(tissue_15$Sample)
L <- order(Sample)
Sample <- Sample[L]

Site <- gsub(pattern = "Nat.|Inv.", replacement = "", x = Sample)
Lin_rep <- gsub(pattern = "BL|CB|CH|CM|CR|PLB|Rt2|SB", replacement = "", x = Sample)
Lineage <- gsub(pattern = "\\d", replacement = "", x = Lin_rep)
rep <- gsub(pattern = "Nat|Inv", replacement = "", x = Lin_rep)

tissue_15 <- data.frame(Sample = Sample, Site, Lineage, Rep = rep, Mass = mass, Percent_N = N, Percent_C = C, stringsAsFactors = F)
head(tissue_15)

sample_meta <- c(tissue_15$Sample, "BLInv2")
site_meta <- c(tissue_15$Site, "BL")
lin_meta <- c(tissue_15$Lineage, "Inv")
rep_meta <- c(tissue_15$Rep, 2)
mass_meta <- c(tissue_15$Mass, NA)
N_meta <- c(tissue_15$Percent_N, NA)
C_meta <- c(tissue_15$Percent_C, NA)

tissue15_wnulls <- data.frame(Sample = sample_meta, Site = site_meta, 
                             Lineage = lin_meta, Rep = rep_meta, 
                             Mass = mass_meta, Percent_N = N_meta, 
                             Percent_C = C_meta, stringsAsFactors = F)

o <- order(tissue15_wnulls$Sample)
tissue15_wnulls <- tissue15_wnulls[o, ]

rownames(tissue15_wnulls) <- tissue15_wnulls$Sample

source("~/git_repos/Root_ITS/data_setup.R")
source("~/git_repos/Root_ITS/community analysis.R")
tissue_only_in_shared <- tissue15_wnulls[rownames(shared), ]

meta_tissue <- tissue_only_in_shared

