source('data_setup.R')
mean(metadata$n_seqs)

rarefy <- read.table(file = "data/Bickford.groups.rarefaction", header=T, stringsAsFactors=F)

ave_columns <- grep(pattern = "X1", colnames(rarefy))
ave_rarefy <- rarefy[ , ave_columns]

numsampled <- rarefy$numsampled

sample_IDs <- gsub(pattern = "X1.", replacement = "", x=colnames(ave_rarefy))
colnames(ave_rarefy) <- sample_IDs

clrs <- c(BL = "grey", CB = "blue", CH = "red", CM = "coral", CR = "black", PLB = "orange", Rt2 = "green", SB = "purple")
plot(NA, type = "l",
     xlab = "Number of Sequences Sampled",
     ylab = "Number of OTUs Observed",
     ylim = c(0,150),
     xlim = c(0,3000)
     )
for(i in sample_IDs){
      points(ave_rarefy[ ,i] ~ numsampled, type= "l",
             col=clrs[metadata[metadata$sample==i, "Site"]],
             lwd = 2, lty = 1
       )
}
abline(v=200)

