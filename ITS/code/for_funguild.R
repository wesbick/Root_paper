source('taxonomy_analysis.R')
taxonomy <- read.table(file = "data/Bickford.cons.taxonomy", header=T, stringsAsFactors = F)

funtax <- taxonomy[grep(pattern = "Fungi", x=taxonomy$Taxonomy), ]
taxnocon <- gsub(pattern = "\\(\\d*\\)", replacement = "", x=funtax$Taxonomy)

tax_for_funguild <- data.frame(OTU = funtax$OTU, Size = funtax$Size, Taxonomy = taxnocon)

library(dplyr)
library(tidyr)

#for all fungi
OTU <- colnames(shared)
trans_shared <- data.frame(OTU, t(shared))
for_FUNGuild <- inner_join(trans_shared, tax_for_funguild, by = c("OTU" = "OTU"))

write.table(for_FUNGuild, "data/for_funguild.txt", sep = "\t", quote = F, row.names = F)

##import from funguild output
guilds <- read.table("data/for_funguild.guilds_matched.txt", header = T, sep = "\t", stringsAsFactors = F)

#make metadata file for guilds
guild_data <- guilds[ , -c(2:48)]
guild_data$Trophic.Mode <- as.factor(guild_data$Trophic.Mode)
guild_data$Guild <- as.factor(guild_data$Guild)
summary(guild_data)

matched_guild_shared <- guilds[ , c(1:48)]
row.names(matched_guild_shared) <- matched_guild_shared$OTU
matched_guild_shared <- matched_guild_shared[ , -1]

### I want rel abundance of each guild
trans_matched_guild_shared <- data.frame(t(matched_guild_shared))

# how do I combine the number of sequences from each of the OTUs

# in the below, 1 is for rows and 2 is for columns
matched_guild_count <- apply(trans_matched_guild_shared, 1, sum)

# how do I repeat this for all phyla?
#make a loop

#need to know the unique trophic modes 
trophic_names <- unique(guild_data$Trophic.Mode)
# need somwhere to put data - a new shared file
n_modes <- length(trophic_names)
n_samples <- nrow(trans_matched_guild_shared)
trophic_shared <- data.frame(matrix(0, nrow = n_samples, ncol = n_modes))
rownames(trophic_shared) <- rownames(trans_matched_guild_shared)
colnames(trophic_shared) <- trophic_names

#my loop
for(i in trophic_names){
  trophic_otus <- guild_data[guild_data$Trophic.Mode == i, "OTU"]
  guild_shared <- shared[ , trophic_otus]
  if(length(trophic_otus) > 1){
    trophic_count <- apply(guild_shared, 1, sum)
  } else {
    trophic_count <- guild_shared
  }
  trophic_shared[ , i] <- trophic_count
  
}

# 3. calculate the relative abundance
n_seqs <- apply(trophic_shared, 1, sum)
trophic_rel_abund <- trophic_shared / n_seqs

tot_trophic_rel_abund <- numeric()
for(h in colnames(trophic_rel_abund)){
  y <- mean(trophic_rel_abund[ ,h])
  tot_trophic_rel_abund[h] <- y
}
sum(tot_trophic_rel_abund)
tot_trophic_rel_abund

L <- sort.list(tot_trophic_rel_abund, decreasing = T)
tot_trophic_rel_abund <- tot_trophic_rel_abund[L]

barplot(height = tot_trophic_rel_abund, beside=T)

# make an OTU table by guild

#need to know the unique trophic modes 
guild_names <- unique(guild_data$Guild)
# need somwhere to put data - a new shared file
n_guilds <- length(guild_names)
n_samples <- nrow(trans_matched_guild_shared)
all_guild_shared <- data.frame(matrix(0, nrow = n_samples, ncol = n_guilds))
rownames(all_guild_shared) <- rownames(trans_matched_guild_shared)
colnames(all_guild_shared) <- guild_names

#my loop
for(i in guild_names){
  guild_otus <- guild_data[guild_data$Guild == i, "OTU"]
  new_shared <- shared[ , guild_otus]
  if(length(guild_otus) > 1){
    guild_count <- apply(new_shared, 1, sum)
  } else {
    guild_count <- new_shared
  }
  all_guild_shared[ , i] <- guild_count
  
}

# 3. calculate the relative abundance
n_seqs <- apply(all_guild_shared, 1, sum)
guild_rel_abund <- all_guild_shared / n_seqs

tot_guild_rel_abund <- numeric()
for(h in colnames(guild_rel_abund)){
  y <- mean(guild_rel_abund[ ,h])
  tot_guild_rel_abund[h] <- y
}
sum(tot_guild_rel_abund)
tot_guild_rel_abund

L <- sort.list(tot_guild_rel_abund, decreasing = T)
tot_guild_rel_abund <- tot_guild_rel_abund[L]

barplot(height = tot_guild_rel_abund, beside=T)

# make an OTU table by growth form

#need to know the unique trophic modes 
growth_forms <- unique(guild_data$Growth.Morphology)
# need somwhere to put data - a new shared file
n_forms <- length(growth_forms)
n_samples <- nrow(trans_matched_guild_shared)
growth_form_shared <- data.frame(matrix(0, nrow = n_samples, ncol = n_forms))
rownames(growth_form_shared) <- rownames(trans_matched_guild_shared)
colnames(growth_form_shared) <- growth_forms

#my loop
for(i in growth_forms){
  gform_otus <- guild_data[guild_data$Growth.Morphology == i, "OTU"]
  new_shared <- shared[ , gform_otus]
  if(length(gform_otus) > 1){
    gform_count <- apply(new_shared, 1, sum)
  } else {
    gform_count <- new_shared
  }
  growth_form_shared[ , i] <- gform_count
  
}
