taxonomy <- read.table(file = "data/Bickford.cons.taxonomy", header=T, stringsAsFactors = F)


funtax <- taxonomy$Taxonomy[grep(pattern = "Fungi", x=taxonomy$Taxonomy)]
plant_otu <- taxonomy[grep(pattern = "Plant", x=taxonomy$Taxonomy), "OTU"]
protist_otu <- taxonomy[grep(pattern = "Protis", x=taxonomy$Taxonomy), "OTU"]

# this separates by phylum
taxnocon <- gsub(pattern = "\\(\\d*\\)", replacement = "", x=taxonomy$Taxonomy)
nofungi <- gsub(pattern = "k__Fungi;", replacement = "", x=taxnocon)
nobegin <- gsub(pattern = "p__", replacement = "", x=nofungi)
phylum <- gsub(pattern = ";.*", replacement = "", x=nobegin)

# by genus
uptogenus <- gsub(pattern = "k_.*g__", replacement = "", x=taxnocon)
addunclass <- gsub(pattern = "k_.*;unclassified.*", replacement = "unclassified", x=uptogenus)
moreunclass <- gsub(pattern = "unclassified_.*", replacement = "unclassified", x=addunclass)
genus <- gsub(pattern = ";.*", replacement = "", x=moreunclass)
  
# i want a column with otu, phylum, and genus

otu_phylum_genus <- data.frame(otu = taxonomy$OTU, phylum = phylum, genus = genus, stringsAsFactors = F)

shared <- read.table(file= "data/Bickford.subsample.shared", header = T, stringsAsFactors = F, row.names = 2)
shared <- shared[ ,-c(1,2)]


L <- otu_phylum_genus$otu %in% colnames(shared)
otu_phylum_genus <- otu_phylum_genus[L, ]
stopifnot(nrow(otu_phylum_genus)==ncol(shared))

# how do I get the otus from the Ascomycota?

asco_otus <- otu_phylum_genus[otu_phylum_genus$phylum == "Ascomycota", "otu"]
asco_shared <- shared[ , asco_otus]

# how do I combine the number of sequences from each of the Acomycota OTUs

    # in the below, 1 is for rows and 2 is for columns
ascomycota_count <- apply(asco_shared, 1, sum)

# how do I repeat this for all phyla?
#make a loop

#need to know the phylum names - phylum_names / phylum_name
phylum_names <- unique(otu_phylum_genus$phylum)
# need somwhere to put data - allphylum_shared
n_phyla <- length(phylum_names)
n_samples <- nrow(shared)
allphylum_shared <- data.frame(matrix(0, nrow = n_samples, ncol = n_phyla))
rownames(allphylum_shared) <- rownames(shared)
colnames(allphylum_shared) <- phylum_names

#my loop
for(i in phylum_names){
  phylum_otus <- otu_phylum_genus[otu_phylum_genus$phylum == i, "otu"]
  phylum_shared <- shared[ , phylum_otus]
  if(length(phylum_otus) > 1){
    phylum_count <- apply(phylum_shared, 1, sum)
    } else {
      phylum_count <- phylum_shared
    }
  allphylum_shared[ , i] <- phylum_count
  
}

# 3. calculate the relative abundance
n_seqs <- apply(allphylum_shared, 1, sum)
phylum_rel_abund <- allphylum_shared / n_seqs[1]

tot_rel_abund <- numeric()
for(h in colnames(phylum_rel_abund)){
  y <- mean(phylum_rel_abund[ ,h])
  tot_rel_abund[h] <- y
}
sum(tot_rel_abund)

barplot(height = tot_rel_abund, beside=T)

source('data_setup.R')


### I want rel abundance of each genus within ascomycota
otu_asco_genus <- otu_phylum_genus[otu_phylum_genus$phylum == "Ascomycota", ]

asco_otus <- otu_phylum_genus[otu_phylum_genus$phylum == "Ascomycota", "otu"]
asco_shared <- shared[ , asco_otus]

# how do I combine the number of sequences from each of the Acomycota OTUs

# in the below, 1 is for rows and 2 is for columns
ascomycota_count <- apply(asco_shared, 1, sum)

# how do I repeat this for all phyla?
#make a loop

#need to know the genus names within ascomycota
genus_names <- unique(otu_asco_genus$genus)
# need somwhere to put data - asco_genera_shared
n_genera <- length(genus_names)
n_samples <- nrow(asco_shared)
asco_genera_shared <- data.frame(matrix(0, nrow = n_samples, ncol = n_genera))
rownames(asco_genera_shared) <- rownames(asco_shared)
colnames(asco_genera_shared) <- genus_names

#my loop
for(i in genus_names){
  genus_otus <- otu_asco_genus[otu_asco_genus$genus == i, "otu"]
  genus_shared <- shared[ , genus_otus]
  if(length(genus_otus) > 1){
    genus_count <- apply(genus_shared, 1, sum)
  } else {
    genus_count <- genus_shared
  }
  asco_genera_shared[ , i] <- genus_count
  
}


# 3. calculate the relative abundance
n_seqs <- apply(asco_genera_shared, 1, sum)
genus_rel_abund <- asco_genera_shared / n_seqs

tot_genus_rel_abund <- numeric()
for(h in colnames(genus_rel_abund)){
  y <- mean(genus_rel_abund[ ,h])
  tot_genus_rel_abund[h] <- y
}
sum(tot_genus_rel_abund)
tot_genus_rel_abund

L <- sort.list(tot_genus_rel_abund, decreasing = T)
tot_genus_rel_abund <- tot_genus_rel_abund[L]

barplot(height = tot_genus_rel_abund, beside=T)

## take out unclassified
genus_names <- unique(otu_asco_genus$genus)
cl_genus_names <- genus_names[genus_names != "unclassified"]
# need somwhere to put data - asco_genera_shared
n_genera <- length(cl_genus_names)
n_samples <- nrow(asco_shared)
asco_cl_genera_shared <- data.frame(matrix(0, nrow = n_samples, ncol = n_genera))
rownames(asco_cl_genera_shared) <- rownames(asco_shared)
colnames(asco_cl_genera_shared) <- cl_genus_names

#my loop
for(i in cl_genus_names){
  genus_otus <- otu_asco_genus[otu_asco_genus$genus == i, "otu"]
  genus_shared <- shared[ , genus_otus]
  if(length(genus_otus) > 1){
    genus_count <- apply(genus_shared, 1, sum)
  } else {
    genus_count <- genus_shared
  }
  asco_cl_genera_shared[ , i] <- genus_count
  
}

# 3. calculate the relative abundance
n_seqs <- apply(asco_cl_genera_shared, 1, sum)
cl_genus_rel_abund <- asco_cl_genera_shared / n_seqs

tot_cl_genus_rel_abund <- numeric()
for(h in colnames(cl_genus_rel_abund)){
  y <- mean(cl_genus_rel_abund[ ,h])
  tot_cl_genus_rel_abund[h] <- y
}
sum(tot_cl_genus_rel_abund)
tot_cl_genus_rel_abund

L <- sort.list(tot_cl_genus_rel_abund, decreasing = T)
tot_cl_genus_rel_abund <- tot_cl_genus_rel_abund[L]

top_genera_rel_abund <- tot_cl_genus_rel_abund[tot_cl_genus_rel_abund >= 0.01]

barplot(height = top_genera_rel_abund, beside=T)
