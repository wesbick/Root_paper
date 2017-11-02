library("dplyr")

rep_fasta <- scan(file = "combined.trim.good.unique.good.filter.unique.precluster.pick.pick.opti_mcc.0.03.rep.fasta", what = "character", sep = "\n")
even <- rep_fasta[is.integer(row(rep_fasta)/2)]
rep_fasta_nodash <- gsub("\\-","", rep_fasta)
rep_fasta_nodot <- gsub("\\.", "", rep_fasta_nodash)
rep_fasta_nospace <- gsub("\\s", "", rep_fasta_nodot)
rep_fasta_nopipe <- gsub("(\\|\\d*\\|).*", "\\1", rep_fasta_nospace)

write(rep_fasta_nopipe, file = "rep_OTUs.fasta")

rep_fasta_top <- rep_fasta_nodash[1:10]
write(rep_fasta_top, file = "rep_OTUs_5.fasta")


tax <- scan(file = "~/git_repos/Root_bacterial/data/otu_taxonomy.txt", what = "character", sep = "\t")
head(tax)
