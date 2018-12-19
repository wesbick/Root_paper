# Root endophytes and invasiveness: no difference between native and non-native <i>Phragmites</i> in the Great Lakes Region
Wesley A. Bickford, Deborah E. Goldberg, Kurt P. Kowalski, and Donald R. Zak

Citation: Bickford, W. A., D. E. Goldberg, K. P. Kowalski, and D. R. Zak. 2018. Root endophytes and invasiveness: no difference between native and non-native <i>Phragmites</i> in the Great Lakes Region. Ecosphere 9(12):e02526. 10.1002/ecs2.2526

https://doi.org/10.1002/ecs2.2526

This repository is associated with the manuscript "Root endophytes and invasiveness: no difference between native and non-native Phragmites in the Great Lakes Region" written by Wesley A. Bickford, Deborah E. Goldberg, Kurt P. Kowalski, and Donald R. Zak.

Information about this repository

#Sequence Data

Sequencing was done at the University of Michigan Sequencing Core using a PacBio-RS II system. FASTQ files were submitted to the NCBI Sequence Read Archive under BioProject PRJNA490140 with SRA accession number SRP160913.

Access original fastq files here: https://www.ncbi.nlm.nih.gov/sra/?term=SRP160913

Directories:

```
|- README
|- Full_Analyses.Rmd    # Rmarkdown document for all statistics run for the manuscript and Appendix A
|- MOTHUR/              # All files necessary to run MOTHUR including the batch file, oligos files and taxa related files
| |- Fungi/
| |- Bacteria/
| |- Oomycetes/
| | |- BLAST/           # Batch file for running blastn analysis with custom oomycete database
|- bacteria/             
| |- code/              # any programmatic code
| |- data/              # raw and primary data, are not changed once created
| |- Root_Bacteria.Rmd  # executable Rmarkdown for the bacterial portion of the study
|- ITS/
| |- code/              # any programmatic code
| |- data/              # raw and primary data, are not changed once created
| |- Root_Fungi.Rmd     # executable Rmarkdown for the fungal portion of the study
|- Oom/
| |- code/              # any programmatic code
| |- data/              # raw and primary data, are not changed once created
| |- Root_Oom.Rmd       # executable Rmarkdown for the oomycete portion of the study
|- Exploratory/         # exploratory anayses not included in manuscript
```
