
tax_table <- function(table, database){
  taxonomy <- read.table(file = table, header=T, stringsAsFactors = F)
  taxnocon <- gsub(pattern = "\\(\\d*\\)", replacement = "", x=taxonomy$Taxonomy)
  
   if(database == "UNITE"){
   
    k_re <- "[k_]+(\\w+);.*"
    kingdom <- gsub(pattern = k_re, "\\1", taxnocon) 
    p_re <- "[k_]+\\w+;[p_]*(\\w+);.*"
    phylum <- gsub(pattern = p_re, "\\1", taxnocon)  
    c_re <- "[k_]+\\w+;[p_]*\\w+;[c_]*(\\w+);.*"
    class <- gsub(pattern = c_re, "\\1", taxnocon) 
    o_re <- "[k_]+\\w+;[p_]*\\w+;[c_]*\\w+;[o_]*(\\w+);.*"
    order <- gsub(pattern = o_re, "\\1", taxnocon)
    f_re <- "[k_]+\\w+;[p_]*\\w+;[c_]*\\w+;[o_]*\\w+;[f_]*(\\w+);.*"
    family <- gsub(pattern = f_re, "\\1", taxnocon)
    g_re <- "[k_]+\\w+;[p_]*\\w+;[c_]*\\w+;[o_]*\\w+;[f_]*\\w+;[g_]*(\\w+);.*"
    genus <- gsub(pattern = g_re, "\\1", taxnocon)
    sp_re <- "[k_]+\\w+;[p_]*\\w+;[c_]*\\w+;[o_]*\\w+;[f_]*\\w+;[g_]*\\w+;[s_]*(\\w+-*\\w*);.*"
    sp <- gsub(pattern = sp_re, "\\1", taxnocon)
    s_re <- "(\\w+)_(\\w+-*\\w*)"
    species <- gsub(pattern = s_re, "\\1 \\2", sp)
    
    otu_table <- data.frame(otu = taxonomy$OTU, kingdom = kingdom, phylum = phylum, class = class, order = order, family = family, genus = genus, species = species, stringsAsFactors = F)
  }else if(database == "SILVA"){
    k_re <- "(\\w+);.*"
    kingdom <- gsub(pattern = k_re, "\\1", taxnocon) 
    p_re <- "\\w+;\"*(\\w+)\"*(-*\\w*)\"*;.*"
    phylum <- gsub(pattern = p_re, "\\1\\2", taxnocon)  
    c_re <- "\\w+;\"*\\w+\"*-*\\w*\"*;\"*(\\w+)\"*(-*\\w*)\"*;.*"
    class <- gsub(pattern = c_re, "\\1\\2", taxnocon) 
    o_re <- "\\w+;\"*\\w+\"*-*\\w*\"*;\"*\\w+\"*-*\\w*\"*;\"*(\\w+)\"*(-*\\w*)\"*;.*"
    order <- gsub(pattern = o_re, "\\1\\2", taxnocon)
    f_re <- "\\w+;\"*\\w+\"*-*\\w*\"*;\"*\\w+\"*-*\\w*\"*;\"*\\w+\"*-*\\w*\"*;\"*(\\w+)\"*(-*\\w*)\"*;.*"
    family <- gsub(pattern = f_re, "\\1\\2", taxnocon)
    g_re <- "\\w+;\"*\\w+\"*-*\\w*\"*;\"*\\w+\"*-*\\w*\"*;\"*\\w+\"*-*\\w*\"*;\"*\\w+\"*-*\\w*\"*;\"*(\\w+)\"*(-*\\w*)\"*.*"
    genus <- gsub(pattern = g_re, "\\1\\2", taxnocon)
    
    
    otu_table <- data.frame(otu = taxonomy$OTU, kingdom = kingdom, phylum = phylum, class = class, order = order, family = family, genus = genus, stringsAsFactors = F)
    
  }else if(database == "GG"){
    taxnocon <- gsub(pattern = "\\(\\d*\\)", replacement = "", x=taxonomy$Taxonomy)
    
    k_re <- "[k_]+(\\w+);.*"
    kingdom <- gsub(pattern = k_re, "\\1", taxnocon) 
    p_re <- "[k_]+\\w+;[kp_]*\\[?(\\w+)\\]?;.*"
    phylum <- gsub(pattern = p_re, "\\1", taxnocon)  
    c_re <- "[k_]+\\w+;[kp_]*\\w+;[kpc_]*\\[?(\\w+)\\]?;.*"
    class <- gsub(pattern = c_re, "\\1", taxnocon) 
    o_re <- "[k_]+\\w+;[kp_]*\\w+;[kpc_]*\\w+;[kpco_]*\\[?(\\w+)\\]?;.*"
    order <- gsub(pattern = o_re, "\\1", taxnocon)
    f_re <- "[k_]+\\w+;[kp_]*\\w+;[kpc_]*\\w+;[kpco_]*\\w+;[kpcof_]*\\[?(\\w+)\\]?;.*"
    family <- gsub(pattern = f_re, "\\1", taxnocon)
    g_re <- "[k_]+\\w+;[kp_]*\\w+;[kpc_]*\\w+;[kpco_]*\\w+;[kpcof_]*\\w+;[kpcofg_]*\\[?(\\w+)\\]?;.*"
    genus <- gsub(pattern = g_re, "\\1", taxnocon)
    sp_re <- "[k_]+\\w+;[kp_]*\\w+;[kpc_]*\\w+;[kpco_]*\\w+;[kpcof_]*\\w+;[kpcofg_]*\\w+;[kpcofgs_]*(\\w+-*\\w*);.*"
    sp <- gsub(pattern = sp_re, "\\1", taxnocon)
    s_re <- "(\\w+)_(\\w+-*\\w*)"
    species <- gsub(pattern = s_re, "\\2", sp)
    
    otu_table <- data.frame(otu = taxonomy$OTU, kingdom = kingdom, phylum = phylum, class = class, order = order, family = family, genus = genus, species = species, stringsAsFactors = F)
    
  }
  num_otus <- length(otu_table$otu)
  rel_kingdom <- (table(otu_table$kingdom)/num_otus)*100
  cat("Percentage of OTUs from each Kingdom:\n")
  print(rel_kingdom)
  
  rel_phyla <- (table(otu_table$phylum[otu_table$kingdom == names(rel_kingdom[rel_kingdom == max(rel_kingdom)])])/num_otus)*100
  rel_phyla <- rel_phyla[order(rel_phyla, decreasing = T)]
  cat("Percentage of OTUs from each represented phylum in", names(rel_kingdom[rel_kingdom == max(rel_kingdom)]),":\n")
  print(rel_phyla)
  return(otu_table)
  
}
