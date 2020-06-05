discovery_probability <- function(seq_signature, terms, cut.points, insitu=cellOrigins::BDGP_insitu_dmel_embryo){
  #assimilate insitu and seq data format
  seq_signature <- seq_signature[!is.na(seq_signature)]
  common_all_genes <-intersect(names(seq_signature), rownames(insitu))
  insitu <- insitu[match(common_all_genes, rownames(insitu)), ]
  seq_signature <- seq_signature[match(common_all_genes, names(seq_signature))]

  #insitu term fusion
  insitu_signature <- 0
  for (term in terms){
    insitu_signature <- insitu_signature+insitu[,term]
  }
  insitu_signature[insitu_signature>1] <- 1

  #results go in here
  p <- matrix(NA, nrow=length(cut.points)-1, ncol=3)
  colnames(p) <- c("p", "seq_in_bin_and_discovered", "all_seq_in_bin")
  #binning
  for (bin in 1:length(cut.points)-1) {
      total_in_bin <- seq_signature>=cut.points[bin] & seq_signature<cut.points[bin+1]
      in_bin_and_discovered <- sum(insitu_signature[total_in_bin])
      p[bin,2] <- in_bin_and_discovered
      p[bin,3] <- sum(total_in_bin)
      p[bin,1] <- in_bin_and_discovered/sum(total_in_bin)
  }
  return(p)
}
