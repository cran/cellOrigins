seqVsInsitu <- function(seq_signature, depth=2, insitu = cellOrigins::BDGP_insitu_dmel_embryo, insitu_discovery_function = discovery.log, saturate=500, prior=prior.temporal_proximity_is_good){
  #for each transcript calculate probability of insitu discovery given sequencing data
  seq_signature <- seq_signature[!is.na(seq_signature)]
  seq_signature <- insitu_discovery_function(seq_signature, saturate=saturate)
  #assimilate insitu and seq data format
  common_all_genes <-intersect(names(seq_signature), rownames(insitu))
  insitu <- insitu[match(common_all_genes, rownames(insitu)), ]
  #precompute tables for speed
  #log space to avoid numeric underflow of very small likelihood products from thousands of genes
  seq_signature_linear <- seq_signature[match(common_all_genes, names(seq_signature))]
  seq_signature <- log2(seq_signature_linear)
  seq_signature_complement <- log2(1-seq_signature_linear)

  #remove all-0 columns=terms without annotated expression insitu
  cs <- colSums(insitu)
  insitu <- insitu[ , cs>0]

  #table of all term combinations to be tested
  insitu_all_terms <- colnames(insitu)
  I <- iterpc(length(insitu_all_terms), r=depth, replace=TRUE, labels=insitu_all_terms)
  allnames_pairs <- getall(I)
  n <- dim(allnames_pairs)[1]

  #Set up results matrix
  pmoracle <- matrix(NA, nrow=n, ncol=4+depth)
  colnames(pmoracle) <- c("posterior", "prior", "likelihood.from.absence.insitu", "likelihood.from.presence.insitu", paste("t", 1:depth, sep=""))
  rownames(pmoracle) <- apply(allnames_pairs, 1, paste, collapse="+")

  #test all combinations
  oracle_position <- 1
  for (t1 in 1:n){
    insitu_signature <- 0
    #insitu term fusion
    counter <- 5
    for (term in allnames_pairs[t1,]){
      insitu_signature <- insitu_signature+insitu[,term]
      insitu_signature[insitu_signature>1] <- 1
      pmoracle[t1, counter] <- sum(insitu_signature)
      counter <- counter + 1
    }
    #Bayes
    pmoracle[oracle_position,3] <- sum(seq_signature_complement[insitu_signature==0])
    pmoracle[oracle_position,4] <- sum(seq_signature[insitu_signature>0])
    #prior probability of tested term combination
    pmoracle[oracle_position,2] <- prior(allnames_pairs[t1,], insitu_signature)
    oracle_position <- oracle_position+1
  }

  #calculate posterior
  pmoracle[,1] <- pmoracle[,2] + pmoracle[,3] + pmoracle[,4]
  #best candidate on top
  pmoracle <- pmoracle[order(pmoracle[,1], decreasing = TRUE),]

  return (pmoracle)
}
