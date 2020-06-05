iterating_seqVsInsitu <-
function(seq_signature, upto_depth, use_topN = 50, start_depth = 2, insitu = cellOrigins::BDGP_insitu_dmel_embryo, insitu_discovery_function = discovery.log, saturate=500, prior=prior.temporal_proximity_is_good){
  message("This may take a long time.\n")
  results <- list()
  top_insitu <- insitu
  for (iteration in start_depth:upto_depth){
    if (iteration == 1){
        message("Now testing individual anatomical terms.\n")
    } else {
      message(sprintf("Now testing combinations of %d anatomical terms.\n", iteration))
    }
    pmoracle <- seqVsInsitu(seq_signature, iteration, insitu=top_insitu, insitu_discovery_function=insitu_discovery_function, saturate=saturate, prior=prior)
    results[[paste(iteration, "insitu terms combined", sep=" ")]] <- pmoracle
    names(results)[iteration==1] = "1 insitu term"
    tophits <- utils::head(rownames(pmoracle), use_topN)
    topterms <- unique(unlist(strsplit(tophits,"+", fixed=TRUE)))
    top_insitu <- insitu[,colnames(insitu) %in% topterms]
  }
  return(results)
}
