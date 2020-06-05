prior.temporal_proximity_is_good <-
function(term_pairs, insitu_signature){
  #prior = temporal distance of tested stages -- as coexistence increasingly unlikely with growing temporal distance.  
  stages <- as.numeric(substr(term_pairs, 1,1))
  ds <- expand.grid(stages, stages)
  prior <- -max(abs(ds[,1]-ds[,2]))
  #adjust prior to number of tested genes
  prior <- prior*length(insitu_signature)*0.01 
  
  return (prior)
}
