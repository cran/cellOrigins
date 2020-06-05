diagnosticPlots.list <-
function(seqVsInsitu_results){
  #for lists coming out of iterating_seqVsInsitu
  seqVsInsitu_results <- lapply(seqVsInsitu_results, na.exclude)
  op <- par(mfrow=c(length(seqVsInsitu_results),2))
  on.exit(par(op))
  items <- names(seqVsInsitu_results)
  for (itemName in items){
    item <- seqVsInsitu_results[[itemName]]
    if (class(item)=="matrix"){
      .plotDiagnosis(item, itemName)
    }
  }
}
