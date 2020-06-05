diagnosticPlots.matrix <-
function(seqVsInsitu_results){
  #for matrices coming out of seqVsInsitu
  op <- par(mfrow=c(1,2))
  on.exit(par(op))
  .plotDiagnosis(seqVsInsitu_results, "")
}
