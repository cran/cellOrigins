.plotDiagnosis <-
function(seqVsInsitu_results, title){
  plot(x=seqVsInsitu_results[,3], y=seqVsInsitu_results[,4], xlab="false positive", ylab="false negative", main=title)
  plot(seqVsInsitu_results[,1], type="l", xlab="ordered candidates", ylab="posterior probability", main=title)    
}
