discovery.linear <-
function(seq, saturate=60, bias = 0.01){
  corr <- saturate/(0.99-bias)
  seq[seq<1] <- 1
  seq <- seq/corr+bias
  seq[seq>0.99] <- 0.99
  seq[seq<0.01] <- 0.01
  return (seq)
}
