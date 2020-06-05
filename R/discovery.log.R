discovery.log <-
function(seq, saturate=60, bias=0.01){
  seq <- log(seq, base=60)
  seq[seq>0.99] <- 0.99
  seq[seq<0.01] <- 0.01
  corr <- 0.99/(0.99-bias)
  seq <- seq/corr+bias
  return (seq)
}
