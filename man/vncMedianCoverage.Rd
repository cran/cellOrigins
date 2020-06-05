\name{vncMedianCoverage.tsv}
\alias{vncMedianCoverage}
\docType{data}
\title{
  %%   ~~ data name/kind ... ~~
    Drosophila melanogaster embryo ventral nerve cord RNASeq coverage
}
\description{
  %%  ~~ A concise (1-5 lines) description of the dataset. ~~
    Median RNAseq read coverages from 3 dissected embryonic (stage 11) fruit fly ventral nerve cords. The sequencing coverages are measured within the probing intervals of \link[=BDGP_insitu_dmel_embryo]{high-confidence BDGP insitu probes}, as described in \link{cellOrigins-package}.
}
\format{
  The format is: probe name, coverage, chromosome, probe beginn, probe end, strand.
}

\source{
  %%  ~~ reference to a publication or URL from which the data were obtained ~~
    Molnar, D 2015, 'Single embryo-single organ transcriptomics of Drosophila embryos', PhD thesis, University of Cambridge.
}
\examples{
  fpath <- system.file("extdata", "vncMedianCoverage.tsv", package="cellOrigins")
vncExpression <- read.delim(file = fpath, header=FALSE, as.is=TRUE)
}
\keyword{datasets}
