\name{discovery_probability}
\alias{discovery_probability}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
In situ discovery probability as a function of FPKM
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Groups transcripts by expression strength and calculates for each such group the percentage of genes that gave a positive staining signal in the in situ hybridisation.

If the sequenced material matches the in situ hybridisation tissue, then weakly expressed genes in the sequenced material should be rearely in the in situ staining set of genes. Strongly expressed genes should correspondingly often also stain during hybridisation. Overall, if the match is not spurious, there should be a logarithmic dose-response relationship between sequencing read coverage and staining probability. In a plot of discovery probability against log(coverage) this shows as an approximately straight line (see example).
}
\usage{
discovery_probability(seq_signature, terms, cut.points,
    insitu=cellOrigins::BDGP_insitu_dmel_embryo)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seq_signature}{
%%     ~~Describe \code{seq_signature} here~~
A named vector containing FPKM RNAseq data. Each element name must correspond to the names used in the \code{insitu} argument. NAs are permitted.
}
  \item{terms}{
%%     ~~Describe \code{depth} here~~
A vector of anatomical terms which together are assumed to be the origin of the RNAseq data.
}
  \item{cut.points}{
%%     ~~Describe \code{insitu} here~~
A vector of cut points for grouping of values. E.g. 0:3 denotes the bins 0<=x<1, 1<=x<2, 2<=x<3.
}
  \item{insitu}{
%%     ~~Describe \code{insitu} here~~
Matrix with in situ hybridisation data. Rows are transcript names (same names as used for \code{seq_signature}) and coloumns are anatomical terms (possibly combined with developmental stages). 1 denotes staining of a particular transcript in a particular tissue, 0 denotes no staining.
Defaults to \code{\link{BDGP_insitu_dmel_embryo}}, a staining dataset for \emph{Drosophila melanogaster} embryos.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A matrix with a row for each bin and three coloumns. The first coloumn is the probability of discovery, the second the number of transcripts in the expression bin that were discovered by in situ hybridisation. The third coloumn is the total number of transcripts in the bin.}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{iterating_seqVsInsitu}}, \code{\link{BDGP_insitu_dmel_embryo}}, \code{\link{discovery.log}}, \code{\link{discovery.linear}}, \code{\link{discovery.identic}}, \code{\link{prior.temporal_proximity_is_good}}, \code{\link{prior.all_equal}}, \code{\link{diagnosticPlots}}.
}
\examples{
fpath <- system.file("extdata", "vncMedianCoverage.tsv", package="cellOrigins")
vncExpression <- read.delim(file = fpath, header=FALSE, as.is=TRUE)

expression <- vncExpression$V2
names(expression) <- vncExpression$V1

p <- discovery_probability(expression,
  "6|ventral nerve cord", c(0, 2^(0:10)))

plot(x=-1:9, y=p[,1], type="l",
  xlab="log2(FPKM)", ylab="p(discovery in situ)")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
