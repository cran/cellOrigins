\name{iterating_seqVsInsitu}
\alias{iterating_seqVsInsitu}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Faster comparisons between mixed tissue-specific RNA sequencing data and high-throughput RNA in situ hybridisation
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
The same functionality as \code{\link{seqVsInsitu}} but computationally less expensive if combinations of anatomical terms are tested.

The number of term combinations to test increases rapidly in \code{\link{seqVsInsitu}}. For example with 350 anatomical terms there are 61425 combinations of 2 terms and 7207200 combinations of 3 terms. This makes the exhaustive search of \code{seqVsInsitu} costly with depth>2.

\code{iterating_seqVsInsitu} reduces the computational cost by initially testing the  combinations of only a few terms. Then in each iteration the cardinality of the combinations is increased by one, but only the top anatomical terms of the previous iteration are used to reduce the number of tested combinations.
}
\usage{
iterating_seqVsInsitu(seq_signature, upto_depth, use_topN = 50,
  start_depth = 2, insitu = cellOrigins::BDGP_insitu_dmel_embryo,
  insitu_discovery_function = discovery.log, saturate = 500,
  prior = prior.temporal_proximity_is_good)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seq_signature}{
%%     ~~Describe \code{seq_signature} here~~
A named vector containing FPKM RNAseq data. Each element name must correspond to the names used in the \code{insitu} argument. NAs are permitted.
}
  \item{upto_depth}{
%%     ~~Describe \code{upto_depth} here~~
Number of terms to combine in the final iteration.
}
  \item{use_topN}{
%%     ~~Describe \code{use_topN} here~~
How many of the top results from the previous iteration to use to find the terms for the current iteration.
}
  \item{start_depth}{
%%     ~~Describe \code{start_depth} here~~
Number of terms to combine in the first iteration. All combinations of all terms are tested at this step.
}
  \item{insitu}{
%%     ~~Describe \code{insitu} here~~
Matrix with RNA in situ hybridisation data. Rows are transcript names (queried by probes: same names as used for \code{seq_signature}) and coloumns are anatomical terms (possibly combined with developmental stages). If a probe stains in a particular tissue, the value is 1, otherwise 0.
Defaults to \code{\link{BDGP_insitu_dmel_embryo}}, a staining dataset for fruit fly embryos.
}
  \item{insitu_discovery_function}{
%%     ~~Describe \code{insitu_discovery_function} here~~
A function that converts FPKM values to the probability of discovery by RNA in situ hybridisation. Values must be ]0..1[, 0 and 1 are not permitted.
Defaults to \code{\link{discovery.log}}, an approximation of empirically determined discovery probabilities. Other available functions are \code{\link{discovery.linear}} and \code{\link{discovery.identic}}.
}
  \item{saturate}{
%%     ~~Describe \code{saturate} here~~
Will be passed on to the \code{insitu_discovery_function}. The data set dependent maximum value at which the discovery probability should saturate.
Defaults to 500 (FPKM).
}
  \item{prior}{
%%     ~~Describe \code{prior} here~~
A function that evaluates to the log2 prior probability of each anatomic term or combination of terms.
Defaults to \code{\link{prior.temporal_proximity_is_good}}, which works well with \code{\link{BDGP_insitu_dmel_embryo}}. \code{\link{prior.all_equal}} assumes equal probability of all terms.
}
}
\value{
Returns a named list that contains a matrix for each iteration like those produced by \code{\link{seqVsInsitu}}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{seqVsInsitu}}
}
\examples{
\dontrun{
fpath <- system.file("extdata", "vncMedianCoverage.tsv", package="cellOrigins")
vncExpression <- read.delim(file = fpath, header=FALSE, as.is=TRUE)

expression <- vncExpression$V2
names(expression) <- vncExpression$V1

oracleResponse <- iterating_seqVsInsitu(expression, 3)
head(oracleResponse[[1]])
head(oracleResponse[[2]])
diagnosticPlots(oracleResponse)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
