\name{diagnosticPlots}
\alias{diagnosticPlots}
\alias{diagnosticPlots.list}
\alias{diagnosticPlots.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Diagnostic plots to explore seqVsInsitu results
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Accepts the result of \code{\link{seqVsInsitu}} and \code{\link{iterating_seqVsInsitu}} and produces diagnostic plots.
If the sequencing data fits to one or more terms or combinations of terms, then the scatterplot will cluster into foci.
As the number of combined terms is increased the foci merge into fewer groups. A diagonal in the scatterplot is a sign of error.
}
\usage{
diagnosticPlots(seqVsInsitu_results)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seqVsInsitu_results}{
%%     ~~Describe \code{seqVsInsitu_results} here~~
Value of \code{\link{seqVsInsitu}} or \code{\link{iterating_seqVsInsitu}}.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
None.
}
\examples{
fpath <- system.file("extdata", "vncMedianCoverage.tsv", package="cellOrigins")
vncExpression <- read.delim(file = fpath, header=FALSE, as.is=TRUE)

expression <- vncExpression$V2
names(expression) <- vncExpression$V1

result <- seqVsInsitu(expression, depth=1)
diagnosticPlots(result)

\dontrun{
oracleResponse <- iterating_seqVsInsitu(expression, 3)
diagnosticPlots(oracleResponse)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
