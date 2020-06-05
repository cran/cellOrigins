\name{discovery.log}
\alias{discovery.log}
\alias{discovery.linear}
\alias{discovery.identic}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Calculates discovery probability by RNA in situ hybridisation given a sequencing signal
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A set of functions with different assumptions on the probability of RNA in situ staining, given a sequencing coverage.
}
\usage{
discovery.log(seq, saturate = 60, bias = 0.01)
discovery.linear(seq, saturate = 60, bias = 0.01)
discovery.identic(seq, saturate=Inf, bias=0)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{seq}{
%%     ~~Describe \code{seq} here~~
A vector of sequencing FPKMs.
}
  \item{saturate}{
%%     ~~Describe \code{saturate} here~~
FPKM value from which on maximum discovery probability (=0.99) is assumed (i.e. almost certain true positives).
Value of 60 is default, may need adjustment to sequencing coverage.
}
  \item{bias}{
%%     ~~Describe \code{bias} here~~
Positive staining probability of 0 FPKM transcripts (i.e. false positives). Must be >0.
Default is 0.01, an empirically determined value.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\enumerate{
\item \strong{discovery.log} Uses a logarithmic saturation function for discovery probabilities. This relationship was empirically determined from sequencing and hybridisation data.
\item \strong{discovery.linear} Linear saturation function for discovery probabilities.
\item \strong{discovery.identic} Passes input through. Useful for comparing RNASeq Vs. RNASeq data. Also for cases when the discovery probability for each transcript has been already determined in some other way.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A vector of probabilities. Element names are preserved.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
\code{\link{seqVsInsitu}}
}
\examples{
plot(0:80, discovery.log(0:80),
  ylim=c(0,1.1), type="l",
  xlab="FPKM", ylab="p(discovery insitu hybridization)")

plot(0:80, discovery.linear(0:80),
  ylim=c(0,1.1), type="l",
  xlab="FPKM", ylab="p(discovery insitu hybridization)")

}

