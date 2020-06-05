\name{prior.temporal_proximity_is_good}
\alias{prior.temporal_proximity_is_good}
\alias{prior.all_equal}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Assign a prior probability to a combination of anatomical terms
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Accepts one or more anatomical terms and assigns to them a prior probability in the Bayesian sense.
\code{prior.all_equal} assumes all terms and combinations to be equally probable.
\code{prior.temporal_proximity_is_good} is meant mainly for use with \code{\link{BDGP_insitu_dmel_embryo}} if working with single or staged embryos.
With this function the prior probability increases if the developmental stages in the tested terms are close together.
The magnitude of the prior is scaled to the number of tested genes.
}
\usage{
prior.temporal_proximity_is_good(term_pairs, insitu_signature)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{term_pairs}{
%%     ~~Describe \code{term_pairs} here~~
A vector with anatomical terms that are tested in combination.
}
  \item{insitu_signature}{
%%     ~~Describe \code{insitu_signature} here~~
The RNA in situ hybridisation data set as produced by fusion of the expression patterns in \code{term_pairs}, and as it will be used for calculating the posterior probability in \code{\link{seqVsInsitu}}.
}
}


