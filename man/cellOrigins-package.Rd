\name{cellOrigins-package}
\alias{cellOrigins-package}
\alias{cellOrigins}
\docType{package}
\title{
Finding the most likely originating tissue(s) and developmental stage(s) of RNASeq data
}
\description{
\if{html}{\figure{delphi.jpg}{Logo: the Pythia at Delphi.}}

\code{cellOrigins} compares RNASeq read coverages with in high-throughput RNA in situ hybridisation patterns for transcriptome source identification and verification. The package can identify both pure transcriptomes and mixtures of transcriptomes. Typical uses are the identification of cancer cell origins, validation of cell culture strain identities, validation of single-cell transcriptomes, and validation of identity and purity of flow-sorting and dissection sequencing products.

The comparison of quantitative RNA sequencing coverage with thresholded, qualitative staining patterns is probabilistic. First, given the sequenced transcriptome, a prediction is made how likely each sequenced transcript would lead to a positive signal in a high-throughput in situ hybridisation experiment. The probability of staining increases with the logarithm of the sequencing coverage. This relationship was empirically found through a comparison between \emph{Drosophila} embryo transcriptomes and RNA in situ staining results. Then, using Bayes's theorem all the genes in the simulated and observed hybridisation patterns are compared. The pattern (or linear combination of patterns) with the highest posterior probability is identified as the most likely source.

Batteries included: the package contains a filtered high-confidence expression pattern dataset for \emph{Drosophila melanogaster} embryos (based on BDGP insitu).

Typical use:

I GENERATE INPUT

Input is RNASeq mean FPKM (fragments per kilobase per million reads). Whole-gene FPKM may  be used (as output by e.g. cufflinks/cuffquant), however assignment difficulties at overlapping transcripts and transcript isoforms reduce prediction quality. For best results use FPKM values calculated for the targets of the in situ hybridisation probes as described below:

Step 1) Generate masking bed file -- this file is included for BDGP insitu in the extdata folder. For other species align probe sequences to the target genome using BLAT (https://genome.ucsc.edu/FAQ/FAQblat.html). Convert the best-scoring alignments to a masking bed file with \code{psl_to_bed_best_score.pl} (https://gist.github.com/davetang/7314846).
Then sort with \code{bedtools sort} (http://bedtools.readthedocs.org/).

Step 2) Get coverages. Use Bedtools with the masking bed file to extract the mean sequencing covereage from wig files in the in situ probed regions:

 \code{bedtools map -a sorted_probes.bed -b sequenced.wig -o max -c 4 >insitu_high_confidence.tsv}

Use the output tab separated values file as input for the function \code{\link{seqVsInsitu}}.

II SOURCE IDENTIFICATION

\code{\link{seqVsInsitu}} and \code{\link{iterating_seqVsInsitu}} calculate the probability for each in situ expression pattern that it is produced by the same gene expression patterns as the sequencing data. If you believe you have a mixed input, allow combined patterns from several target tissues. This is computationally expensive for more than two tissues. \code{iterating_seqVsInsitu} is faster thorugh calculating all combinations for n==2 and then using only the top tissues for n==3. The top tissues of n==3 is then are used for n==4 etc.

III INTERPRETATION

\code{seqVsInsitu} and \code{iterating_seqVsInsitu} return the terms or term combinations together with a log2 probability score for each. They also produce two diagnostic graphs. If multiple tissues contribute to the sample, the scatterplot should show a number of clusters at low n. As n increases, the clusters should merge into just two clusters at the ideal value of n. The line graph shows the log2 probability distribution.

\code{\link{discovery_probability}} if RNASeq and in situ hybridisation data from the same tissue are paired, then with increasing FPKM the probability of RNA in situ discovery should increase logarithmically. If the tissue sources do not match, no such relationship should be visible. Using this function, if the tissue combination in the argument is a match, there should by a nearly linearly increasing relationship in the log-plot, with saturation at very high FPKM values only.

}
\details{
\tabular{ll}{
Package: \tab cellOrigins\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-03-18\cr
License: \tab Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License\cr
}
}
\author{David Molnar

Maintainer: David Molnar <dmolnar100@icloud.com>
}
\references{
Molnar, D 2015, 'Single embryo-single organ transcriptomics of Drosophila embryos', PhD thesis, University of Cambridge. \cr
BDGP insitu: Tomancak, Genome Biol. 2007;8(7):R145. \cr
BDGP insitu homepage: insitu.fruitfly.org/cgi-bin/ex/insitu.pl
}

\keyword{
package
}

\examples{
\dontrun{
pmoracle <- seqVsInsitu(transcriptomeMatrix)
rownames(pmoracle)[1:3]
diagnosticPlots(pmoracle)
}
##loading the BDGP insitu probe coordinates if not
##copied directly from the package extdata folder
system.file("extdata", "BDGP_insitu_probes.bed", package = "cellOrigins")
}
