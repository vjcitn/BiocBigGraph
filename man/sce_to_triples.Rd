% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sce2trip.R
\name{sce_to_triples}
\alias{sce_to_triples}
\title{filter a discretized SCE using getTopHVGs from scran, the produce triples (edges)
(cell - weight - gene)}
\usage{
sce_to_triples(sce, outtsv, ngenes = 5000, n_bins = 5)
}
\arguments{
\item{sce}{instance of SingleCellExperiment with assays logcounts}

\item{outtsv}{character(1) file to hold triples in tsv format}

\item{ngenes}{numeric(1) number of genes to retain from getTopHVGs after}

\item{n_bins}{numeric(1) passed to disc_matrix, defaults to 5
scran::modelGeneVar on logcounts}
}
\description{
filter a discretized SCE using getTopHVGs from scran, the produce triples (edges)
(cell - weight - gene)
}
\examples{
p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
assay(p3k) = as.matrix(assay(p3k)) # dense for now
p3k = scuttle::logNormCounts(p3k)
tf = tempfile()
sce_to_triples(p3k, tf, ngenes=1000)
head(read.delim(tf, sep="\t", h=FALSE))
}
