

#' filter a discretized SCE using getTopHVGs from scran, the produce triples (edges)
#' (cell - weight - gene)
#' @import SingleCellExperiment
#' @import scran
#' @param sce instance of SingleCellExperiment with assays logcounts
#' @param outtsv character(1) file to hold triples in tsv format
#' @param ngenes numeric(1) number of genes to retain from getTopHVGs after
#' @param n_bins numeric(1) passed to disc_matrix, defaults to 5
#' scran::modelGeneVar on logcounts
#' @examples
#' p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
#' assay(p3k) = as.matrix(assay(p3k)) # dense for now
#' p3k = scuttle::logNormCounts(p3k)
#' tf = tempfile()
#' sce_to_triples(p3k, tf, ngenes=1000)
#' head(read.delim(tf, sep="\t", h=FALSE))
#' @export
sce_to_triples = function(sce, outtsv, ngenes=5000, n_bins=5) {
stopifnot("logcounts" %in% assayNames(sce))
stats = scran::modelGeneVar(assay(sce, "logcounts", withDimnames=FALSE))
tt = scran::getTopHVGs(stats, n=ngenes)
sce = sce[tt,]
assays(sce)$disc = disc_matrix(assays(sce)$logcounts, n_bins=n_bins)
make_triples(sce, outtsv)
}

