

#' filter a discretized SCE using getTopHVGs from scran, then produce triples (edges)
#' (cell - weight - gene)
#' @import SingleCellExperiment
#' @import scran
#' @param sce instance of SingleCellExperiment with assays logcounts
#' @param outtsv character(1) file to hold triples in tsv format
#' @param ngenes numeric(1) number of genes to retain from getTopHVGs after
#' scran::modelGeneVar on logcounts
#' @param n_bins numeric(1) passed to disc_matrix, defaults to 5
#' @param colname character(1) passed to make_triples, defaults to "Barcode"
#' @param cuts numeric() passed to disc_matrix
#' @examples
#' p3k = TENxPBMCData::TENxPBMCData("pbmc3k")
#' assay(p3k) = as.matrix(assay(p3k)) # dense for now
#' p3k = scuttle::logNormCounts(p3k)
#' tf = tempfile()
#' sce_to_triples(p3k, tf, ngenes=1000)
#' head(read.delim(tf, sep="\t", h=FALSE))
#' @export
sce_to_triples = function(sce, outtsv, ngenes=5000, n_bins=5,
   colname="Barcode", cuts=NULL) {
stopifnot("logcounts" %in% assayNames(sce))
stats = scran::modelGeneVar(assay(sce, "logcounts", withDimnames=FALSE))
tt = scran::getTopHVGs(stats, n=ngenes)
sce = sce[tt,]
assays(sce)$disc = disc_matrix(assays(sce)$logcounts, n_bins=n_bins, cuts=cuts)
make_triples(sce, outtsv, colname=colname, assayname="disc")
}

