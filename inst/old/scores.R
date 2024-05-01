#' @param colfeat character(1) element of colData of `sce` to index cells
#' @param rowfeat character(1) element of rowData of `sce` to index genes
#' @return a SummarizedExperiment
embs_to_scores = function(embout, sce, colfeat="Barcode", rowfeat="Symbol") {
  stopifnot(inherits(embout, "CG_emb_ing"))
  C = t(embout$cemb)
  FF = embout$gemb
  C %*% FF -> BIG
  EBIG = exp(BIG)
  CSUMS = apply(EBIG,1,sum)
  pmat = t(as.matrix(EBIG/CSUMS))
  rownames(pmat) = embout$G_entities_ordered # gents
  colnames(pmat) = embout$C_entities_ordered # cents
  rinds = make.names(rowData(sce)[[rowfeat]], unique=TRUE)
  if (!(all(rinds %in% rownames(pmat)))) stop("embout$G_entities_ordered is inconsistent with rowData(sce)[[rowfeat]]")
  pse = SummarizedExperiment(pmat[rinds, colData(sce)[[colfeat]]])
  mc = match.call()
  metadata(pse) = list(call = mc)
  pse
}

