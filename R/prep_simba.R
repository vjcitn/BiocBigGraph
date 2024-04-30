#' produce simba barplot for selected gene
#' @param sce a SingleCellExperiment for which add_simba_scores has been run
#' @param colvar character(1) variable in colData(sce) to use for cell type,
#' defining bar colors
#' @param sym character(1) name of row in sce for which expression scores should
#' be visualized
#' @param maxrank numeric(1) largest rank to retain, defaults to total number of cells
#' @return ggplot instance
#' @examples
#' sce = grab_bgdemo_sce()
#' data(p3k.coarse)
#' sce$celltype = p3k.coarse
#' sce = add_simba_scores(sce)
#' simba_barplot(sce, "celltype", "NKG7") +
#'  ylab("SIMBA celltype:expression score for NKG7") + ggtitle("PBMC3K")
#' @export
simba_barplot = function(sce, colvar, sym, maxrank=ncol(sce)) {
 datf = simba_barplot_df(sce, sym, colvars=colvar)
 datf = datf[which(datf$rank <= maxrank),]
 ggplot(datf, aes(x=rank, xend=rank, y=0, yend=prob,
   colour=celltype)) + geom_segment(linewidth=.3) 
 }

#' set up a data.frame for barplot visualization for a gene
#' @param sce SingleCellExperiment with altExp element "sprobs"
#' @param genesym character(1)
#' @param colvars character() columns from colData(sce) to propagate
#' @return data.frame with elements rank, prob, and propagated columns
#' @examples
#' sce = grab_bgdemo_sce()
#' data(p3k.coarse)
#' sce$celltype = p3k.coarse
#' sce = add_simba_scores(sce)
#' gns = c("CST3", "MS4A1", "NKG7", "GAPDH")
#' alldf = lapply(gns, function(x) simba_barplot_df(sce, x, 
#'   colvars = "celltype"))
#' alldf = do.call(rbind, alldf)
#' ggplot(alldf, aes(x=rank,xend=rank,y=0, yend=prob,colour=celltype)) + geom_point() +
#'   geom_segment() + facet_wrap(~symbol, ncol=2) + 
#'     theme(strip.text=element_text(size=24), 
#'       legend.text=element_text(size=24), 
#'       axis.text=element_text(size=24)) +
#'     guides(colour = guide_legend(override.aes = 
#'        list(linetype=1, lwd=2)))
#' @export
simba_barplot_df = function(sce, genesym, colvars="celltype") {
  stopifnot("sprobs" %in% altExpNames(sce))
  stopifnot(genesym %in% rownames(altExp(sce, "sprobs")))
  stopifnot(all(colvars %in% names(colData(sce))))
  resp = as.numeric(assay(altExp(sce, "sprobs"))[genesym,])
  o = order(resp, decreasing=TRUE)
  ans = data.frame(rank=seq_len(length(o)), prob = resp[o], tmp=colData(sce)[o,colvars])
  names(ans)[-c(1,2)] = colvars
  ans$symbol = genesym
  ans
}

