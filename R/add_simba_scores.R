#' add the SIMBA gene:cell probabilities to an SCE with embeddings available
#' @param sce a SingleCellExperiment instance with `pbg_cell_emb` in altExps and `g_emb` in metadata
#' @param aeind numeric(1) defaults to 1, picking from multiple altExps for the pbg cell embedding
#' @examples
#' sce = grab_bgdemo_sce()
#' sce2 = add_simba_scores(sce)
#' data(p3k.coarse)
#' nkg7 = as.numeric(assay(altExp(sce2, "sprobs")["NKG7",]))
#' lm(nkg7~factor(p3k.coarse))
#' @export
add_simba_scores = function(sce, aeind=1) {
   stopifnot("pbg_cell_emb" %in% names(altExps(sce, aeind))) # need to iterate over altExps?
   stopifnot("g_emb" %in% names(metadata(sce)))
   simba_scores = function(cemb, gemb) {
     C = t(cemb)
     FF = gemb
     C %*% FF -> BIG
     EBIG = exp(BIG)
     CSUMS = apply(EBIG,1,sum)
     pmat = t(as.matrix(EBIG/CSUMS))
   }
   
   mat = simba_scores(assay(altExp(sce, "pbg_cell_emb")),
       metadata(sce)$g_emb)
   rownames(mat) = metadata(sce)$g_ent_ord
   altExp(sce, "sprobs") = SummarizedExperiment(mat)
   sce
}
   
   
