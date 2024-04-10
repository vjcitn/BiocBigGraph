#' work with the output of CG_embed_sce
#' @param outemb list derived from configuration schema produced
#' in the embedding function; for demo4, use `make_demo_emb`
#' @examples
#' example(make_demo_emb) # creates mm
#' nnn = ingest_embeddings(mm)
#' names(nnn)
#' pp = prcomp(t(nnn$cemb))
#' data(p3k.fine)
#' p3k=TENxPBMCData("pbmc3k")
#' p3k = scuttle::logNormCounts(p3k)
#' mm = match(p3k$Barcode, nnn$C_entities_ordered)
#' pairs(pp$x[mm,2:4], pch=19, cex=.4, col=factor(p3k.fine))
#' @export
ingest_embeddings = function(outemb) {
#
# define a class for the embed_sce run?
#
   chkp = outemb$conf$checkpoint_path
   entp = outemb$conf$entity_path
   hpath = dir(chkp, full.names=TRUE, patt="h5$")
   cembpath = grep("C_0", hpath, value=TRUE)
   gembpath = grep("G_0", hpath, value=TRUE)

   cemb = rhdf5::h5read(cembpath, "embeddings")
   gemb = rhdf5::h5read(gembpath, "embeddings")

   cents = jsonlite::fromJSON(paste0(entp, "/", "entity_names_C_0.json"))
   gents = jsonlite::fromJSON(paste0(entp, "/", "entity_names_G_0.json"))
   statstmp = readLines(sprintf("%s/training_stats.json", chkp))
   statsa = lapply(statstmp, jsonlite::fromJSON)
   stats = do.call( rbind, lapply(statsa, data.frame))

 list(cemb=cemb, gemb=gemb, stats=stats, C_entities_ordered=cents, G_entities_ordered=gents)
}

