# https://mghp.osn.xsede.org/bir190004-bucket01/BiocBigGraphDemo/demo4.zip


#' check that a URL can get a 200 for a HEAD request
#' @importFrom httr status_code HEAD
#' @param url character(1)
#' @return logical(1)
url_ok = function(url) {
  httr::status_code(httr::HEAD(url)) == 200
}

#' cache the demonstration BigGraph output (train only)
#' @param cache BiocFileCache instance or equivalent
#' @param url character(1) defaults to OSN address of demo4.zip, produced March 2024
#' @note This function will check for presence of url in cache using bfcquery; 
#' if a hit is found, returns the rpath associated with the last
#' matching record.  etags can be available for use with bfcneedsupdate.
#' @examples
#' ca = BiocFileCache::BiocFileCache()
#' bgdempath = grab_bgdemo(ca)
#' bgdempath
#' @export
grab_bgdemo = function(cache = BiocFileCache::BiocFileCache(), url=
  "https://mghp.osn.xsede.org/bir190004-bucket01/BiocBigGraphDemo/demo4.zip"
   ) {
   inf = BiocFileCache::bfcquery(cache, url)
   if (nrow(inf)>0) {
     res = inf[nrow(inf),]
     message(sprintf("resource %s already in cache from %s\n", res$rid, url))
     return(res$rpath)
     } 
   if (!url_ok(url)) stop(sprintf("HEAD for %s does not return status code 200\n", url))
   unname(BiocFileCache::bfcadd(cache, rname = basename(url), fpath=url, rtype="web"))
}

#' cache the demonstration BigGraph output (train only)
#' @param cache BiocFileCache instance or equivalent
#' @param url character(1) defaults to OSN address of 1500 gene, 35 epoch
#' pbmc3k embedding, produced April 2024
#' @param use_400 logical(1) if TRUE use a slightly outdated
#' structure including a 400-d embedding, otherwise a 75-d embedding is used
#' @note This function will check for presence of url in cache using bfcquery.
#' @return SingleCellExperiment
#' @export
grab_bgdemo_sce = function(cache = BiocFileCache::BiocFileCache(), url=
  "https://mghp.osn.xsede.org/bir190004-bucket01/BiocBigGraphDemo/p3k_e35_g1500_75d.rda", use_400=FALSE) {
   if (use_400) url = gsub("_75d", "", url)
   pa = grab_bgdemo(cache=cache, url=url)
   get(load(pa))
}
