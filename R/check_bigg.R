#' verify access to modified torchbiggraph
#' @export
check_bigg = function() {
 fbloc = system.file("python", "fbsource", package="BiocBigGraph")
 proc = basilisk::basiliskStart(bsklenv, fork=FALSE)
 on.exit(basilisk::basiliskStop(proc))
 basilisk::basiliskRun(proc, function() {
 att = names(import("attrs"))
 torn = names(import_from_path("torchbiggraph", path=fbloc))
 conf = names(import_from_path("torchbiggraph.config", path=fbloc))
 imps = names(import_from_path("torchbiggraph.converters.importers", path=fbloc))
# att = names(import("attrs"))
# torn = names(import("torchbiggraph"))
# conf = names(import("torchbiggraph.config"))
# imps = names(import("torchbiggraph.converters.importers"))
 list(tor=torn, conf=conf, imps=imps, att=att)
 }, fork=FALSE)
}
