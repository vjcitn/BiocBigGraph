#' needed to ensure code for gpu usage is compiled
#' @note Runs basilisk-managed pip to compile and install
#' GPU-enabled torchbiggraph.
#' @export
set_gpu = function() {
pippath = (file.path(basilisk.utils::getExternalDir(), "BiocBigGraph", "0.0.3", "bsklenv", "bin", "pip"))
tpath = system.file("python", "fbsource", package="BiocBigGraph")
system2(pippath, c("install", tpath), env=c("PBG_INSTALL_CPP=1"))
}

#' needed to allow subprocess to find torchbiggraph for train module
#' @note Runs basilisk-managed pip to compile and install
#' @export
set_train = function() {
pippath = (file.path(basilisk.utils::getExternalDir(), "BiocBigGraph", "0.0.3", "bsklenv", "bin", "pip"))
tpath = system.file("python", "fbsource", package="BiocBigGraph")
system2(pippath, c("install", tpath))
system2(pippath, c("install", "torch")) # needed for random seed set?
}
