.onAttach = function(libname, pkgname) {
  check_bigg()
}
#pippath = (file.path(basilisk.utils::getExternalDir(), "BiocBigGraph", "0.0.3", "bsklenv", "bin", "pip"))
#tpath = system.file("python", "fbsource", package="BiocBigGraph")
#system2(pippath, c("install", tpath), env=c("PBG_INSTALL_CPP=1"))
#}
