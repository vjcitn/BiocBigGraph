
# necessary for python module control
#' python declarations
#' @import basilisk
bsklenv <- basilisk::BasiliskEnvironment(
  envname = "bsklenv",
  pkgname = "BiocBigGraph",
  packages = c("python=3.10"),
  pip=c("torch==2.1.1", "torchvision==0.16.1", "attrs==23.1.0",
     "h5py==3.9.0")
  )

