
# necessary for python module control
#' python declarations
#' @import basilisk
bsklenv <- basilisk::BasiliskEnvironment(
  envname = "bsklenv",
  pkgname = "BiocBigGraph",
  packages = c("python=3.11"),
  pip=c("torchbiggraph==1.dev1", "torch==2.1.1", "torchvision==0.16.1", "attr==0.3.2")
  )

