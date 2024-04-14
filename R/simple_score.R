simba_scores = function(cemb, gemb) {
  C = t(cemb)
  FF = gemb
  C %*% FF -> BIG
  EBIG = exp(BIG)
  CSUMS = apply(EBIG,1,sum)
  pmat = t(as.matrix(EBIG/CSUMS))
}

