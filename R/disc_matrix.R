#' discretize a dense matrix
#' @importFrom stats quantile
#' @importFrom utils write.table
#' @param x matrix
#' @param n_bins numeric(1)
#' @param q4pos logical(1) if TRUE, quantiles are formed for positive entries only to form the cutpoints
#' @param cuts numeric vector, if NULL, quantiles are used as noted,
#' otherwise these are used with cut()
#' @note The lowest category includes all zero values.
#' @return matrix with values 0:(nbins-1) corresponding to quantile groups
#' @export
disc_matrix = function(x, n_bins=5, q4pos=TRUE, cuts=NULL) {
 xn = as.numeric(x)
 if (is.null(cuts)) {
   cuts = quantile(xn, probs = seq(0, 1, 1/n_bins))
   if (q4pos) cuts = quantile(xn[xn>0], probs = seq(0, 1, 1/n_bins))
   cuts[1] = min(xn)-.01
   cuts[length(cuts)] = max(xn)+.01
   }
 else {
   if (min(cuts) >= min(xn)) stop("min of supplied cuts must be less than min assay entries")
   if (max(cuts) <= max(xn)) stop("max of supplied cuts must be greater than max assay entries")
   }
 dx = cut(xn,  cuts)  # one-based factor
 x[] = as.numeric(dx)-1  # zero-based numeric
 x
}
