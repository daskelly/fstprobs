#' Write probs matrix to fst
#'
#' This function writes a probs matrix to a series of fst files. Took ~10s on my 2-core early 2015 Macbook Pro with SSD for probs with dimension 185 * 8 * 69005
#' @param probs 3D matrix of probs in DOQTL format
#' @param prefix prefix for fst files
#' @param compress compression level for fst files (passed to fst function)
#' @export
#' write_fst_probs()
write_fst_probs <- function(probs, prefix, compress=50) {
    assertthat::assert_that(dim(probs)[2] == 8)
    samples <- dimnames(probs)[[1]]
    haps <- dimnames(probs)[[2]]
    markers <- dimnames(probs)[[3]]
    for (i in 1:length(haps)) {
        hap_probs <- as.data.frame(t(probs[, i, ]))
        filename <- paste0(prefix, "_", haps[i], ".fst")
        fst::write_fst(hap_probs, filename, compress=compress)
    }
    filename <- paste0(prefix, "_names.fst")
    names_df <- data.frame(dim=rep(1:3, dim(probs)), 
        name=c(samples, haps, markers), stringsAsFactors=FALSE)
    fst::write_fst(names_df, filename, compress=compress)
}
