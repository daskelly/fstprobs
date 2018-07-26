#' Read probs matrix from fst
#'
#' Given some fst files written by write_fst_probs, read them back into a 3D probs array. Took ~5s on my 2-core early 2015 Macbook Pro with SSD for probs with dimension 185 * 8 * 69005
#' @param prefix prefix passed to write_fst_probs()
#' @export
#' read_fst_probs()
read_fst_probs <- function(prefix) {
    filename <- paste0(prefix, "_names.fst")
    assertthat::assert_that(file.exists(filename))
    names_df <- fst::read_fst(filename)
    names_list <- unname(split(names_df$name, names_df$dim))
    hap_files <- paste0(prefix, "_", names_list[[2]], ".fst")
    probs_list <- lapply(hap_files, fst::read_fst)       
    probs <- abind::abind(probs_list, along=3)
    probs <- aperm(probs, c(2, 3, 1))
    dimnames(probs) <- names_list
    probs
}
