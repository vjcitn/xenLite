#' helper function to map ENS ids to symbols
#' @importFrom utils data
#' @param x character() mix of Ensembl Ids and other strings; the latter are left unchanged
#' @return a vector like x with gene symbols from v79 mapping substituted where possible
#' @examples
#' e2sym(c("ABC", "ENSG00000213088", "ENSG00000107796", "ENSG00000163017"))
#' @export
e2sym = function (x) 
{
    data("e79sym", package="xenLite")
    toch = which(x %in% names(e79sym))
    x[toch] = as.character(e79sym[x[toch]])
    x
}

