#' print method for ggplot2-prepared XenSPEP
#' @param x instance of S3 class `xen_ggprep`
#' @param \dots not used
#' @return operates with cat()
printXenGgprep <- function(x, ...) {
  cat(sprintf("xen_ggprep instance for %d cells with %d cell boundary vertices.\n", ncol(x$sampd), nrow(x$bounds)))
}

#' prepare a XenSPEP for ggplot2 visualization
#' @importFrom stats quantile
#' @param xsce XenSPEP instance
#' @param xlim numeric(2)
#' @param ylim numeric(2)
#' @note This is idiosyncratic.  Quintiles of cell_area (values in `sizq`) are produced, and transcript locations
#' are filtered.  A more general approach that allows selection of coloring
#' of cells by feature characteristics is needed.
#' @return a list with components `bounds` (data.frame including relevant colData
#' rows (all colData variables) and cell boundary coordinates) and `txdata`, a filtered
#' arrow Table.
#' @examples
#' pa <- cacheXenLuad()
#' luad <- restoreZipXenSPEP(pa)
#' hh <- ggprepSeg(luad, c(4000, 4500), c(2000, 2500))
#' ggplot2::ggplot(hh$bounds, ggplot2::aes(
#'   x = vertex_x, y = vertex_y, group = cell_id,
#'   colour = sizq, fill = sizq
#' )) +
#'   ggplot2::geom_polygon(alpha = .5)
#' @export
ggprepSeg <- function(xsce, xlim = c(5800, 6200), ylim = c(6300, 6700)) {
  sampd <- as.data.frame(SummarizedExperiment::colData(xsce)) # 'population level' quantiles
  sizq <- cut(sampd$cell_area, c(0, quantile(sampd$cell_area, c(.2, .4, .6, .8)), max(sampd$cell_area) + .01))
  sampd$sizq <- sizq
  bounds <- getCellBoundaries(xsce)
  bounds <- bounds[bounds$vertex_x > xlim[1] &
    bounds$vertex_x < xlim[2] & bounds$vertex_y > ylim[1] & bounds$vertex_y < ylim[2], ]
  c4 <- as.data.frame(bounds)
  tx <- getTranscripts(xsce)
  myt <- tx[tx$x_location > xlim[1] & tx$x_location < xlim[2] &
    tx$y_location > ylim[1] & tx$y_location < ylim[2], ]
  bounds <- dplyr::left_join(c4, sampd, by = "cell_id")
  ans <- list(bounds = bounds, txdata = myt)
  class(ans) <- c("xen_ggprep", "list")
  ans
}

#' plot method for ggplot2-prepared XenSPEP
#' @importFrom ggplot2 ggplot aes geom_polygon
#' @param x instance of S3 class `xen_ggprep`
#' @param y not used
#' @param \dots not used
#' @return ggplot
#' @note roxygen had problems with this
#' @examples
#' pa <- cacheXenLuad()
#' luad <- restoreZipXenSPEP(pa)
#' hh <- ggprepSeg(luad, c(4000, 4500), c(2000, 2500))
#' plotXenGgprep(hh)
#' @export
plotXenGgprep <- function(x, y, ...) {
  ggplot(x$bounds, aes(x = vertex_x, y = vertex_y, group = cell_id, colour = sizq, fill = sizq)) +
    geom_polygon(alpha = .5) #+ geom_path(colour="black")
}
