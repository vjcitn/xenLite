
#' print method for ggplot2-prepared XenSPEP
#' @param x instance of S3 class `xen_ggprep`
#' @param \dots not used
#' @export
print_xen_ggprep = function(x, ...) {
  cat(sprintf("xen_ggprep instance for %d cells with %d cell boundary vertices.\n", ncol(x$sampd), nrow(x$bounds)))
}
 
#' prepare a XenSPEP for ggplot2 visualization
#' @param xsce XenSPEP instance
#' @param xlim numeric(2)
#' @param ylim numeric(2)
#' @note This is idiosyncratic.  Quintiles of cell_area are produced, and transcript locations
#' are filtered
#' @return a list with components `bounds` (data.frame including relevant colData
#' rows (all colData variables) and cell boundary coordinates) and `txdata`, a filtered
#' arrow Table.
#' @export
ggprep_seg = function(xsce, xlim=c(5800,6200), ylim=c(6300, 6700)) {
  sampd = as.data.frame(SummarizedExperiment::colData(xsce)) # 'population level' quantiles
  sizq = cut(sampd$cell_area, c(0, quantile(sampd$cell_area, c(.2,.4,.6,.8)), max(sampd$cell_area)+.01))
  sampd$sizq = sizq
  bounds = getCellBoundaries(xsce)
  bounds = bounds[bounds$vertex_x > xlim[1] &
      bounds$vertex_x < xlim[2] & bounds$vertex_y > ylim[1] & bounds$vertex_y < ylim[2],]
  c4 <- as.data.frame(bounds)
  tx = getTranscripts(xsce)
  myt = tx[tx$x_location > xlim[1] & tx$x_location < xlim[2] &
                  tx$y_location > ylim[1] & tx$y_location < ylim[2], ]
  bounds = dplyr::left_join(c4, sampd, by="cell_id")
  ans = list(bounds=bounds, txdata=myt)
  class(ans) = c("xen_ggprep", "list")
  ans
}

#' plot method for ggplot2-prepared XenSPEP
#' @importFrom ggplot2 ggplot aes geom_polygon
#' @param x instance of S3 class `xen_ggprep`
#' @param y not used
#' @param \dots not used
#' @note roxygen had problems with this
#' @export
plot_xen_ggprep = function(x, y, ...) {
  ggplot(x$bounds, aes(x=vertex_x, y=vertex_y, group=cell_id, colour=sizq, fill=sizq)) + 
      geom_polygon(alpha=.5) #+ geom_path(colour="black")
}
