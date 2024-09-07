
#' render boundaries of cells with optional centroid positions and transcript positions
#' @importFrom graphics points polygon
#' @param xsce XenSCE instance
#' @param add_cent logical(1)
#' @param cent_col character(1) default to "red"
#' @param cent_cex numeric(1) default to 0.2
#' @param add_tx logical(1)
#' @param tx_cex numeric(1)
#' @examples
#' dem = build_panc_subset()
#' plotCellBoundaries(clip_rect(dem, xlim=c(600,850), ylim=c(500,750)))
#' @export
plotCellBoundaries = function(xsce, add_cent=TRUE, cent_col="red", cent_cex=.2, add_tx=TRUE,
   tx_cex=.1) {
  bb = getCellBoundaries(xsce)
  bb = bb[as(bb$cell_id, "character") %in% colnames(xsce),]
  xlim = range(bb$vertex_x)
  ylim = range(bb$vertex_y)
  plot(min(xlim), min(ylim), pch=" ", xlim=xlim, ylim=ylim, xlab="x", ylab="y")
  bid = as.character(bb$cell_id)
  sbb = split(as(bb, "data.frame"), bid)
  for (i in seq_len(length(sbb))) polygon(sbb[[i]]$vertex_x, sbb[[i]]$vertex_y)
  if (add_cent) points(xsce$x_centroid, xsce$y_centroid, cex=cent_cex, col=cent_col)
  if (add_tx) points(getTranscripts(xsce)$x_location, getTranscripts(xsce)$y_location, cex=tx_cex)
  invisible(NULL)
}

#library(XenSCE)
#x = build_panc_subset()
#plotCellBoundaries(clip_rect(x, xlim=c(200,450), ylim=c(800,1000)))
