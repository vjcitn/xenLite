namesInBox <- function(xsce, xlim, ylim) {
  if (!requireNamespace("SpatialExperiment")) stop("install SpatialExperiment to use this function")
  cmat <- SpatialExperiment::spatialCoords(xsce)
  xc <- cmat[, "x_centroid"]
  yc <- cmat[, "y_centroid"]
  inds <- which(xc >= xlim[1] & xc <= xlim[2] & yc >= ylim[1] &
    yc <= ylim[2])
  unique(colnames(xsce[, inds]))
}

bfeatsInBox <- function(xsce, feat = "cellbounds", xlim, ylim) {
  fb <- slot(xsce, feat)
  xc <- fb[, "vertex_x"]
  yc <- fb[, "vertex_y"]
  inds <- which(xc >= xlim[1] & xc <= xlim[2] & yc >= ylim[1] &
    yc <= ylim[2])
  fb[inds, ]
}

newbfinbx <- function(xsce, feat = "cellbounds", xlim, ylim) {
  #
  # works with cbtab slot of XenSPEP
  #
  fb <- slot(xsce, feat)
  xc <- fb[, "vertex_x"]
  if (isFALSE(is(xc, "numeric"))) {
    xc <- xc |>
      dplyr::collect() |>
      unlist() |>
      as.numeric()
  }
  yc <- fb[, "vertex_y"]
  if (isFALSE(is(yc, "numeric"))) {
    yc <- yc |>
      dplyr::collect() |>
      unlist() |>
      as.numeric()
  }
  fb[xc >= xlim[1] & xc <= xlim[2] & yc >= ylim[1] &
    yc <= ylim[2], ]
}


txfeatsInBox <- function(xsce, feat = "transcripts", xlim, ylim) # may have colname x or x_location etc.
{
  fb <- slot(xsce, feat)
  nms <- names(fb)
  if ("x" %in% nms) use <- c(x = "x", y = "y")
  if ("x_location" %in% nms) use <- c(x = "x_location", y = "y_location")
  xc <- fb[, use["x"]]
  yc <- fb[, use["y"]]
  inds <- which(xc >= xlim[1] & xc <= xlim[2] & yc >= ylim[1] &
    yc <= ylim[2])
  fb[inds, ]
}

#' restrict XenSPEP to cells with centroids in specified rectangle,
#' also restrict boundary and transcript location features
#' @param xsce XenSPEP instance
#' @param xlim numeric(2)
#' @param ylim numeric(2)
#' @note Could be too RAM-hungry.
#' @return XenSPEP instance
#' @examples
#' args(clipRect)
#' @export
clipRect <- function(xsce, xlim, ylim) {
  nn <- namesInBox(xsce, xlim, ylim)
  stopifnot(length(nn) > 0)
  ini <- xsce[, nn]
  cb <- bfeatsInBox(ini, xlim = xlim, ylim = ylim)
  nb <- bfeatsInBox(ini, "nucbounds", xlim, ylim)
  tloc <- txfeatsInBox(ini, "transcripts", xlim, ylim)
  slot(ini, "cellbounds") <- cb
  slot(ini, "nucbounds") <- nb
  slot(ini, "transcripts") <- tloc
  ini
}
