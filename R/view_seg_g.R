#' naive polygon viewer, will indicate presence of transcripts for two genes in cells
#' @param x instance of XenSPEP
#' @param xlim numeric(2) ordered vector of max and min on x
#' @param ylim numeric(2) ordered vector of max and min on y
#' @param show_tx logical(1) display transcript locations if TRUE, defaults to FALSE.
#' @param gene1 character(1) gene to be checked, cell polygon will be filled if gene has non-zero count
#' @param gene2 character(1) gene to be checked, cell polygon will be filled if gene has non-zero count
#' @param \dots passed to polygon()
#' @note This is more RAM-sparing than clipRect followed by view.  Colors are
#' pre-assigned for individual and joint occupancies in this draft of this visualizer.
#' @return Primarily for plotting.  A list is invisibly returned with elements polys,
#' ncells and call.
#' @examples
#' luad <- cacheXenLuad()
#' pa <- cacheXenLuad()
#' luad <- restoreZipXenSPEP(pa)
#' rownames(luad) <- make.names(SummarizedExperiment:::rowData(luad)$Symbol, unique = TRUE)
#' out <- viewSegG2(luad, c(5800, 6300), c(1300, 1800), lwd = .5, gene1 = "CD4", gene2 = "EPCAM")
#' legend(5800, 1370, fill = c("purple", "cyan", "pink"), legend = c("CD4", "EPCAM", "both"))
#' out$ncells
#' @export
viewSegG2 <- function(x, xlim, ylim, gene1, gene2, show_tx = FALSE, ...) {
  stopifnot(gene1 %in% rownames(x))
  stopifnot(gene2 %in% rownames(x))
  cc <- match.call()
  if (!requireNamespace("SpatialExperiment")) {
    stop("install SpatialExperiment to use this function.")
  }
  ppdf <- data.frame(SpatialExperiment::spatialCoords(x))
  rngs <- vapply(ppdf[, c("x_centroid", "y_centroid")], range, numeric(2))
  cb <- getCellBoundaries(x)
  cb2 <- cb[cb$vertex_x > xlim[1] & cb$vertex_x < xlim[2] &
    cb$vertex_y > ylim[1] & cb$vertex_y < ylim[2], ]
  cb2w <- as.data.frame(cb2)
  un <- unique(cb2w$cell_id)
  if (nrow(cb2w) == 0) {
    stop("no observations for cell boundaries.")
  }
  scb2w <- split(cb2w, cb2w$cell_id)
  ncells <- length(scb2w)
  rngs <- vapply(cb2w[, c("vertex_x", "vertex_y")], range, numeric(2))
  plot(rngs[1, 1], rngs[1, 2], xlim = rngs[, 1], ylim = rngs[
    ,
    2
  ], xlab = "x", ylab = "y", pch = " ")
  pres1 <- as.matrix(SummarizedExperiment::assay(x[gene1, un])) >
    0
  pres2 <- as.matrix(SummarizedExperiment::assay(x[gene2, un])) >
    0
  zz <- lapply(seq_len(length(scb2w)), function(i, ...) {
    thed <- NULL
    col <- "white"
    angle <- 0
    if (isTRUE(pres1[i])) {
      thed <- NULL
      angle <- 45
      col <- "purple"
    }
    if (isTRUE(pres2[i])) {
      thed <- NULL
      angle <- 135
      col <- "cyan"
    }
    if (isTRUE(pres2[i]) & isTRUE(pres1[i])) {
      thed <- NULL
      angle <- 180
      col <- "pink"
    }
    polygon(scb2w[[i]]$vertex_x, scb2w[[i]]$vertex_y,
      density = thed,
      col = col, angle = angle, ...
    )
  }, ...)
  if (show_tx) {
    tx <- getTranscripts(x)
    message("start filter tx")
    tx2 <- tx[tx$x_location > xlim[1] & tx$x_location < xlim[2] &
      tx$y_location > ylim[1] & tx$y_location < ylim[2], ]
    message("end filter tx")
    points(tx2$x_location, tx2$y_location,
      pch = ".", cex = 0.1,
      col = "gray"
    )
  }
  invisible(list(polys = zz, ncells = ncells, call = cc))
}


viewSegG <- function(x, xlim, ylim, gene, show_tx = FALSE, ...) {
  stopifnot(gene %in% rownames(x))
  cc <- match.call()
  if (!requireNamespace("SpatialExperiment")) stop("install SpatialExperiment to use this function.")
  ppdf <- data.frame(SpatialExperiment::spatialCoords(x))
  rngs <- vapply(ppdf[, c("x_centroid", "y_centroid")], range, numeric(2))
  #  stopifnot(rngs
  cb <- getCellBoundaries(x)
  cb2 <- cb[cb$vertex_x > xlim[1] & cb$vertex_x < xlim[2] & cb$vertex_y > ylim[1] & cb$vertex_y < ylim[2], ]
  cb2w <- as.data.frame(cb2)
  un <- unique(cb2w$cell_id)
  if (nrow(cb2w) == 0) stop("no observations for cell boundaries.")
  scb2w <- split(cb2w, cb2w$cell_id)
  ncells <- length(scb2w)
  rngs <- vapply(cb2w[, c("vertex_x", "vertex_y")], range, numeric(2))
  plot(rngs[1, 1], rngs[1, 2], xlim = rngs[, 1], ylim = rngs[, 2], xlab = "x", ylab = "y", pch = " ")
  pres <- as.matrix(SummarizedExperiment::assay(x[gene, un])) > 0
  zz <- lapply(seq_len(length(scb2w)), function(i, ...) {
    thed <- NULL
    if (isTRUE(pres[i])) thed <- 20
    polygon(scb2w[[i]]$vertex_x,
      scb2w[[i]]$vertex_y,
      density = thed, ...
    )
  }, ...)
  if (show_tx) {
    tx <- getTranscripts(x)
    message("start filter tx")
    tx2 <- tx[tx$x_location > xlim[1] & tx$x_location < xlim[2] & tx$y_location > ylim[1] & tx$y_location < ylim[2], ]
    message("end filter tx")
    points(tx2$x_location, tx2$y_location, pch = ".", cex = .1, col = "gray")
  }
  # scb2w[[1]]
  # head(scb2w[[1]])
  # lapply(scb2w, function(x) polygon(x$vertex_x, x$vertex_y, ...))
  # savehistory(file="lkpoly.hist.txt")
  invisible(list(polys = zz, ncells = ncells, call = cc))
}
