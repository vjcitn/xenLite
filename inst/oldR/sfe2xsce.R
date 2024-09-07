
colgeom2df = function(sfe, elem="cellSeg", BPPARAM) {
  if (!requireNamespace("SpatialFeatureExperiment")) stop("install SpatialFeatureExperiment to use this function")
  stopifnot(elem %in% names(SpatialFeatureExperiment::colGeometries(sfe)))
  geomel = SpatialFeatureExperiment::colGeometries(sfe)[[elem]]
  blens = BiocParallel::bplapply(seq_len(nrow(geomel)) , 
    function(x) nrow(geomel[x,]$geometry[[1]][[1]][[1]]),
    BPPARAM=BPPARAM)
  cnames = rep(colnames(sfe), blens)
  cbmats = BiocParallel::bplapply(seq_len(nrow(geomel)) , 
    function(x) geomel[x,]$geometry[[1]][[1]][[1]],
    BPPARAM=BPPARAM)
  matall = do.call(rbind, cbmats)
  ndf = data.frame(cnames, matall)
  names(ndf) = c("cell_id", "vertex_x", "vertex_y")
  ndf
}

colgeom2df_safe = function(sfe, elem="nucSeg", BPPARAM) {
# deals with empty nuclei
  if (!requireNamespace("SpatialFeatureExperiment")) stop("install SpatialFeatureExperiment to use this function")
  stopifnot(elem %in% names(SpatialFeatureExperiment::colGeometries(sfe)))
  geomel = SpatialFeatureExperiment::colGeometries(sfe)[[elem]]
  blens = BiocParallel::bplapply(seq_len(nrow(geomel)) , 
    function(x) try(nrow(geomel[x,]$geometry[[1]][[1]][[1]]), silent=TRUE),
    BPPARAM=BPPARAM)
  bad = which(sapply(blens, inherits, "try-error")) # needed for empty nuclei
  if (length(bad)==0) return(colgeom2df(sfe, elem, BPPARAM))
  blens = blens[-bad]
  cnames = rep(colnames(sfe)[-bad], blens)
  cbmats = BiocParallel::bplapply(seq_len(nrow(geomel))[-bad] , 
    function(x) geomel[x,]$geometry[[1]][[1]][[1]],
    BPPARAM=BPPARAM)
  matall = do.call(rbind, cbmats)
  ndf = data.frame(cnames, matall)
  names(ndf) = c("cell_id", "vertex_x", "vertex_y")
  ndf
}

rowgeom2df = function(sfe, elem="txSpots", BPPARAM) {
  if (!requireNamespace("SpatialFeatureExperiment")) stop("install SpatialFeatureExperiment to use this function")
  stopifnot(elem %in% names(SpatialFeatureExperiment::rowGeometries(sfe)))
  geomel = SpatialFeatureExperiment::rowGeometries(sfe)[[elem]]
  blens = BiocParallel::bplapply(seq_len(nrow(geomel)), 
    function(x) nrow(as(geomel[x,]$geometry[[1]], "matrix")),
    BPPARAM=BPPARAM)
  cnames = rep(geomel$gene, blens)
  cbmats = BiocParallel::bplapply(seq_len(nrow(geomel)), 
    function(x) as(geomel[x,]$geometry[[1]], "matrix"),
    BPPARAM=BPPARAM)
  matall = do.call(rbind, cbmats)
  ndf = data.frame(cnames, matall)
  names(ndf) = c("gene", "x", "y", "z")
  ndf
}


#' convert SpatialFeatureExperiment to XenSCE
#' @import BiocParallel
#' @param sfe SpatialFeatureExperiment instance
#' @param BPPARAM see BiocParallel
#' @examples
#' example(cache_sfeLung)
#' nx = sfe2xsce(ans, BPPARAM=BiocParallel::MulticoreParam(8))
#' nx
#' @export 
sfe2xsce = function(sfe, BPPARAM=BiocParallel::SerialParam()) {
  spe = as(sfe, "SpatialExperiment") # has spatialCoords
  cellb = colgeom2df(sfe, "cellSeg", BPPARAM=BPPARAM)
  nucb = colgeom2df_safe(sfe, "nucSeg", BPPARAM=BPPARAM)
  txs = rowgeom2df(sfe, "txSpots", BPPARAM=BPPARAM)
  sdf = S4Vectors::DataFrame  # move up to '2df' funcs
  new("XenSCE", spe, cellbounds=sdf(cellb), nucbounds=sdf(nucb), transcripts=sdf(txs))
}
