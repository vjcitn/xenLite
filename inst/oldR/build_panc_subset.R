xenbase = function(ent)
 sprintf("https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/%s", ent)


#' cache and/or retrieve path to zipped pancreas subset (originally from SFEData)
#' @param cache BiocFileCache::BiocFileCache() is default
#' @importFrom methods as slot<-
#' @examples
#' cache_panc_subset()
#' @export
cache_panc_subset = function(cache=BiocFileCache::BiocFileCache()) {
   need = "sfe_pancsub.zip"
   chk = BiocFileCache::bfcquery(cache, need)
   nn = nrow(chk)
   if (nn>0) return(chk[nn,]$rpath)
   BiocFileCache::bfcadd(cache, rname=xenbase(need), action="copy", download=TRUE)
}

#' build a XenSCE instance based on the SFEData XeniumOutput element
#' @import SingleCellExperiment S4Vectors
#' @param cache BiocFileCache::BiocFileCache() is default
#' @importFrom utils unzip
#' @note This builds a XenSCE instance with on-disk counts.  `data(panc_sub)`
#' retrieves an instance with counts in dgCMatrix format, in memory.
#' @examples
#' panc_sub_h5 = build_panc_subset()
#' panc_sub_h5
#' @export
build_panc_subset = function(cache=BiocFileCache::BiocFileCache()) {
  pa = cache_panc_subset(cache)
  dir.create(td <- tempfile("pancsub"))
  x = unzip(pa, exdir=td)
  td = file.path(td, "sfedxtest")
  counts = HDF5Array::TENxMatrix(file.path(td, "cell_feature_matrix.h5"))
  tx = read.csv(file.path(td, "transcripts.csv.gz"))
  nb = read.csv(file.path(td, "nucleus_boundaries.csv.gz"))
  cb = read.csv(file.path(td, "cell_boundaries.csv.gz"))
  cmeta = read.csv(file.path(td, "cells.csv.gz"))
  sce = SingleCellExperiment(assays=SimpleList(counts=counts))
  colData(sce) = DataFrame(cmeta)
  colnames(sce) = sce$cell_id
  syms = DataFrame(symbol=e2sym(rownames(sce)))
  rowData(sce) = syms
  spe = as(sce, "SpatialExperiment")
  SpatialExperiment::spatialCoords(spe) = data.matrix(as.data.frame(cmeta)[, c("x_centroid", "y_centroid")])
  new("XenSCE", spe, transcripts=DataFrame(tx), nucbounds=DataFrame(nb),
    cellbounds = DataFrame(cb))
}
