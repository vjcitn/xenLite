
#' produce a pre-loaded XenSPEP (SpatialExperiment with parquet references)
#' @import Matrix SingleCellExperiment HDF5Array
#' @param folder character(1) 'standard' Xenium output folder
#' @examples
#' chkns = function(pkstring) {
#' if (!requireNamespace(pkstring)) {
#'   message(sprintf("install %s to use this feature; returning NULL", pkstring))
#'   return(NULL)
#'   }
#' }
#' chkns("SFEData")
#' chkns("HDF5Array")
#' chkns("SingleCellExperiment")
#' if (requireNamespace("SFEData")) {
#' td = tempdir()
#' z = SFEData::XeniumOutput("v2", td)
#' ii = ingest_xen(file.path(td, "xenium2"))
#' print(validObject(ii))
#' plot(SpatialExperiment::spatialCoords(ii), pch=".")
#' }
#' @export
ingest_xen = function(folder) {
  stopifnot(file.exists(cmetapath <- file.path(folder, "cells.parquet")))
  stopifnot(file.exists(cbpath <- file.path(folder, "cell_boundaries.parquet")))
  stopifnot(file.exists(nbpath <- file.path(folder, "nucleus_boundaries.parquet")))
  stopifnot(file.exists(txpath <- file.path(folder, "transcripts.parquet")))
  if (file.exists(cfmpath <- file.path(folder, "cell_feature_matrix.tar.gz"))) {
    txf = TENxIO::TENxFile(cfmpath)
    sce = TENxIO::import(txf) # SCE with dgCMatrix for assay
    }
  else if (file.exists(cfmpath <- file.path(folder, "cell_feature_matrix.h5"))) {
    chkns = function(pkstring) {
      if (!requireNamespace(pkstring)) {
      message(sprintf("install %s to use this feature; returning NULL", pkstring))
      return(NULL)
      }
    }
    chkns("HDF5Array")
    chkns("SingleCellExperiment")
    chkns("S4Vectors")
    td = tempdir()
    quant = HDF5Array::TENxMatrix(cfmpath)
    quant = as(quant, "dgCMatrix") # dimnames are in
    sce =SingleCellExperiment::SingleCellExperiment(assays=S4Vectors::SimpleList(counts=quant))
    }
  else stop("can't find tar.gz or hdf5 for cell_feature_matrix")
  cmeta = arrow::read_parquet(cmetapath)
  cd = as.data.frame(cmeta)
  stopifnot(all(c("x_centroid", "y_centroid") %in% names(cd)))
  rownames(cd) = cd[,1]
  cd = S4Vectors::DataFrame(cd)
  colData(sce) = cd
  spe = as(sce, "SpatialExperiment") # propagates assayNames
  SpatialExperiment::spatialCoords(spe) = data.matrix(cd[,c("x_centroid", "y_centroid")])
  cb = arrow::read_parquet(cbpath)
  obj = new("XenSPEP", spe, cellbounds_path=file.path(folder, "cell_boundaries.parquet"),
       nucbounds_path=nbpath,
       tx_path=txpath,
       loaded=FALSE) 
  #list(spe=spe, cb=cb, obj=obj)
  obj
}

