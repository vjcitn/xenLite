#' XenSPEP (SpatialExperiment with parquet references) constructor
#' @importFrom SummarizedExperiment colData<-
#' @param folder character(1) 'standard' Xenium output folder
#' @return instance of XenSPEP
#' @examples
#' # is not used yet
#' args(XenSPEP)
#' @export
XenSPEP = function(folder) {
  stopifnot(file.exists(cfmpath <- file.path(folder, "cell_feature_matrix.tar.gz")))
  stopifnot(file.exists(cmetapath <- file.path(folder, "cells.parquet")))
  stopifnot(file.exists(cbpath <- file.path(folder, "cell_boundaries.parquet")))
  stopifnot(file.exists(nbpath <- file.path(folder, "nucleus_boundaries.parquet")))
  stopifnot(file.exists(txpath <- file.path(folder, "transcripts.parquet")))
  txf = TENxIO::TENxFile(cfmpath)
  sce = TENxIO::import(txf) # SCE with dgCMatrix for assay
  cmeta = arrow::read_parquet("cells.parquet") 
  cd = as.data.frame(cmeta)
  stopifnot(all(c("x_centroid", "y_centroid") %in% names(cd)))
  rownames(cd) = cd[,1]
  cd = S4Vectors::DataFrame(cd)
  colData(sce) = cd
  spe = as(sce, "SpatialExperiment") # propagates assayNames
  SpatialExperiment::spatialCoords(spe) = data.matrix(cd[,c("x_centroid", "y_centroid")])
  cb = arrow::read_parquet(cbpath)
  new("XenSPEP", spe, cellbounds_path=file.path(folder, "cell_boundaries.parquet"),
       nucbounds_path=file.path(folder, "nucleus_boundaries.parquet"), 
       tx_path=file.path(folder, "transcripts.parquet"),
       loaded=TRUE, cbtab = arrow::read_parquet("cell_boundaries.parquet", as_data_frame=FALSE),
       nbtab = arrow::read_parquet("nucleus_boundaries.parquet", as_data_frame=FALSE),
       txtab = arrow::read_parquet("transcripts.parquet", as_data_frame=FALSE))
}
