#' read and bind parquet data to XenSPEP
#' @param x instance of XenSPEP
#' @return instance of XenSPEP
#' @export
setGeneric("loadGeometry", function(x) standardGeneric("loadGeometry"))

#' read and bind parquet data to XenSPEP
#' @param x instance of XenSPEP
#' @return instance of XenSPEP
#' @export
setMethod("loadGeometry", "XenSPEP", function(x) {
  slot(x, "cbtab") <- arrow::read_parquet(slot(x, "cellbounds_path"), as_data_frame = FALSE)
  slot(x, "nbtab") <- arrow::read_parquet(slot(x, "nucbounds_path"), as_data_frame = FALSE)
  slot(x, "txtab") <- arrow::read_parquet(slot(x, "tx_path"), as_data_frame = FALSE)
  slot(x, "loaded") <- TRUE
  x
})
