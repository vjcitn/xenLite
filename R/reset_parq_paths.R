#' utility for dealing with cached Xen_SPEP in temp folder
#' @param xsp instance of XenSPEP
#' @param base folder path where parquet files are found
#' @note Will prepend current folder path to parquet-oriented
#' slot values.
#' @return XenSPEP instance
#' @export
reset_parq_paths = function(xsp, base) {
  xsp@cellbounds_path = file.path(base, basename(xsp@cellbounds_path))
  xsp@nucbounds_path = file.path(base, basename(xsp@nucbounds_path))
  xsp@tx_path = file.path(base, basename(xsp@tx_path))
  xsp
}

