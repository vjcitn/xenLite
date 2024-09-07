#' utility for dealing with cached Xen_SPEP in temp folder
#' @param xsp instance of XenSPEP
#' @param base folder path where parquet files are found
#' @note Will prepend current folder path to parquet-oriented
#' slot values.
#' @examples
#' prp = cache_xen_prost() # parquet paths are pre-set :(
#' dir.create(td <- file.path(tempdir(), "xen_work"))
#' unzip(prp, exdir=td)
#' ini = HDF5Array::loadHDF5SummarizedExperiment(file.path(td, "xen_prost"))
#' fin = reset_parq_paths(ini, base=td)
#' print(fin)
#' txloc = arrow::read_parquet(slot(fin, "tx_path"), as_data_frame=FALSE) # premature
#' txloc |> dplyr::count() |> dplyr::collect()
#' fin = loadGeometry(fin) # proper
#' slot(fin, "txtab")
#' slot(fin, "txtab") |> dplyr::count() |> dplyr::collect()
#' @export
reset_parq_paths = function(xsp, base) {
  xsp@cellbounds_path = file.path(base, basename(xsp@cellbounds_path))
  xsp@nucbounds_path = file.path(base, basename(xsp@nucbounds_path))
  xsp@tx_path = file.path(base, basename(xsp@tx_path))
  xsp
}

