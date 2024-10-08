#' cache and/or retrieve path to xenium human prostate adenocarcinoma, 5k genes
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where zip file can be retrieved
#' @note Retrieves path to a 912MB zip file.  This
#' includes a 'saved' HDF5-backed SpatialExperiment and
#' parquet files for cell, nucleus, and transcript coordinates.
#' The example shows how these can be assembled into a
#' XenSPEP instance.
#' @examples
#' if (interactive()) {
#'  td = tempdir()
#'  pzip_path = cache_xen_prost()
#'  dir.create(xpw <- file.path(td, "xen_prost_work"))
#'  unzip(pzip_path, exdir=xpw)
#'  prost = HDF5Array::loadHDF5SummarizedExperiment(file.path(xpw, "xen_prost"))
#'  prost
#'  prost = reset_parq_paths(prost, xpw)
#'  prost = loadGeometry(prost)
#'  print(slot(prost, "cbtab") |> head() |> dplyr::collect())
#'  print(slot(prost, "txtab") |> dplyr::count() |> dplyr::collect())
#'  plot(SpatialExperiment::spatialCoords(prost), pch=".",
#'    main="10x FFPE Human prostate adenocarcinoma sample")
#' }
#' @export
cache_xen_prost = function(cache=BiocFileCache::BiocFileCache(), 
   url="https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/xen_prost.zip") {
 chk = bfcquery(cache, "BiocXenData/xen_prost.zip")
 n = nrow(chk)
 if (n>=1) return(chk[n,]$rpath)
 bfcadd(cache, rname=basename(url), fpath=url, action="copy", download=TRUE)
}
