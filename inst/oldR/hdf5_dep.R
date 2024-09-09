
#' cache and/or retrieve path to an SFE of V1 lung demo data from 10x
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where zip file can be retrieved
#' @note We are explicitly avoiding declaring reliance on terra
#' or SpatialFeatureExperiment, to keep package weight low.
#' Thus the example will fail unless these are already present.
#' @examples
#' if (!requireNamespace("terra")) {
#'   message("install terra package to run this example")
#'   } else if (!requireNamespace("SpatialFeatureExperiment")) {
#'   message("install SpatialFeatureExperiment package to run this example")
#'   } else {
#'   zp = cache_sfeLung()
#'   td = tempdir()
#'   unzip(zp, exdir=td)
#'   ans = HDF5Array::loadHDF5SummarizedExperiment(file.path(td, "lungSFEtxg"))
#'   SpatialFeatureExperiment::show(ans)
#'   }
#' @export
cache_sfeLung = function(cache=BiocFileCache::BiocFileCache(), 
   url="https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/lungSFEtxg.zip") {
 chk = bfcquery(cache, "BiocXenData/lungSFEtxg.zip")
 n = nrow(chk)
 if (n>=1) return(chk[n,]$rpath)
 bfcadd(cache, rname=basename(url), fpath=url, action="copy", download=TRUE)
}


#' cache and/or retrieve path to xenium human primary melanoma, 5k genes
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where zip file can be retrieved
#' @note Retrieves path to a 1.05GB zip file.  The zip file
#' includes a 'saved' HDF5-backed SpatialExperiment and
#' parquet files for cell, nucleus, and transcript coordinates.
#' The example shows how these can be assembled into a
#' XenSPEP instance.
#' @examples
#' if (interactive()) {
#'  td = tempdir()
#'  pzip_path = cache_xen_sk()
#'  dir.create(xpw <- file.path(td, "xen_sk_work"))
#'  unzip(pzip_path, exdir=xpw)
#'  mel = HDF5Array::loadHDF5SummarizedExperiment(file.path(xpw, "xen_sk"))
#'  mel = reset_parq_paths(mel, xpw)
#'  mel = loadGeometry(mel)
#'  print(slot(mel, "cbtab") |> head() |> dplyr::collect())
#'  print(slot(mel, "txtab") |> dplyr::count() |> dplyr::collect())
#'  plot(SpatialExperiment::spatialCoords(mel), pch=".",
#'    main="10x FFPE Human dermal melanoma sample")
#' }
#' @export
cache_xen_sk = function(cache=BiocFileCache::BiocFileCache(), 
   url="https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/xen_sk_spep.zip") {
 chk = bfcquery(cache, "BiocXenData/xen_sk_spep.zip")
 n = nrow(chk)
 if (n>=1) return(chk[n,]$rpath)
 bfcadd(cache, rname=basename(url), fpath=url, action="copy", download=TRUE)
}

