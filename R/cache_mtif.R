#' cache and/or retrieve path to an ome.tif file for demonstration 
#' @import BiocFileCache
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where tiff file can be retrieved
#' @export
cache_mtif = function(cache=BiocFileCache::BiocFileCache(), 
   url="https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/morphology_focus_0001.ome.tif") {
 chk = bfcquery(cache, "BiocXenData/morphology_focus_0001.ome.tif")
 n = nrow(chk)
 if (n>=1) return(chk[n,]$rpath)
 bfcadd(cache, rname=basename(url), fpath=url, action="copy", download=TRUE)
}

#' cache and/or retrieve path to an SFE of V1 lung demo data from 10x
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where zip file can be retrieved
#' @note Lacks transcript coordinates
cache_sfeLung_ntx = function(cache=BiocFileCache::BiocFileCache(), 
   url="https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/sfeLung.zip") {
 chk = bfcquery(cache, "BiocXenData/sfeLung.zip")
 n = nrow(chk)
 if (n>=1) return(chk[n,]$rpath)
 bfcadd(cache, rname=basename(url), fpath=url, action="copy", download=TRUE)
}

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

#' cache and/or retrieve path to xenium human prostate adenocarcinoma, 5k genes
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where tiff file can be retrieved
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

#' cache and/or retrieve path to xenium human primary melanoma, 5k genes
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where tiff file can be retrieved
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


#' cache and/or retrieve path to Xenium Lung Adenocarcinoma example data, zipped SPEP
#' accompanied by parquet
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where tiff file can be retrieved
#' @examples
#' if (interactive()) {
#'    pa = cache_xen_luad()
#'    luad = restoreZipXenSPEP(pa)
#'    print(luad)
#'    print(slot(luad, "cellbounds_path"))
#'    view_seg(luad, xlim=c(4000,4500), ylim=c(2000,2500))
#' }
#' @export
cache_xen_luad = function(cache=BiocFileCache::BiocFileCache(), 
   url="https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/luad2.zip") {
 chk = bfcquery(cache, "BiocXenData/luad2.zip")
 n = nrow(chk)
 if (n>=1) return(chk[n,]$rpath)
 bfcadd(cache, rname=basename(url), fpath=url, action="copy", download=TRUE)
}
