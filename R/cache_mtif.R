#' cache and/or retrieve path to an ome.tif file for demonstration
#' @import BiocFileCache
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where tiff file can be retrieved
#' @note The tiff file was retrieved after running SFEData::XeniumOutput("v2"),
#' and depicts a pancreas tissue sample.
#' @return path to cached resource
#' @examples
#' pa <- cacheMtif()
#' if (!requireNamespace("tiff")) stop("install tiff package to run this example")
#' x <- tiff::readTIFF(pa)
#' plot(0, xlim = c(0, 1000), ylim = c(0, 1000), xlab = " ", ylab = " ")
#' rasterImage(x * 5.5, 0, 0, 1000, 1000)
#' @export
cacheMtif <- function(
    cache = BiocFileCache::BiocFileCache(),
    url = "https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/morphology_focus_0001.ome.tif") {
  chk <- bfcquery(cache, "BiocXenData/morphology_focus_0001.ome.tif")
  n <- nrow(chk)
  if (n >= 1) {
    return(chk[n, ]$rpath)
  }
  bfcadd(cache, rname = basename(url), fpath = url, action = "copy", download = TRUE)
}

#' cache and/or retrieve path to an SFE of V1 lung demo data from 10x
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where zip file can be retrieved
#' @return path to cached resource
#' @note Lacks transcript coordinates
cacheSfeLungNtx <- function(
    cache = BiocFileCache::BiocFileCache(),
    url = "https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/sfeLung.zip") {
  chk <- bfcquery(cache, "BiocXenData/sfeLung.zip")
  n <- nrow(chk)
  if (n >= 1) {
    return(chk[n, ]$rpath)
  }
  bfcadd(cache, rname = basename(url), fpath = url, action = "copy", download = TRUE)
}

#' cache and/or retrieve path to Xenium Lung Adenocarcinoma example data, zipped SPEP
#' accompanied by parquet
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where zip file can be retrieved
#' @return path to cached resource
#' @examples
#' if (interactive()) {
#'   pa <- cacheXenLuad()
#'   luad <- restoreZipXenSPEP(pa)
#'   print(luad)
#'   print(slot(luad, "cellbounds_path"))
#'   viewSeg(luad, xlim = c(4000, 4500), ylim = c(2000, 2500))
#' }
#' @export
cacheXenLuad <- function(
    cache = BiocFileCache::BiocFileCache(),
    url = "https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/luad_lite.zip") {
  chk <- bfcquery(cache, "BiocXenData/luad_lite.zip")
  n <- nrow(chk)
  if (n >= 1) {
    return(chk[n, ]$rpath)
  }
  bfcadd(cache, rname = basename(url), fpath = url, action = "copy", download = TRUE)
}

#' counts-in-memory version of prostate 5k dataset
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where zip file can be retrieved
#' @return path to cached resource
#' @examples
#' if (interactive()) {
#'   pa <- cacheXenProstLite()
#'   prost_lite <- restoreZipXenSPEP(pa)
#'   print(prost_lite)
#' }
#' @export
cacheXenProstLite <- function(
    cache = BiocFileCache::BiocFileCache(),
    url = "https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/prost_lite.zip") {
  chk <- bfcquery(cache, "BiocXenData/prost_lite.zip")
  n <- nrow(chk)
  if (n >= 1) {
    return(chk[n, ]$rpath)
  }
  bfcadd(cache, rname = basename(url), fpath = url, action = "copy", download = TRUE)
}

#' counts-in-memory version of melanoma 5k dataset
#' @param cache defaults to BiocFileCache::BiocFileCache()
#' @param url location where zip file can be retrieved
#' @return path to cached resource
#' @examples
#' if (interactive()) {
#'   pa <- cacheXenPdmelLite()
#'   pdmel_lite <- restoreZipXenSPEP(pa)
#'   print(pdmel_lite)
#' }
#' @export
cacheXenPdmelLite <- function(
    cache = BiocFileCache::BiocFileCache(),
    url = "https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/pdmel_lite.zip") {
  chk <- bfcquery(cache, "BiocXenData/pdmel_lite.zip")
  n <- nrow(chk)
  if (n >= 1) {
    return(chk[n, ]$rpath)
  }
  bfcadd(cache, rname = basename(url), fpath = url, action = "copy", download = TRUE)
}
