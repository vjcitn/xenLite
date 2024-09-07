##-rw-r--r-- 1 exouser exouser 3300521324 Mar 20 21:17 transcripts.parquet
##-rw-r--r-- 1 exouser exouser   68454210 Mar 20 21:16 nucleus_boundaries.parquet
##-rw-r--r-- 1 exouser exouser   73791358 Mar 20 21:15 cell_boundaries.parquet
##-rw-r--r-- 1 exouser exouser 242459483 Apr 10 03:46 cell_feature_matrix.tar.gz
##-rw-r--r-- 1 exouser exouser 44907408 Mar 20 21:15 cells.csv.gz


xenbase = function(ent)
 sprintf("https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/%s", ent)

xenassets = c(
  "transcripts.parquet", # 3.3GB
  "nucleus_boundaries.parquet",
  "cell_boundaries.parquet",
  "cell_feature_matrix.tar.gz",
  "cells.csv.gz")

is_present = function(cache, x) {
    chk = BiocFileCache::bfcquery(cache, basename(x))
    if (nrow(chk)>0) return(chk[nrow(chk),]$rpath)
    FALSE
    }

#' return paths to cached entities after retrieving and caching them if needed
#' @import BiocFileCache
#' @note On first usage around 4GB of data, mostly parquet, will be downloaded
#' and added to cache.
#' @param cache instance of BiocFileCache::BiocFileCache()
#' @examples
#' if (interactive()) {
#'    chk = yesno::yesno("If not already cached, this function will download 4GB of files.  Proceed?")
#'    if (chk) cache_assets()
#' }
#' @export
cache_assets = function(cache=BiocFileCache::BiocFileCache()) {
  urls = xenbase(xenassets)
  pas = lapply(urls, function(src) {
      ans = is_present(cache, src)
      if (isFALSE(ans)) ans = BiocFileCache::bfcadd(cache, rname=src,
    action="copy", download=TRUE)
      ans
   })
  names(pas) = xenassets
  pas
}

# NB I don't like these below, maybe delete after August

#' helper for caching a built demo XenSCE
#' @param src a XenSCE instance
#' @param cache instance of BiocFileCache::BiocFileCache()
#' @note Cache will hold an RDS file gbm_xen_demo.rds.
#' @return zero if `gbm_xen_demo.rds` is already present
#' in cache; otherwise result of BiocFileCache::bfcadd().
#' @export
cache_demo = function(src, cache=BiocFileCache::BiocFileCache()) {
  stopifnot(inherits(src, "XenSCE"))
  td = tempdir()
  tst = is_present(cache, "gbm_xen_demo.rds")
  if (!isFALSE(tst)) return(0)
  tf = file.path(td, "gbm_xen_demo.rds")
  saveRDS(src, tf)
  BiocFileCache::bfcadd(cache, rname=tf, action="move", download=FALSE)
}
 
#' retrieve cached demo XenSCE
#' @param cache instance of BiocFileCache::BiocFileCache()
#' @export
retrieve_demo = function(cache=BiocFileCache::BiocFileCache()) { 
  tst = is_present(cache, "gbm_xen_demo.rds")
  if (isFALSE(tst)) stop("please use build_demo and cache_demo to populate cache")
  readRDS(tst)
}
