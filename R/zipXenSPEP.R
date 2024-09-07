#' serialize the collection of XenSPEP and parquet with zip
#' @importFrom utils zip
#' @importFrom methods is
#' @param xsp instance of XenSPEP with geometry loaded
#' @param targetfile character(1) destination of zip process
#' @note a .rds and three parquet files are zipped together for restoration
#' by `restoreZipXenSPEP`.  The outcome is `paste0(targetfile, ".zip")`.
#' @export
zipXenSPEP = function(xsp, targetfile) {
  stopifnot(is(xsp, "XenSPEP"))
  stopifnot(isTRUE(slot(xsp, "loaded")))
  pas = unlist(lapply(c("cellbounds_path", "nucbounds_path", "tx_path"),
      function(x) slot(xsp, x)))
  rdstarg = paste0(basename(tempfile()), ".rds")
  saveRDS(xsp, file=rdstarg, compress="xz")
  zip(targetfile, c(rdstarg, pas))
}

#' use unzip, readRDS, and loadGeometry to restore a XenSPEP
#' @param zipf character(1) path to zip file created with `zipXenSPEP`
#' @param exdir character(1) defaults to tempdir(), where contents are unpacked
#' @return instance of XenSPEP
#' @note Session folder position will change with setwd(), on.exit ensures return to 
#' position when started.
#' @export
restoreZipXenSPEP = function(zipf, exdir=tempdir()) {
  ini = getwd()
  on.exit(setwd(ini))
  fns = unzip(zipf, list=TRUE)
  toread = grep("rds$", fns$Name, value=TRUE)
  if (file.exists(file.path(exdir, "transcripts.parquet"))) warning("transcripts.parquet etc. will be overwritten")
  unzip(zipf, exdir=exdir)
  ans = readRDS(file.path(exdir, toread))
  setwd(exdir)
  ans = loadGeometry(ans)
  ans = reset_parq_paths(ans, base=exdir)
  ans
}
