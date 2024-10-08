% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cache_mtif.R
\name{cacheXenLuad}
\alias{cacheXenLuad}
\title{cache and/or retrieve path to Xenium Lung Adenocarcinoma example data, zipped SPEP
accompanied by parquet}
\usage{
cacheXenLuad(
  cache = BiocFileCache::BiocFileCache(),
  url = "https://mghp.osn.xsede.org/bir190004-bucket01/BiocXenData/luad_lite.zip"
)
}
\arguments{
\item{cache}{defaults to BiocFileCache::BiocFileCache()}

\item{url}{location where zip file can be retrieved}
}
\value{
path to cached resource
}
\description{
cache and/or retrieve path to Xenium Lung Adenocarcinoma example data, zipped SPEP
accompanied by parquet
}
\examples{
if (interactive()) {
  pa <- cacheXenLuad()
  luad <- restoreZipXenSPEP(pa)
  print(luad)
  print(slot(luad, "cellbounds_path"))
  viewSeg(luad, xlim = c(4000, 4500), ylim = c(2000, 2500))
}
}
