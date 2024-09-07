
#' build a XenSCE instance for GBM demo data from TENx
#' @import SingleCellExperiment ParquetDataFrame S4Vectors
#' @importFrom utils read.csv read.delim untar
#' @importFrom SummarizedExperiment rowData<- colData<-
#' @rawNamespace import(Matrix, except=c(unname, expand))
#' @importClassesFrom SpatialExperiment SpatialExperiment
#' @note This can take some time to run, especially if no resources
#' have been cached yet.  The data are derived from
#' `https://www.10xgenomics.com/datasets/ffpe-human-brain-cancer-data-with-human-immuno-oncology-profiling-panel-and-custom-add-on-1-standard`.
#' @examples
#' if (interactive()) {
#'   gbmdemo = build_demo()
#'   gbmdemo
#' }
#' @export
build_demo = function() {
 paths = cache_assets()
 mat.tgzpath = paths[["cell_feature_matrix.tar.gz"]]
 td = tempdir()
 untar(mat.tgzpath, exdir=td)
 newp = dir(file.path(td, "cell_feature_matrix"), full.names=TRUE)
 matpa = grep("mtx", newp, value=TRUE)
 coumat = Matrix::readMM(matpa)
 barpa = grep("barco", newp, value=TRUE)
 bardf = read.delim(barpa, sep="\t", header=FALSE) # V1 needed
 feapa = grep("features", newp, value=TRUE)
 feadf = read.delim(feapa, sep="\t", header=FALSE) # V1 needed
 rownames(coumat) = feadf$V1
 colnames(coumat) = bardf$V1
 colnames(feadf) = c("ensid", "symbol", "type")
 alist = SimpleList(counts=coumat)
 sce = SingleCellExperiment(assays=alist)
 cellmeta = read.csv(paths[["cells.csv.gz"]])
 colData(sce) = DataFrame(cellmeta)
 rowData(sce) = DataFrame(feadf)
 tppath = paths[["transcripts.parquet"]]
 tx = ParquetDataFrame(tppath)
 cbpath = paths[["cell_boundaries.parquet"]]
 cb = ParquetDataFrame(cbpath)
 nbpath = paths[["nucleus_boundaries.parquet"]]
 nb = ParquetDataFrame(nbpath)
 spe = as(sce, "SpatialExperiment")
 SpatialExperiment::spatialCoords(spe) = data.matrix(
        as.data.frame(colData(spe)[,c("x_centroid", "y_centroid")]))
 colnames(spe) = spe$cell_id
 new("XenSCE", spe, transcripts=tx, cellbounds=cb,
    nucbounds=nb)
}
 
 

