setOldClass("ArrowTabular")
setOldClass("arrow_dplyr_query")
setOldClass("Table")
setClassUnion("ArrowTabOrNULL", c("Table", "ArrowTabular", "arrow_dplyr_query", "NULL"))
#library(SpatialExperiment)


#' manage SpatialExperiment with parquet references
#' @importClassesFrom SpatialExperiment SpatialExperiment
#' @importFrom methods slot<- as
#' @export
setClass("XenSPEP", contains="SpatialExperiment",  # spatial expt with parquet refs
    representation(cellbounds_path="character",
                   nucbounds_path="character",
                   tx_path="character", loaded="logical",
                   cbtab="ArrowTabOrNULL", nbtab="ArrowTabOrNULL",
                      txtab="ArrowTabOrNULL" ))

#' display aspects of XenSPEP
#' @importFrom methods callNextMethod new slot
#' @param object instance of XenSPEP
#' @return operates with cat()
#' @export
setMethod("show", "XenSPEP", function(object) {
  cat("XenSPEP instance.  SPEcomponent:\n")
  show(as(object, "SpatialExperiment"))
  cat("use spatialCoords() for cell centroids.\n")
  if (!slot(object, "loaded")) {
       cat("Geometry element paths (not loaded):\n")
       cat(sprintf(" %s\n %s\n %s\n", slot(object, "cellbounds_path"),
                   slot(object, "nucbounds_path"),
                   slot(object, "tx_path"))) 
     }
  else {
      cat("Geometry elements loaded.\n")
       }
} )

#' formal bracket definition, that leaves parquet geometry information alone.
#' @param x instance of XenSPEP
#' @param i feature selection
#' @param j cell selection
#' @param \dots passed to SpatialExperiment methods
#' @param drop logical(1)
#' @return XenSPEP instance
#' @note Gives a message and calls callNextMethod.
#' @export
setMethod("[", c("XenSPEP"), function (x, i, j, ..., drop = TRUE) {
#  if (!missing(i)) stop("method not defined")
#  if (!missing(j)) {
#     co = SpatialExperiment::spatialCoords(x)
#     rngs = apply(co, 2, range)
#     x@cbtab = x@cbtab |> dplyr::filter(vertex_x > rngs[1,1] & vertex_x < rngs[2,1] &
#           vertex_y > rngs[1,2] & vertex_y < rngs[2,2])
#     x@nbtab = x@nbtab |> dplyr::filter(vertex_x > rngs[1,1] & vertex_x < rngs[2,1] &
#           vertex_y > rngs[1,2] & vertex_y < rngs[2,2])
#     x@txtab = x@txtab |> dplyr::filter(x_location > rngs[1,1] & x_location < rngs[2,1] &
#           y_location > rngs[1,2] & y_location < rngs[2,2])
#     }
     message("Parquet geometry data untouched by subsetting.  Affects SpatialExperiment content only")
     callNextMethod()
   })
     



setValidity("XenSPEP", function(object) {
 cb = arrow::read_parquet(object@cellbounds_path)
 nb = arrow::read_parquet(object@nucbounds_path)
 tx = arrow::read_parquet(object@tx_path)
 if (!(all(c("vertex_x", "vertex_y") %in% names(cb)))) return(sprintf("'vertex_x' or 'vertex_y' absent from %s", object@cellbounds_path))
 if (!(all(c("vertex_x", "vertex_y") %in% names(nb)))) return(sprintf("'vertex_x' or 'vertex_y' absent from %s", object@nucbounds_path))
 if (!(all(c("x_location", "y_location", "z_location") %in% names(tx)))) return(sprintf("'x_location' or 'y_location' or 'z_location' absent from %s", object@nucbounds_path))
 TRUE
 })




#' helper function for XenSCE show method, producing dimensions for
#' geometry information
#' @param x instance of XenSCE
#' @return data.frame
xdims = function (x) 
{
    ans = sapply(c("transcripts", "cellbounds", "nucbounds"), 
        function(z) dim(slot(x, z)))
    ans = t(ans)
    colnames(ans) = c("nrow", "ncol")
    data.frame(ans)
}

#' method for transcript extraction
#' @param x instance of XenSPEP
#' @return reference to ingested parquet
#' @examples
#' showMethods("getTranscripts")
#' @export
setGeneric("getTranscripts", function(x) standardGeneric("getTranscripts"))

#' method for transcript extraction
#' @param x instance of XenSPEP
#' @return reference to ingested parquet
#' @examples
#' showMethods("getTranscripts")
#' @export
setMethod("getTranscripts", "XenSPEP", function(x) slot(x, "txtab"))

#' method for cell boundary extraction
#' @param x instance of XenSPEP
#' @return reference to ingested parquet
#' @examples
#' showMethods("getCellBoundaries")
#' @export
setGeneric("getCellBoundaries", function(x) standardGeneric("getCellBoundaries"))

#' method for cell boundary extraction
#' @param x instance of XenSPEP
#' @return reference to ingested parquet
#' @export
setMethod("getCellBoundaries", "XenSPEP", function(x) slot(x, "cbtab"))

#' method for nucleus boundary extraction
#' @param x instance of XenSPEP
#' @return reference to ingested parquet
#' @examples
#' showMethods("getNucleusBoundaries")
#' @export
setGeneric("getNucleusBoundaries", function(x) standardGeneric("getNucleusBoundaries"))

#' method for nucleus boundary extraction
#' @param x instance of XenSPEP
#' @return reference to ingested parquet
#' @export
setMethod("getNucleusBoundaries", "XenSPEP", function(x) slot(x, "nbtab"))

chkxspep = function(object) {
 nc = ncol(object)
 if (!isTRUE(length(colnames(object))==nc)) return("colnames(object) has incorrect length")
 cc = SpatialExperiment::spatialCoords(object)
 nn = colnames(cc)[1:2]
 if (!isTRUE(all.equal(nn, c("x_centroid", "y_centroid")))) return("first two elements of spatialCoords(object) are not 'x_centroid', 'y_centroid'")
 TRUE
}

setValidity("XenSPEP", method=chkxspep)
