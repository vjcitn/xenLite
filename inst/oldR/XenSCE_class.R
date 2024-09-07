
#' define container for Xenium demo data
#' @importClassesFrom SpatialExperiment SpatialExperiment
#' @export
setClass("XenSCE", contains="SpatialExperiment", slots=c(cellbounds="DataFrame",
  transcripts="DataFrame", nucbounds="DataFrame"))

#' summarize XenSCE
#' @importFrom methods callNextMethod new slot
#' @param object instance of XenSCE
#' @export
setMethod("show", "XenSCE", function(object) {
  callNextMethod(); 
  cat("Geometry elements:\n")
  print(xdims(object)) 
} )


#' helper function for XenSCE show method, producing dimensions for
#' geometry information
#' @param x instance of XenSCE
xdims = function (x) 
{
    ans = sapply(c("transcripts", "cellbounds", "nucbounds"), 
        function(z) dim(slot(x, z)))
    ans = t(ans)
    colnames(ans) = c("nrow", "ncol")
    data.frame(ans)
}

#' method for transcript extraction
#' @param x instance of XenSCE
#' @export
setGeneric("getTranscripts", function(x) standardGeneric("getTranscripts"))
#' method for transcript extraction
#' @param x instance of XenSCE
#' @export
setMethod("getTranscripts", "XenSCE", function(x) slot(x, "transcripts"))
#' method for transcript extraction
#' @param x instance of XenSPEP
#' @export
setMethod("getTranscripts", "XenSPEP", function(x) slot(x, "txtab"))

#' method for cell boundary extraction
#' @param x instance of XenSCE
#' @export
setGeneric("getCellBoundaries", function(x) standardGeneric("getCellBoundaries"))
#' method for cell boundary extraction
#' @param x instance of XenSCE
#' @export
setMethod("getCellBoundaries", "XenSCE", function(x) slot(x, "cellbounds"))
#' method for cell boundary extraction
#' @param x instance of XenSPEP
#' @export
setMethod("getCellBoundaries", "XenSPEP", function(x) slot(x, "cbtab"))

#' method for nucleus boundary extraction
#' @param x instance of XenSCE
#' @export
setGeneric("getNucleusBoundaries", function(x) standardGeneric("getNucleusBoundaries"))
#' method for nucleus boundary extraction
#' @param x instance of XenSCE
#' @export
setMethod("getNucleusBoundaries", "XenSCE", function(x) slot(x, "nucbounds"))
#' method for nucleus boundary extraction
#' @param x instance of XenSPEP
#' @export
setMethod("getNucleusBoundaries", "XenSPEP", function(x) slot(x, "nbtab"))

chkxsce = function(object) {
 nc = ncol(object)
 if (!isTRUE(length(colnames(object))==nc)) return("colnames(object) has incorrect length")
 cc = SpatialExperiment::spatialCoords(object)
 nn = colnames(cc)[1:2]
 if (!isTRUE(all.equal(nn, c("x_centroid", "y_centroid")))) return("first two elements of spatialCoords(object) are not 'x_centroid', 'y_centroid'")
 TRUE
}

setValidity("XenSCE", method=chkxsce)
