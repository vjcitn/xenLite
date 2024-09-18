
# we are deliberately avoiding basilisk here for now

#' use spatialdata bounding_box_query to confine attention
#' @param xlim extent of interest, 'horizontal' axis
#' @param ylim extent of interest, 'vertical' axis
#' @return py_run_string output, which is a python environment,
#' which will have element 'crop0' to use as python function
make_cropper = function(xlim=c(19000,22000), ylim=c(8000,8500)) {
   sda = reticulate::import("spatialdata")
str = '
import spatialdata as sda
crop0 = lambda x: sda.bounding_box_query(
    x,
    min_coordinate=[%d, %d], 
    max_coordinate=[%d, %d],
    axes=("x", "y"),
    target_coordinate_system="global"
)'
   ai = as.integer
   str = sprintf(str, ai(xlim[1]), ai(ylim[1]), ai(xlim[2]), ai(ylim[2]))
   reticulate::py_run_string(str,convert=FALSE)
}

#' produce an array from selected image in a spatialdata zarr store
#' @param zpath path to zarr store
#' @param xlim extent of interest, 'horizontal' axis
#' @param ylim extent of interest, 'vertical' axis
#' @param image_name character(1)
#' @param target_width numeric(1)
#' @return R array dimensions nx x ny x 3 suitable for use
#' with EBImage
#' @examples
#' \donttest{
#' if (dir.exists("lungdata.zarr")) {
#'   nn = rast_zarr("lungdata.zarr")
#'   if (requireNamespace("EBImage")) {
#'      ii = EBImage::Image(nn, colormode="Color")
#'      EBImage::display(ii) # to browser for zoom/pan
#'   } else message("Install EBImage to run this code.")
#' }
#' }
#' @export
rast_zarr = function(zpath, xlim=c(19000,22000), ylim=c(8000,8500),
    image_name = "he_image", target_width=800.0) {
 sda = reticulate::import("spatialdata")
 plt = reticulate::import("matplotlib.pyplot")
 sdp = reticulate::import("spatialdata_plot")
 xlim = as.double(xlim)
 ylim = as.double(ylim)
 crop0 = make_cropper(xlim, ylim)
 ndat = sda$read_zarr(zpath)
 cc = crop0$crop0(ndat)
# the following seems to add raster info to cc
 rr = sda$rasterize(cc, axes=reticulate::tuple("x", "y"), 
   min_coordinate=c(xlim[1],ylim[1]), # seems repetitious
   max_coordinate=c(xlim[2],ylim[2]), 
   target_coordinate_system="global", 
   target_width=as.double(target_width)) # width in pixels
 p1 = cc$images$pop('he_image') # should parameterize, but how to 
                               # learn available names?
 dd = p1$to_dict()
 a0 = dd["/scale0"]$to_array()
 a0n = a0$to_numpy()
 ppp = reticulate::py_to_r(a0n[0L,,,])
 aperm(1.0*ppp/255.,c(2,3,1))
}

