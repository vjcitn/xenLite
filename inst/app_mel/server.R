library(shiny)
library(ParquetDataFrame)
library(xenLite)
library(SpatialExperiment)
library(ggplot2)


if (!exists("xspep"))    xspep = restoreZipXenSPEP("pdmel_lite.zip")
what = "pdMel"

  stopifnot(is(xspep, "XenSPEP"))
  rngs = apply(spatialCoords(xspep),2,range)
  xmid = mean(rngs[,"x_centroid"])
  ymid = mean(rngs[,"y_centroid"])
  
  
  server = function(input, output) {
   output$cells = renderPlot({
    view_seg(xspep, c(input$xstart, input$xstart+input$width), c(input$ystart, input$ystart+input$width))
    })
   output$cellcount = renderUI({
     pp = prepit()
     nc = length(unique(pp$bounds$cell_id))
     helpText(sprintf("Number of cells: %d", nc))
     })
   prepit = reactive({
     ggprep_seg(xspep, xlim=c(input$xstart, input$xstart+input$width), 
          ylim=c(input$ystart, input$ystart+input$width))
    })

   output$gg1 = renderPlot({
     prep = prepit() #ggprep_seg(xspep, xlim=c(input$xstart, input$xstart+input$width), 
          #ylim=c(input$ystart, input$ystart+input$width))
     txd = prep$txdata |> dplyr::filter(feature_name==input$gene) |> dplyr::collect() |> as.data.frame()
     txd$sizq = factor(input$gene)
     if (nrow(txd)==0 | !input$showg) plot_xen_ggprep(prep)
     else plot_xen_ggprep(prep) + geom_point(data=txd, aes(x=x_location, y=y_location), size=.5, colour="black")
     })
   output$map = renderPlot({
    plot(SpatialExperiment::spatialCoords(xspep), pch=".", cex=.5)
    pmat = rbind(c(input$xstart, input$ystart), c(input$xstart, input$ystart+input$width),
             c(input$xstart+input$width, input$ystart+input$width), c(input$xstart+input$width, input$ystart))
    polygon(pmat, col="white", lwd=2, density=0)
    })
   output$showx = renderPrint({
    print(xspep)
    })
   output$topbox = renderUI({
      helpText(sprintf("xenLite package version %s; interactive view of TENx Xenium %s", packageVersion("xenLite"),
       what))
    })
   output$secbox = renderUI({
      helpText("Use 'map' tab to see complete cell layout.  White rectangle is region of interest.  Position and resize the region using sliders.  Use checkbox and text field below to visualize positions of transcripts for a selected gene.")
    })
  }
  


