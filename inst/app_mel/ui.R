library(shiny)
library(xenLite)
library(SpatialExperiment)

if (!exists("xspep"))    xspep = restoreZipXenSPEP("pdmel_lite.zip")

what = "PDMel"

  stopifnot(is(xspep, "XenSPEP"))
  rngs = apply(spatialCoords(xspep),2,function(x)round(range(x),0))
  xmid = round(mean(rngs[,"x_centroid"]),0)
  ymid = round(mean(rngs[,"y_centroid"]),0)
  
  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     uiOutput("topbox"),
     uiOutput("secbox"),
     sliderInput("xstart", "xstart", min=rngs[1,"x_centroid"], max=rngs[2,"x_centroid"], step=50, value=7100),
     sliderInput("ystart", "ystart", min=rngs[1,"y_centroid"], max=rngs[2,"y_centroid"], step=50, value=600),
     sliderInput("width", "width", min=200, max=3000, step=100, value=1700),
     checkboxInput("showg", "show tx", value=TRUE),
     uiOutput("cellcount"),
     selectInput("gene", "gene", choices=sort(rowData(xspep)$Symbol), selected="FBL")
#     actionButton("go", "go", class="btn-success")
     ),
    mainPanel(
     tabsetPanel(
      tabPanel("cells", plotOutput("gg1", height="600px", width="600px")),
      tabPanel("map", plotOutput("map", height="600px", width="600px")),
      tabPanel("about", verbatimTextOutput("showx"))
     )
    )
   )
  )
