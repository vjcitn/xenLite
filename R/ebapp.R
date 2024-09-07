
#' simple app to explore an image
#' @import EBImage
#' @import shiny
#' @param simple logical(1) if TRUE, use a cached tiff for illustration
#' @note Navigate file input control to location of tiffs
#' @examples
#' if (interactive()) demoapp(simple=TRUE)
#' @export
demoapp = function(simple=FALSE) {
  ui = fluidPage(
   sidebarLayout(
    sidebarPanel(
     helpText("EBImage explorer"),
     fileInput("inimg", "file"),
     numericInput("scalefactor", "multfac", min=1, max=50, value=10,step=1),
     sliderInput("blursig", "sigma for blur", min=1, max=100, value=50),
     helpText("vertical or horizontal reflection:"),
     checkboxInput("flipV", "flipV", value=FALSE),
     checkboxInput("flipH", "flipH", value=FALSE)
    ),
    mainPanel(
     tabsetPanel(
      tabPanel("basic", plotOutput("basic")),
      tabPanel("blur", plotOutput("blurred")),
      tabPanel("clip", uiOutput("slider1"), uiOutput("slider2"),
          plotOutput("clipped")),
      tabPanel("about", verbatimTextOutput("vers"))
      )
     )
    )
   )
  
  server=function(input, output) {
   getImg = reactive({
    if (simple) dat = cache_mtif()
    else {
      validate(need(nchar(input$inimg)>0, "pick img"))
      dat = input$inimg$datapath 
      }
    ans = EBImage::readImage( dat )
    if (input$flipV) ans = flip(ans)
    if (input$flipH) ans = flop(ans)
    ans
    })
   output$basic = renderPlot({
    img = getImg()
    display(img*input$scalefactor, method="raster")
    })
   output$blurred = renderPlot({
    img = getImg()
    display( gblur(img*input$scalefactor, sigma=input$blursig), method="raster")
    })
   output$slider1 = renderUI({
    img = getImg()
    d = dim(img)
    maxx = d[1]
    maxy = d[2]
    sliderInput("xsel", "xlim", min=1, max=maxx, value=c(1, maxx))
    })
   output$slider2 = renderUI({
    img = getImg()
    d = dim(img)
    maxx = d[1]
    maxy = d[2]
    sliderInput("ysel", "ylim", min=1, max=maxy, value=c(1, maxy))
    })
   output$clipped = renderPlot({
    img = getImg()
    validate(need(length(input$xsel)>0, "use slider"))
    display( input$scalefactor*img[ seq(input$xsel[1], input$xsel[2]), seq(input$ysel[1], input$ysel[2]) ],
      method="raster" )
    })
   }
  
  runApp(list(ui=ui, server=server))
}
   
