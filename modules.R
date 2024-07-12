# Button to download plots (only plotly objects) as .svg file

downloadSvgButton <- function(id, label) {
  ns <- NS(id)
    downloadButton(ns("svgexport"), label = label)
}

downloadSvgPlot <- function(id, plot) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      if (!file.exists("exported_fig")) {
        dir.create("exported_fig")
      }
      output$svgexport <- downloadHandler(
        filename = function() {
              paste(ns("svgexport"), ".svg", sep="")
            },
            content = function(file) {
              showModal(modalDialog("Downloading SVG plot, please wait...", footer=NULL))
              filepath <- paste("exported_fig/", ns("svgexport"), ".svg", sep="")
              save_image(plot(), filepath)
              file.copy(filepath, file)
              removeModal()
            }
      )
  })
}
