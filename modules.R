# Button to download plots as .svg file

downloadSvgButton <- function(id, label) {
  ns <- NS(id)
    downloadButton(ns("svgexport"), label = label)
}

downloadSvgPlot <- function(id, plot) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      output$svgexport <- downloadHandler(
        filename = function() {
              paste(ns("svgexport"), ".svg", sep="")
            },
            content = function(file) {
              filepath <- paste("exported_fig/", ns("svgexport"), ".svg", sep="")
              save_image(plot(), filepath)
              file.copy(filepath, file)
            }
      )
  })
}
