#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button in Rstudio.
#
# Find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com/
#


library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinythemes)
library(shinyjqui)
library(shinycssloaders)
library(shinyWidgets)
library(plotly)
library(rbokeh)
library(DT)
library(data.table)
library(arrow)
library(bigvis)
library(tidyr)
library(sortable)
library(dplyr)
library(dtplyr)
options(shiny.port = 5424)

source("plots.R")
source("modules.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Parquet MS data visualizer"),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("Input data", tabName = "inputdata", icon = icon("file-import")),
                        menuItem("Plots", tabName = "plots", icon = icon("chart-bar"),
                                        menuSubItem("2D Plot", tabName = "2Dplot", icon = icon("chart-bar")),
                                        menuSubItem("3D Plot", tabName = "3Dplot", icon = icon("chart-bar")),
                                        menuSubItem("TIC/BPI/EIC", tabName = "chroms", icon = icon("chart-bar"))
                          )
                      )
                    ),

                    ## Body content
                    dashboardBody(
                      tabItems(
                      # second tab: importation of parquet MS files
                     tabItem(tabName = "inputdata",
                     fluidRow(
                       box(
                         title = "Upload parquet MS file",
                         width = 12,
                         #Color of the head of the box (primary=Blue,success=Green,info=Blue,warning=Orange,danger=Red)
                         status = "primary",
                         # solidHeader = TRUE,
                         collapsible = TRUE,
                         "Input file must be a parquet file from Unifi data containing 4 columns:", br(),
                         tags$ul(
                                             tags$li("An 'rt' column containing retention times"),
                                             tags$li("A 'masses' column containing arrays of masses at each retention time"),
                                             tags$li("An 'intensities' column containing arrays of intensities at each retention time"),
                                             tags$li("A 'scan_size' column containing arrays of scan sizes at each retention time")
                         ),
                                                                                                                                                                                                                                     "Choose either an external file or a demo file:",br(),
                                                                                                                                                                                                                                     selectInput("dataType", label = "Data type",
                                                                                                                                                                                                                                                 choices = list("Parquet file" = 1, "Demo data" = 2),
                                                                                                                                                                                                                                                 selected = 1),
                                                                                                                                                                                                                                     conditionalPanel("input.dataType == 1",
                                                                                                                                                                                                                                                      fileInput("file0", "Choose Parquet file")
                                                                                                                                                                                                                                                      )

                                        ),
                    box(
                                        width = 12,
                                        collapsible = TRUE,
                                        "Retention time range:", textOutput("rtrange", inline = T),
                                        br(),
                                        "m/z range:", textOutput("mzrange", inline = T),
                                        br(),
                                        "Bin (drift time) range:", textOutput("binrange", inline = T),
                                        br(),
                                        "Data dimensions:", textOutput("rawdatadim", inline = T)
                          )

                     )
                     ),
    # tab-content: 2D plot

    tabItem(tabName = "2Dplot",
            fluidRow(
              box(width = 3,
                  title = "Bin parameters",
                  status = "primary",
                  collapsible = TRUE,
                  selectInput("energy_level_2D", label = "Energy level (HDMSe)",
                                    choices = list("Low" = 1, "High" = 2)
                                    ),
                  selectInput("chrom2Dtype", label = "Graph type",
                                    choices = list("heatmap" = 1, "contour" = 2)
                                    ),
                  selectInput("chrom2Daxes", label = "Graph axes",
                                    choices = list("RT vs m/z" = 1, "DT vs RT" = 2, "DT vs m/z" = 3)
                                    ),
                  # sliderInput("ybin", "bin parameter for y axis", min = 0, max = 10, value = 2, step = 0.1, round = 0),
                  # sliderInput("xbin", "bin parameter for x axis", min = 0, max = 1, value = 0.1, step = 0.01, round = 0),
                  p("Decrease bin values for higher mass precision (warning: will slow down app), or zoom on the plot"),
                  uiOutput("xbinUI"),
                  uiOutput("ybinUI"),
                  sliderInput("zscale", "range of intensities", min = 0, max = 50000, value = c(1000,10000), step = 10, round = 0),
                  htmlOutput("zoomranges"),
                  actionButton("zoomreset", "Reset zoom")
              ),
              box(
	            title = "2D chromatogram",
	            width = 9,
	            height = "100%",
	            status = "primary",

												                             jqui_resizable(plotlyOutput("full2Dplot", height="100%")),
												                             downloadSvgButton("full2DPlot", "Download plot as svg (vector format)")
                                                      )
                                                  )
                                              ),
		# tab-content: 3D plot

        tabItem(tabName = "3Dplot",
    		        fluidRow(
    							column(
    						       width = 12,
                               box(
                                 title = "3D chromatogram",
                                 width = 12,
                                 status = "primary",
    							 collapsible = TRUE,
                                 jqui_resizable(plotlyOutput("rtmz3Dplot", height="100%")),
                                 downloadSvgButton("rtmz3DPlot", "Download plot as svg (vector format)")
                                   )

    												)
                               ) # end fluidRow
				),
                # tab-content: EIC plot

                tabItem(tabName = "chroms",
                        fluidRow(
                column(
                    width = 3,
                          box(width = 12,
                              title = "Parameters",
                               status = "primary",
                               collapsible = TRUE,
                               selectInput("energy_level_chrom", label = "Energy level (HDMSe)",
                               choices = list("Low" = 1, "High" = 2)
                               ),
                               selectInput("chromtype", label = "Select chromatogram type",
                              choices = list("TIC" = 1, "BPI" = 2, "EIC" = 3),
                              selected = 1),
                              selectInput("plottype", label = "Select plot type",
                             choices = list("Plotly" = 1, "Bokeh" = 2),
                             selected = 1),
                              conditionalPanel("input.chromtype == 3",
                                  numericInput("EICmz", "Enter m/z value to extract", 216.1016),
                                  sliderInput("EICtol", label = h3("m/z value tolerance (±)"), min = 0, max = 5, step = 0.1, value = 0.1),
                                  actionButton("EICcalc", "Load")
                             ),
                             conditionalPanel("input.chromtype == 1",
                                 actionButton("TICcalc", "Load")
                            ),
                            conditionalPanel("input.chromtype == 2",
                                actionButton("BPIcalc", "Load")
                           ),
                            hr(),
                            h4("Mass spectra filtering"),
                            checkboxInput("IMS_checkbox", label = "Filter with IMS", value = FALSE),
                            checkboxInput("combine_bins" , label = ' Combine several DT bins', value = FALSE ),
                            sliderInput("dt_bin_selection", label = "DT bin selection", min = 1, max = 200, value = 1),
                            sliderInput("dt_tolerance", label = "DT tolerance (number of bins to combine)", min = 1, max = 200, value = 1)


                        ),
                         box(width = 12,
                             title = "code",
                             status = "primary",
                             collapsible = TRUE,
                             verbatimTextOutput("chromCode")
                         )
                          ),
                                    column(
                                           width = 9,
                           box(
                             title = "Chromatogram",
                             width = 12,
                             status = "primary",
                             collapsible = TRUE,
                             conditionalPanel("input.chromtype == 1",
                             conditionalPanel("input.plottype == 1",
                                jqui_resizable(plotlyOutput("TICplot", height="100%")),
                                downloadSvgButton("TICPlot", "Download plot as svg (vector format)")
                            ),
                            conditionalPanel("input.plottype == 2",
                                jqui_resizable(rbokehOutput("TICplot2", height="500px", width="100%")),
                                downloadSvgButton("TICPlot2", "Download plot as svg (vector format)")
                            )
                            ),
                            conditionalPanel("input.chromtype == 2",
                                conditionalPanel("input.plottype == 1",
                                    jqui_resizable(plotlyOutput("BPIplot", height="100%")),
                                    downloadSvgButton("BPIPlot", "Download plot as svg (vector format)")
                                ),
                                conditionalPanel("input.plottype == 2",
                                    jqui_resizable(rbokehOutput("BPIplot2", height = "500px", width="100%"))
                           )
                            ),
                            conditionalPanel("input.chromtype == 3",
                                conditionalPanel("input.plottype == 1",
                                    jqui_resizable(plotlyOutput("EICplot", height="100%")),
                                    downloadSvgButton("EICPlot", "Download plot as svg (vector format)")
                                ),
                                conditionalPanel("input.plottype == 2",
                                    textOutput("selection_text"),
                                    jqui_resizable(rbokehOutput("EICplot2", height = "500px", width="100%"))
                           )
                            ),
       ),
       box(
                           title = "Spectra",
                          width = 12,
                          status = "primary",
                          collapsible = TRUE,
                          h4("Low energy"),
                          jqui_resizable(plotlyOutput("spectrumLowPlot", height="100%")),
                          downloadSvgButton("spectrumLowPlot", "Download plot as svg (vector format)"),
                          h4("High energy"),
                          jqui_resizable(plotlyOutput("spectrumHighPlot", height="100%")),
                          downloadSvgButton("spectrumHighPlot", "Download plot as svg (vector format)")
                      ),
      box(
        title = "Mobility traces (low energy)",
        width = 12,
        height = "100%",
        status = "primary",
        collapsible = TRUE,
        h4("1D"),

         jqui_resizable(plotlyOutput("mobility1Dplot", height="100%")),
         downloadSvgButton("mobility1DPlot", "Download plot as svg (vector format)"),
         br(),
         hr(),
        h4("2D"),

         jqui_resizable(plotlyOutput("mobility2Dplot", height="100%")),
         downloadSvgButton("mobility2DPlot", "Download plot as svg (vector format)")
                                              )

                                                                                )
                                                           ) # end fluidRow
                                                )

								) #end tabItems
  ) #end dashboardBody
                    ) #end dashboardPage
