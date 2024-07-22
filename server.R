`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}


# Define server logic

server <- function(input, output, session) {
  options(shiny.maxRequestSize=5000*1024^2)


# Loading file
# ================

f0 <- reactive(
 {
   if (input$dataType == 1) {
     inFile <- input$file0
     if (is.null(inFile)) return(NULL)
     datafile <- arrow::open_dataset(inFile$datapath)
   }
   else if (input$dataType == 2) {
     datafile <- arrow::open_dataset("cal1-full-long.parquet")
   }
   return(datafile)
 }
)

rtdata <- reactive({
  if (!is.null(f0())) {
    data_arrow = f0()
    rtmin = data_arrow |>
    summarize(across(rt, ~ min(.))) |>
    collect()
    rtmax = data_arrow |>
    summarize(across(rt, ~ max(.))) |>
    collect()
    rt = list(rtmin, rtmax)
    return(rt)
   }
})
mzdata <- reactive({
  if (!is.null(f0())) {
    data_arrow = f0()
    mzmin = data_arrow |>
    summarize(across(mz, ~ min(.))) |>
    collect()
    mzmax = data_arrow |>
    summarize(across(mz, ~ max(.))) |>
    collect()
    mz = list(mzmin, mzmax)
    return(mz)
   }
})
bindata <- reactive({
  if (!is.null(f0())) {
    data_arrow = f0()
    binmin = data_arrow |>
    summarize(across(bin, ~ min(.))) |>
    collect()
    binmax = data_arrow |>
    summarize(across(bin, ~ max(.))) |>
    collect()
    bin = list(binmin, binmax)
    return(bin)
   }
})
output$rtrange <- renderText({
  rt = rtdata()
   rt1 = as.numeric(rt[[1]])
   rt2 = as.numeric(rt[[2]])
   rt1 = round(rt1, digits = 2)
   rt2 = round(rt2, digits = 2)
   paste(rt1, "-", rt2)
   })

output$mzrange <- renderText({
 mz = mzdata()
  mz1 = as.numeric(mz[[1]])
  mz2 = as.numeric(mz[[2]])
  mz1 = round(mz1, digits = 1)
  mz2 = round(mz2, digits = 1)
  paste(mz1, "-", mz2)
  })

output$binrange <- renderText({
 bin = bindata()
  bin1 = as.numeric(bin[[1]])
  bin2 = as.numeric(bin[[2]])
  paste(bin1, "-", bin2)
  })

output$rawdatadim <- renderText({
 if (!is.null(f0())) {
  data = f0()
  dim(data)
}
})

 #Create a reactive list for xaxis and yaxis zoom values, set values as NULL
 reactiveList <- reactiveValues(valX1 = NULL, valX2 = NULL, valY1 = NULL, valY2 = NULL)
observe({
  req(rtdata())
  rt = rtdata()
  mz = mzdata()
  bin = bindata()
  if (input$chrom2Daxes == 1) {
  reactiveList$valX1 = as.numeric(rt[[1]])
  reactiveList$valX2 = as.numeric(rt[[2]])
  reactiveList$valY1 = as.numeric(mz[[1]])
  reactiveList$valY2 = as.numeric(mz[[2]])
  }
  if (input$chrom2Daxes == 2) {
    reactiveList$valX1 = as.numeric(bin[[1]])
    reactiveList$valX2 = as.numeric(bin[[2]])
    reactiveList$valY1 = as.numeric(rt[[1]])
    reactiveList$valY2 = as.numeric(rt[[2]])
  }
  if (input$chrom2Daxes == 3) {
    reactiveList$valX1 = as.numeric(bin[[1]])
    reactiveList$valX2 = as.numeric(bin[[2]])
    reactiveList$valY1 = as.numeric(mz[[1]])
    reactiveList$valY2 = as.numeric(mz[[2]])
  }
})
# bin data
binneddata <- reactive({
  req(reactiveList)
  data_arrow = f0()
  xmin = reactiveList$valX1
  xmax = reactiveList$valX2
  ymin = reactiveList$valY1
  ymax = reactiveList$valY2

  condensedata(data_arrow, input$energy_level_2D, xmin, xmax, ymin, ymax, input$xbin, input$ybin, selectedaxes = input$chrom2Daxes)
})

# Plots panel
# ===========

# 2D plot
# --------

full2Dplot <- reactive({
  req(binneddata())
  spreaddt = binneddata()
  plotly2D(spreaddt, selectedtype = input$chrom2Dtype, selectedaxes = input$chrom2Daxes, zmin = input$zscale[1], zmax = input$zscale[2], session)
})

output$full2Dplot = renderPlotly({
  full2Dplot()
})

downloadSvgPlot("full2DPlot", full2Dplot)


#Create a reactive function to update the reactive list every time the plotly_relayout changes
  relayout_data <- reactive({
    vals=event_data("plotly_relayout",source="2Dplot")
    if (is.null(vals$`xaxis.range[0]`)){
    } else {
      reactiveList$valX1 = vals$`xaxis.range[0]`
      reactiveList$valX2 = vals$`xaxis.range[1]`
      reactiveList$valY1 = vals$`yaxis.range[0]`
      reactiveList$valY2 = vals$`yaxis.range[1]`
      # print(reactiveList$valX1)
      # print(reactiveList$valX2)
      # print(reactiveList$valY1)
      # print(reactiveList$valY2)
    }
  })

output$xbinUI <- renderUI({
  req(reactiveList)
  relayout_data()
  if (input$chrom2Daxes == 1) {
    xbin = (reactiveList$valX2 - reactiveList$valX1)/720
  }
  if (input$chrom2Daxes == 2) {
    xbin = (reactiveList$valX2 - reactiveList$valX1)/100
  }
  if (input$chrom2Daxes == 3) {
    xbin = (reactiveList$valX2 - reactiveList$valX1)/100
  }
  freezeReactiveValue(input, "xbin")
  return(sliderInput("xbin", "bin parameter for x axis", step = xbin/10, value = xbin, min = 0, max = xbin * 10))
})

output$ybinUI <- renderUI({
  req(reactiveList)
  relayout_data()
  if (input$chrom2Daxes == 1) {
    ybin = (reactiveList$valY2 - reactiveList$valY1)/200
  }
  if (input$chrom2Daxes == 2) {
    ybin = (reactiveList$valY2 - reactiveList$valY1)/720
  }
  if (input$chrom2Daxes == 3) {
    ybin = (reactiveList$valY2 - reactiveList$valY1)/200
  }
  freezeReactiveValue(input, "ybin")
  return(sliderInput("ybin", "bin parameter for y axis", step = ybin/10, value = ybin, min = 0, max = ybin * 10))
})

output$zoomranges <- renderUI({
  str1 = paste("x axis: ", round(reactiveList$valX1, 2), "to ", round(reactiveList$valX2, 2))
  str2 = paste("y axis: ", round(reactiveList$valY1, 2), "to ", round(reactiveList$valY2, 2))
  HTML(paste(str1, str2, sep = "<br/>"))
})

## reset zoom

observeEvent(input$zoomreset, {
  req(rtdata())
  rt = rtdata()
  mz = mzdata()
  bin = bindata()
  rt[[1]]
  if (input$chrom2Daxes == 1) {
  reactiveList$valX1 = as.numeric(rt[[1]])
  reactiveList$valX2 = as.numeric(rt[[2]])
  reactiveList$valY1 = as.numeric(mz[[1]])
  reactiveList$valY2 = as.numeric(mz[[2]])
  }
  if (input$chrom2Daxes == 2) {
    reactiveList$valX1 = as.numeric(bin[[1]])
    reactiveList$valX2 = as.numeric(bin[[2]])
    reactiveList$valY1 = as.numeric(rt[[1]])
    reactiveList$valY2 = as.numeric(rt[[2]])
  }
  if (input$chrom2Daxes == 3) {
    reactiveList$valX1 = as.numeric(bin[[1]])
    reactiveList$valX2 = as.numeric(bin[[2]])
    reactiveList$valY1 = as.numeric(mz[[1]])
    reactiveList$valY2 = as.numeric(mz[[2]])
  }
})

# 3D plot
# --------

rtmz3Dplot <- reactive({
  # req(spectraValues$high)
  spreaddt = binneddata()
  spreadmatrix = as.matrix(spreaddt[,-1])

  fig <- plot_ly(
    x = as.numeric(colnames(spreaddt[,-1])),
    y = spreaddt$prepd.mz,
    z = ~spreadmatrix,

    cmin = input$zscale[1],
    cmax = input$zscale[2]
  )

  fig <- fig %>% layout(scene = list(zaxis = list(range=c(0,2000000), title="Intensity"),
                                     xaxis=list(title="Retention time (min)"),
                                     yaxis=list(title="m/z")
                                     )
                        ) %>% add_surface()

fig
})

output$rtmz3Dplot = renderPlotly({
  rtmz3Dplot()
})

downloadSvgPlot("rtmz3DPlot", rtmz3Dplot)


# EIC, TIC, BPI plots
# -------------------

output$chromCode <- renderText({
    if(input$energy_level_chrom == 1) {energy = "1"} else {energy = "2"}



    if(input$chromtype == 1) {
      paste0(
      "data_arrow = arrow::open_dataset(inFile$datapath) \n\n",
      "TIC = data_arrow |>
      filter(mslevel == ", energy, " ) |>
      arrange(rt) |>
      group_by(rt, scanid) |>
      summarise(sum_int = sum(intensity)) |>
      collect()"
      )
    } else if (input$chromtype == 2) {
      paste0(
        "data_arrow = arrow::open_dataset(inFile$datapath) \n\n",
        "BPI = data_arrow |>
          filter(mslevel == ", energy, " ) |>
          arrange(rt) |>
          group_by(rt, scanid) |>
          summarise(sum_int = max(intensity)) |>
          collect()"
        )
    } else {
      paste0(
        "data_arrow = arrow::open_dataset(inFile$datapath) \n\n",
        "# calculate ranges of m/z values with target m/z and tolerance
        lowmz = input$EICmz - input$EICtol
        highmz = input$EICmz + input$EICtol

        EIC = data_arrow |>
          filter(mslevel == ", energy, " ) |>
          arrange(rt) |>
          filter(mz > lowmz & mz < highmz) |>
          group_by(rt, scanid) |>
          summarise(sum_int = sum(intensity)) |>
          collect()"
      )
    }
})


EIC <- eventReactive(input$EICcalc, {
req(f0())
data_arrow = f0()
if (input$energy_level_chrom == 1) {energy = "1"} else {energy = "2"}

lowmz = input$EICmz - input$EICtol
highmz = input$EICmz + input$EICtol

EIC = data_arrow |>
  filter(mslevel == energy) |>
  arrange(rt) |>
  filter(mz > lowmz & mz < highmz) |>
  group_by(rt, scanid) |>
  summarise(sum_int = sum(intensity)) |>
  collect()

EIC = as.data.table(EIC)
return(EIC)
})

TIC <- eventReactive(input$TICcalc, {
req(f0())
data_arrow = f0()
if (input$energy_level_chrom == 1) {energy = "1"} else {energy = "2"}

TIC = data_arrow |>
  filter(mslevel == energy) |>
  arrange(rt) |>
  group_by(rt, scanid) |>
  summarise(sum_int = sum(intensity)) |>
  collect()

TIC = as.data.table(TIC)
return(TIC)
})

BPI <- eventReactive(input$BPIcalc, {
req(f0())
data_arrow = f0()
if (input$energy_level_chrom == 1) {energy = "1"} else {energy = "2"}

BPI = data_arrow |>
  filter(mslevel == energy) |>
  arrange(rt) |>
  group_by(rt, scanid) |>
  summarise(sum_int = max(intensity)) |>
  collect()

BPI = as.data.table(BPI)
return(BPI)
})

EICPlotlyObj <- reactive({
    req(EIC())
    EIC = EIC()
      plotly_chrom(EIC, "EIC", session)
  })

output$EICplot <- renderPlotly({
  EICPlotlyObj()
  EIC = EIC()
  plotly_chrom(EIC, "EIC", session)
})

downloadSvgPlot("EICPlot", EICPlotlyObj)

observeEvent(EIC(), {
  EIC = EIC()
  output$EICplot2 <- renderRbokeh({
    figure() %>% ly_lines(x = rt, y = sum_int, data = EIC) %>%
    ly_points(x = rt, y = sum_int, data = EIC, lname = "rt", hover=list("rt"), size=5, alpha = 0) %>%
    tool_crosshair() %>%
    tool_hover(shiny_callback("hover_info"), "rt") %>%
      tool_selection(shiny_callback("sel_info"), "rt") %>%
      tool_box_select(shiny_callback("tbsel_info"), "rt")
})
})

observeEvent(TIC(), {
  TIC = TIC()
  output$TICplot <- renderPlotly({
    plotly_chrom(TIC, "TIC", session)
  })
})

TICPlotlyObj <- reactive({
    req(TIC())
    TIC = TIC()
      plotly_chrom(TIC, "TIC", session)
  })

downloadSvgPlot("TICPlot", TICPlotlyObj)

observeEvent(BPI(), {
  BPI = BPI()
  output$BPIplot <- renderPlotly({
    plotly_chrom(BPI, "BPI", session)
  })
})

BPIPlotlyObj <- reactive({
    req(BPI())
    BPI = BPI()
      plotly_chrom(BPI, "BPI", session)
  })

downloadSvgPlot("BPIPlot", BPIPlotlyObj)

output$selection_text <- reactive({
   si <- input$tbsel_info
   if(!is.null(si)) {
     paste("index:", paste(si, collapse = ", "))
     print(si)
   } else {
     "waiting for selection event (click point(s) or use box select tool to trigger)..."
   }
 })


 # display MS spectra plots from chromatogram selection
 # ----------------------------------------------------

chrom.event.data.clicked <- reactive({
  # req(EIC())
  if (input$chromtype == 1) {
    p = event_data("plotly_click", source = "TICselect")
  } else if (input$chromtype == 2) {
    p = event_data("plotly_click", source = "BPIselect")
  } else {
    p = event_data("plotly_click", source = "EICselect")
  }
  return(p)
})
chrom.event.data.selected <- reactive({
  # req(EIC())
  if (input$chromtype == 1) {
    p = event_data("plotly_selected", source = "TICselect")
  } else if (input$chromtype == 2) {
    p = event_data("plotly_selected", source = "BPIselect")
  } else {
    p = event_data("plotly_selected", source = "EICselect")
  }
  return(p)
})
# store selected rts in reactive values object
chromSelectedRt <- reactiveValues()

chromclickobserver <- observeEvent(chrom.event.data.clicked(), suspended = F, {
  event <- chrom.event.data.clicked()
  s <- as.numeric(event$pointNumber) + 1
  selectedRt <- event$key
  chromSelectedRt$scanids = as.numeric(selectedRt)
})
chromselectobserver <-  observeEvent(chrom.event.data.selected(), suspended = F, {
  event <- chrom.event.data.selected()
  s <- as.numeric(event$pointNumber) + 1
  selectedRts <- event$key
  chromSelectedRt$scanids = as.numeric(selectedRts)
})

dt_bin_min = reactive({
  #req(input$dtscanvalue)
  if (input$combine_bins) {
    dtbinvalue = as.numeric(input$dt_bin_selection)
    dt_bin_min = dtbinvalue - input$dt_tolerance
    if (dt_bin_min <= 1) {
      dt_bin_min = 1
    }
    return(dt_bin_min)
  } else {
    return(1)
  }
  })

dt_bin_max = reactive({
  #req(input$dtscanvalue)
  if (input$combine_bins) {
    dtbinvalue = as.numeric(input$dt_bin_selection)
    dt_bin_max = dtbinvalue + input$dt_tolerance
    if (dt_bin_max >= 200) {
      dt_bin_max = 200
    }
    return(dt_bin_max)
  } else {
    return(200)
  }
  })

observe({
   #req(input$dtscanvalue)
  dtbinvalue = input$dt_bin_selection
  req(input$dt_tolerance)
  updateSliderInput(session, "dt_bin_selection",
    min = dt_bin_min(),
    max = dt_bin_max(),
    value = dtbinvalue)
})

spectrumLowData <- reactive({
  req(chromSelectedRt$scanids)
  scanids <- chromSelectedRt$scanids
  # print(scanids)
  data_arrow = f0()
  MSlow = data_arrow |>
  filter(mslevel == "1") |>
  filter(scanid %in% !!scanids) |>
  group_by(mz, bin) |>
  summarise(sum_int = sum(intensity)) |>
  collect()
 #reorder to plot correctly
  MSlow = MSlow[order(MSlow$mz),]
})

spectrumHighData <- reactive({
  req(chromSelectedRt$scanids)
  scanids <- chromSelectedRt$scanids
  data_arrow = f0()
  MShigh = data_arrow |>
  filter(mslevel == "2") |>
  filter(scanid %in% !!scanids) |>
  group_by(mz, bin) |>
  summarise(sum_int = sum(intensity)) |>
  collect()
 #reorder to plot correctly
  MShigh = MShigh[order(MShigh$mz),]
})

spectrumLowPlotObj <- reactive({
  req(chromSelectedRt$scanids)

  MSlow = spectrumLowData()
  if (input$IMS_checkbox) {
    if (input$combine_bins) {
      bins_array = seq(dt_bin_min(), dt_bin_max(), 1)
    } else {
      bins_array = as.vector(input$dt_bin_selection)
    }
    MSlow = MSlow |>
    filter(bin %in% bins_array) |>
    group_by(mz) |>
    summarise(sum_int = sum(sum_int)) |>
    collect()
  } else {
    MSlow = MSlow |>
    group_by(mz) |>
    summarise(sum_int = sum(sum_int)) |>
    collect()
  }
  #reorder to plot correctly
  MSlow = MSlow[order(MSlow$mz),]

  plot_ly(data = MSlow, x = ~mz, y = ~sum_int, type="scatter", mode = "line") %>%
    layout(
           xaxis = list(title = "m/z"),
           yaxis = list(title = "Intensity"))
})

output$spectrumLowPlot <- renderPlotly({
  spectrumLowPlotObj()
})

downloadSvgPlot("spectrumLowPlot", spectrumLowPlotObj)


spectrumHighPlotObj <- reactive({
  req(chromSelectedRt$scanids)

  MShigh = spectrumHighData()
  if (input$IMS_checkbox) {
    if (input$combine_bins) {
      bins_array = seq(dt_bin_min(), dt_bin_max(), 1)
    } else {
      bins_array = as.vector(input$dt_bin_selection)
    }
    MShigh = MShigh |>
    filter(bin %in% bins_array) |>
    group_by(mz) |>
    summarise(sum_int = sum(sum_int)) |>
    collect()
  } else {
    MShigh = MShigh |>
    group_by(mz) |>
    summarise(sum_int = sum(sum_int)) |>
    collect()
  }
  #reorder to plot correctly
  MShigh = MShigh[order(MShigh$mz),]

  plot_ly(data = MShigh, x = ~mz, y = ~sum_int, type="scatter", mode = "line") %>%
    layout(
           xaxis = list(title = "m/z"),
           yaxis = list(title = "Intensity"))
})

output$spectrumHighPlot <- renderPlotly({
  spectrumHighPlotObj()
})

downloadSvgPlot("spectrumHighPlot", spectrumHighPlotObj)


# 1D mobility plot
# ----------------

mobility1Dplot <- reactive({
  req(chromSelectedRt$scanids)
  scanids <- chromSelectedRt$scanids
  data_arrow = f0()
  if (input$energy_level_chrom == 1) {energy = "1"} else {energy = "2"}

  xmin = reactiveList$valX1
  xmax = reactiveList$valX2
  ymin = reactiveList$valY1
  ymax = reactiveList$valY2

  IMStrace1D = data_arrow |>
    to_duckdb() |>
    filter(mslevel == energy) |>
    filter(scanid %in% !!scanids) |>
    group_by(bin) |>
    summarise(sum_int = sum(intensity)) |>
    collect()

  IMStrace1D = IMStrace1D[order(IMStrace1D$bin),]

  plotly_imstrace(IMStrace1D, type = "IMStrace", session)
})

output$mobility1Dplot = renderPlotly({
  mobility1Dplot()
})

downloadSvgPlot("mobility1DPlot", mobility1Dplot)

# 2D mobility plot
# ----------------

mobility2Dplot <- reactive({
  req(chromSelectedRt$scanids)
  scanids <- chromSelectedRt$scanids
  data_arrow = f0()
  if (input$energy_level_chrom == 1) {energy = "1"} else {energy = "2"}

  mz = mzdata()
  bin = bindata()
  xmin = as.numeric(bin[[1]])
  xmax = as.numeric(bin[[2]])
  ymin = as.numeric(mz[[1]])
  ymax = as.numeric(mz[[2]])

  spreaddt = condensedataIMStrace(data_arrow, energy, xmin, xmax, ymin, ymax, 0.0001, 0.0001, scanids)

  plotly2D(spreaddt, selectedtype = 2, selectedaxes = 3, zmin = 0, zmax = max(spreaddt[,-1]), session)
})

output$mobility2Dplot = renderPlotly({
  mobility2Dplot()
})

downloadSvgPlot("mobility2DPlot", mobility2Dplot)


} #end of server function
