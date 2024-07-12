condensedata <- function(data_arrow, energy_level, xmin, xmax, ymin, ymax, xbin, ybin, selectedaxes) {
  if (selectedaxes == 1) {xaxis <- "rt"; yaxis = "mz"}
  if (selectedaxes == 2) {xaxis <- "bin"; yaxis = "rt"}
  if (selectedaxes == 3) {xaxis <- "bin"; yaxis = "mz"}
  if (energy_level == 1) {energy = "Low"} else {energy = "High"}
  xbinned = as.symbol(glue::glue('{xaxis}_binned'))
  ybinned = as.symbol(glue::glue('{yaxis}_binned'))
  xaxis = as.symbol(xaxis)
  yaxis = as.symbol(yaxis)

  binneddata = data_arrow |>
  to_duckdb() |>
  filter(energy_level == energy) |>
  filter(xaxis > xmin & xaxis < xmax) |>
  filter(yaxis > ymin & yaxis < ymax) |>
    mutate("{xbinned}" := floor(xaxis/xbin)*xbin) |>
    mutate("{ybinned}" := floor(yaxis/ybin)*ybin) |>
    group_by(xbinned, ybinned) |>
    summarise(sum_int_binned = sum(intensities)) |>
    collect()
  # spread to get matrix
  binneddata = as.data.table(binneddata)
  xbinned2 = as.character(xbinned)
  ybinned2 = as.character(ybinned)
  binneddata = setorderv(binneddata, c(xbinned2, ybinned2))
  print(dim(binneddata))

  spreaddt = binneddata |> pivot_wider(names_from = xbinned, values_from = sum_int_binned)
  spreaddt = setorderv(spreaddt, ybinned2)
  setnafill(spreaddt, fill = 0)

  return(spreaddt)
}

condensedataIMStrace <- function(data_arrow, energy_level, xmin, xmax, ymin, ymax, xbin, ybin, scanids) {

  IMStrace = data_arrow |>
    to_duckdb() |>
    filter(energy_level == !!energy_level) |>
    filter(scanid %in% !!scanids) |>
    filter(bin > xmin & bin < xmax) |>
    filter(mz > ymin & mz < ymax) |>
    group_by(bin, mz) |>
    summarise(sum_int = sum(intensities)) |>
    mutate(mz_binned = floor(mz/ybin)*ybin) |>
    group_by(bin, mz_binned) |>
    summarise(sum_int_binned = sum(sum_int)) |>
    collect()

  IMStrace = IMStrace[order(IMStrace$bin),]
  # spread to get matrix
  spreadIMS = IMStrace |> pivot_wider(names_from = bin, values_from = sum_int_binned)
  spreadIMS = spreadIMS[order(spreadIMS$mz_binned),]
  setnafill(spreadIMS, fill = 0)

  return(spreadIMS)
}

plotly2D <- function(spreaddt, selectedtype, selectedaxes, zmin, zmax, session) {

  if (selectedtype == 1) {chrom2Dtype = "heatmap"} else {chrom2Dtype = "contour"}
  if (selectedaxes == 1) {xaxis <- "rt"; yaxis = "mz"; xaxistitle = "Retention Time (min)"; yaxistitle = "m/z"}
  if (selectedaxes == 2) {xaxis <- "bin"; yaxis = "rt"; xaxistitle = "Drift Time (bin)"; yaxistitle = "Retention Time (min)"}
  if (selectedaxes == 3) {xaxis <- "bin"; yaxis = "mz"; xaxistitle = "Drift Time (bin)"; yaxistitle = "m/z"}

  spreadmatrix = as.matrix(spreaddt[,-1])
  yvalues = spreaddt[[glue::glue("{yaxis}_binned")]]
  fig = plot_ly(
    x = as.numeric(colnames(spreaddt[,-1])),
    y = yvalues,
    z = spreadmatrix,

    type = chrom2Dtype,
   zmin = zmin,
   zmax = zmax,
   source = "2Dplot"
 ) %>% event_register("plotly_relayout")
 fig <- layout(fig,
                 xaxis=list(title=xaxistitle
                           ),
                 yaxis = list(title=yaxistitle
                 ))
 return(fig)

}


plotly3D <- function(selecteddata, threshold, bubblesize, session) {

  # selectedsampledata <- sample[, c("RetentionTime", "m/z", "DriftTime", noquote(selectedsample)), with=F]
  # selectedsampledata <- selectedsampledata[apply(selectedsampledata[,noquote(selectedsample), with=F], MARGIN = 1, function(x) any(x > threshold)), ]
  # datastack <- melt(selectedsampledata, id=c("RetentionTime","m/z","DriftTime"))

  # intensities <- selectedsampledata[, get(noquote(selectedsample))]
  tmax <- max(selecteddata[,value])
  updateSliderInput(session, "intensitythreshold", step = 1, min = 0, max = tmax)

p <- plot_ly(selecteddata, type='scatter3d', mode='markers', x = ~rt, y = ~mz, z = ~bin, split = ~variable, size = ~value, colors = '#4AC6B7', key = ~PrimaryId, source = "rawplotselect",
             marker = list(symbol = 'circle', sizemode = 'diameter'),
             sizes = bubblesize,
             text = paste("RT (min):", selecteddata[,RetentionTime], "; m/z:", selecteddata[,mz], "; Drift Time (ms):", selecteddata[,DriftTime], "; intensity:", selecteddata[,value])
             ) %>%
  layout(
         scene = list(xaxis = list(title = 'RT (min)',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwidth = 2),
                      yaxis = list(title = 'm/z',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2),
                      zaxis = list(title = 'Drift Time (ms)',
                                   gridcolor = 'rgb(255, 255, 255)',
                                   zerolinewidth = 1,
                                   ticklen = 5,
                                   gridwith = 2)
                   ),
                   legend = list(x=0, y=-0.3)
  )
return(p)
}


plotly_chrom <- function(data, type, session) {

  sourcetype = paste0(type, "select")
  p = plotly::plot_ly(data, y=~sum_int, x=~rt, type="scatter", mode="markers", source = sourcetype, key = ~scanid, marker = list(color = 'rgba(0, 0, 0, 0)', line = list(color = 'rgba(0, 0, 0, 0)'))) %>% layout(
    xaxis=list(title= "Retention Time (min)"
              ),
    yaxis = list(title="Intensity"
  )) %>%
 add_trace(
 type = 'scatter',
 mode = 'lines',
 line = list(color = 'rgba(0,0,0,1)', width = 1),
 line = list(shape = 'spline', smoothing = 1),
 x = ~rt,
 y = ~sum_int
) %>% hide_legend()
  return(p)
}

plotly_imstrace <- function(data, type, session) {

  sourcetype = paste0(type, "select")
  p = plotly::plot_ly(data,
   y=~sum_int,
   x=~bin,
   type = 'scatter',
   mode = 'lines',
   line = list(color = 'rgba(0,0,0,1)', width = 1),
   line = list(shape = 'spline', smoothing = 1),
   source = sourcetype
   ) %>% layout(
    xaxis=list(title= "Retention Time (min)"
              ),
    yaxis = list(title="Intensity"
  )) %>% hide_legend()
  return(p)

}
