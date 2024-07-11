# note to users:
# the app requires the installation of the following packages:
# - shiny
# - plotly
# - DT
# - shinythemes
# - shinyjqui
# - zip
# - latest version of data.table from source

# In addition, the kaleido library is required to save plots as svg
# See details and installation at https://github.com/plotly/Kaleido
# run py_install("python-kaleido") in R with the reticulate package
# and py_install("plotly")

# UI and graphs
install.packages(c('shiny', 'plotly', 'DT', "shinythemes", "shinydashboard", "shinyjqui", "shinyWidgets", "shinyjs", "jsonlite", "zip", "ggplot2", "RColorBrewer", "sortable"))
# data.table from source
install.packages("data.table", type = "source",repos = "http://Rdatatable.github.io/data.table")
install.packages(c("dplyr", "dtplyr"))
devtools::install_github('andrewsali/shinycssloaders')
