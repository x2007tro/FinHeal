##
# Packages
##
library(readxl)
library(shinythemes)
library(DT)
library(lubridate)
library(ggplot2)
library(treemapify)

##
# Source server and ui components
##
source("helper/dbhelper.R")
source("helper/utility.R")
source("global.R", local = FALSE)
source("iUI/main.R")
source("iServer/main.R")

##
# Launch shiny app
##
shinyApp(
  ui = mainUI,
  server = mainServer
)