##
# Packages
##
library(readxl)
library(shinythemes)
library(DT)
library(lubridate)
library(ggplot2)
library(treemapify)
source('http://faculty.ucr.edu/~tgirke/Documents/R_BioCond/My_R_Scripts/mortgage.R')

##
# Source server and ui components
##kmpka
source("helper/dbhelper.R")
source("helper/utility.R")
source("global.R", local = FALSE)
source("iServer/main.R")
source("iUI/main.R")

##
# Launch shiny app
##
shinyApp(
  ui = mainUI,
  server = mainServer
)