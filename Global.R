##################################################
###### 2 .- Carga de paquetes ###########

packages <- c('ggplot2','plotly','reshape2','reshape','plyr','shiny','rhandsontable',
              'data.table','DT','stringr','knitr','rmarkdown','OneR','shinythemes',
              'visNetwork','networkD3','tidyverse','igraph')

for (package in packages) {
  if (!(require(package, character.only=T, quietly=T))) {
    install.packages(package)
    library(package, character.only=T)
  }
}

rm(package,packages)

library(visNetwork)
library(networkD3)
require(reshape)
library(rhandsontable)
library(shiny)
require(data.table)
require(reshape2)
require(DT)
require(plotly)
require(RColorBrewer)
require(OneR)
require(shinythemes)
require(dplyr)
require(reshape2)
library(tidyverse)
require(igraph)
