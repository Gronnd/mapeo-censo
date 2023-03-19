if(!require("leaflet")) install.packages("leaflet")
if(!require("sf")) install.packages("sf")
if(!require("tmap")) install.packages("tmap")
if(!require("tidyverse")) install.packages("tidyverse")

library(leaflet)
library(sf)
library(tmap)
library(tidyverse) 
library(languageserver)

#set working directory
setwd("C:/Users/xx_da/Documents/GitHub/mapeo-censo")



temp2 <- tempfile()


mydir <- getwd()
zip_file <- list.files(mydir, pattern = ".zip$", full.names = TRUE)

unzip(zipfile = zip_file, exdir = temp2)

shp_file <- list.files(temp2, pattern = ".shp$", full.names = TRUE)

censales_shp <- sf::read_sf(shp_file)
