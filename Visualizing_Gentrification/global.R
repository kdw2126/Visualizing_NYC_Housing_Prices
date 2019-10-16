library(tidyverse)
library(tigris)
library(leaflet)
library(ggthemes)
library(geojsonio)

options(shiny.fullstacktrace = TRUE)
options(tigris_use_cache = TRUE)

nyc_tracts <- tracts("NY", c("New York", "Kings", "Queens", "Bronx", "Richmond"), cb=TRUE)
nyc_zipcodes <- zctas(cb = TRUE, starts_with = c("100", "101", "102", "103", "104", "105","110", "111", "112", "113", "114", "115", "116"))
nyc_neighborhoods <- geojson_read("Neighborhood Tabulation Areas.geojson", what = "sp")

income_data <- read.csv("Income_Info.csv",header=TRUE)
employment_info <- read.csv("Aggregated_Employment_Info.csv",header=TRUE)
industry_data <- read.csv("Aggregated_Industry_Info.csv",header=TRUE)

permit_information_tract <- read.csv("Permits by Census Tract.csv", header=TRUE, stringsAsFactors = FALSE, colClasses = c(rep("character",4), rep("numeric", 1), "character", "numeric"))
permit_information_zips <- read.csv("Permits by ZIP.csv", header=TRUE, stringsAsFactors = FALSE)
permit_information_zips = mutate(permit_information_zips, ZCTA5CE10 = as.character(ZCTA5CE10))

permit_information_ntas <-read.csv("Permits by Neighborhood.csv", header=TRUE, stringsAsFactors = FALSE)
