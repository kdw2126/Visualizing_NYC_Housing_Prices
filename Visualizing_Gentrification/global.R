library(tidyverse)
library(tigris)
library(leaflet)
library(ggthemes)

options(shiny.fullstacktrace = TRUE)

nyc_tracts <- tracts("NY", c("New York", "Kings", "Queens", "Bronx", "Richmond"), cb=TRUE)
# nyc_zipcodes <- zctas(cb = TRUE, starts_with = c("100", "101", "102", "103", "104", "105","110", "111", "112", "113", "114", "115", "116"))

income_data <- read.csv("Income_Info.csv",header=TRUE)
employment_info <- read.csv("Aggregated_Employment_Info.csv",header=TRUE)
industry_data <- read.csv("Aggregated_Industry_Info.csv",header=TRUE)
permit_information_tract <- read.csv("Permits by Census Tract.csv",header=TRUE)
