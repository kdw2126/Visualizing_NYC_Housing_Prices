rm(list = ls())

library(tidyverse)
library(tigris)
library(leaflet)
library(stringr)
library(rgdal)

#### Information about Building Permits

setwd("~/Permit Information")

permit_information = read.csv("DOB_Permit_Issuance.csv", stringsAsFactors = T)

permit_information = permit_information %>%
  select(-"Bin..", -"Job..", -"Expiration.Date", -"Self_Cert", -"Owner.s.Business.Type", -"HIC.License", -"Job.doc...", -"Site.Fill", -"PERMIT_SI_NO", -"Work.Type", -"Act.as.Superintendent", -"Community.Board", -"COUNCIL_DISTRICT", -"Permittee.s.Other.Title", -"Block", -"Lot", -"Oil.Gas", -"DOBRunDate", -"Permittee.s.License..", -"Owner.s.Business.Name", -"Owner.s.First.Name", -"Owner.s.Last.Name", -"LATITUDE", -"LONGITUDE", -"Non.Profit", -"Bldg.Type", -starts_with("Site.Safety"), -starts_with("Super"), -contains("Permittee"), -contains("Phone")) %>%
  unite(address, c("House..", "Street.Name"), sep=" ") %>%
  rename(borough = BOROUGH, neighborhood_name = NTA_NAME) %>%
  select(-contains("House"), -contains("Special.District")) %>%
  filter(Job.Type != "A3" | Job.Type != "SG") %>%
  filter(Permit.Type != "SG") %>%
  filter(Permit.Type != "EW" | Job.Type != "A2") %>%
  filter(Permit.Status != "REVOKED") %>%
  mutate(Filing.Date = substr(Filing.Date, 1, 10),
         Issuance.Date = substr(Issuance.Date, 1, 10),
         Job.Start.Date = substr(Job.Start.Date, 1, 10)) %>%
  mutate(Filing.Date = as.Date(x = Filing.Date, format = "%m/%d/%Y"),
         Issuance.Date = as.Date(x = Issuance.Date, format = "%m/%d/%Y"),
         Job.Start.Date = as.Date(x = Job.Start.Date, format = "%m/%d/%Y")) %>%
  # head(5)
  mutate(filed_year = as.numeric(format(Filing.Date, "%Y")),
         issued_year = as.numeric(format(Issuance.Date, "%Y")),
         start_year = as.numeric(format(Job.Start.Date, "%Y"))) %>%
  select(-contains(".Date")) %>%
  mutate(new_building = (Permit.Type == "NB" | Job.Type == "NB")) %>%
  mutate(demolition = (Permit.Type == "DM" | Job.Type == "DM")) %>%
  mutate(conversion = (Permit.Type == "A1" | Job.Type == "A1")) %>%
  mutate(
    COUNTYFP = case_when(
      borough == "BRONX" ~ "005",
      borough == "BROOKLYN" ~ "047",
      borough == "MANHATTAN" ~ "061",
      borough == "QUEENS" ~ "081",
      borough == "STATEN ISLAND" ~ "085",
     )
    )

write.csv(permit_information, "Raw Permit Information.csv", row.names = FALSE)

rm(permit_information)

#### Build Tract Data

tract_permit_information = read.csv("Raw Permit Information.csv", stringsAsFactors = F, colClasses = c(rep("character",12), rep("numeric", 3), rep("logical", 3), "character"))

tract_permit_information = tract_permit_information %>%
  group_by(address, filed_year, issued_year, start_year, COUNTYFP, CENSUS_TRACT, Residential) %>%
  summarise(ns = sum(new_building), dem = sum(demolition), cs = sum(conversion)) %>%
  mutate(ns = ifelse(ns >= 1, 1, 0), dem = ifelse(dem >= 1, 1, 0), cs = ifelse(cs >= 1, 1, 0)) %>%
  mutate(Residential = ifelse(Residential == "YES", "YES", "NO")) %>%
  pivot_longer(c(filed_year, issued_year, start_year), values_to = "Year", names_to = "year_type")  %>%
  pivot_longer(c(ns, dem, cs), values_to = "outcome", names_to = "outcome_variable") %>%
  mutate(Residential = as.character(Residential)) %>%
  mutate(year_type = as.character(year_type)) %>%
  mutate(outcome_variable = as.character(outcome_variable)) %>%
  ungroup() %>%
  group_by(COUNTYFP, CENSUS_TRACT, Residential, year_type, Year, outcome_variable) %>%
  summarise(outcome = sum(outcome)) %>%
  ungroup() %>%
  rename(NAME = CENSUS_TRACT) %>%
  mutate(NAME = as.character(NAME))

write.csv(tract_permit_information, "Permits by Census Tract.csv", row.names = FALSE)

#### Build ZIP Data

zip_permit_information = read.csv("Raw Permit Information.csv", stringsAsFactors = F, colClasses = c(rep("character",12), rep("numeric", 3), rep("logical", 3), "character"))

zip_permit_information = zip_permit_information %>%
  group_by(address, filed_year, issued_year, start_year, Zip.Code, Residential) %>%
  summarise(ns = sum(new_building), dem = sum(demolition), cs = sum(conversion)) %>%
  mutate(ns = ifelse(ns >= 1, 1, 0), dem = ifelse(dem >= 1, 1, 0), cs = ifelse(cs >= 1, 1, 0)) %>%
  mutate(Residential = ifelse(Residential == "YES", "YES", "NO")) %>%
  pivot_longer(c(filed_year, issued_year, start_year), values_to = "Year", names_to = "year_type")  %>%
  pivot_longer(c(ns, dem, cs), values_to = "outcome", names_to = "outcome_variable") %>%
  mutate(Residential = as.character(Residential)) %>%
  mutate(year_type = as.character(year_type)) %>%
  mutate(outcome_variable = as.character(outcome_variable)) %>%
  ungroup() %>%
  group_by(Zip.Code, Residential, year_type, Year, outcome_variable) %>%
  summarise(outcome = sum(outcome)) %>%
  ungroup() %>%
  mutate(ZCTA5CE10 = as.character(Zip.Code))

write.csv(zip_permit_information, "Permits by ZIP.csv", row.names = FALSE)

#### Build Neighborhood Data

nta_permit_information = read.csv("Raw Permit Information.csv", stringsAsFactors = F, colClasses = c(rep("character",12), rep("numeric", 3), rep("logical", 3), "character"))

nta_permit_information = nta_permit_information %>%
  group_by(address, filed_year, issued_year, start_year, neighborhood_name, Residential) %>%
  summarise(ns = sum(new_building), dem = sum(demolition), cs = sum(conversion)) %>%
  mutate(Residential = ifelse(Residential == "YES", "YES", "NO")) %>%
  mutate(ns = ifelse(ns >= 1, 1, 0), dem = ifelse(dem >= 1, 1, 0), cs = ifelse(cs >= 1, 1, 0)) %>%
  pivot_longer(c(filed_year, issued_year, start_year), values_to = "Year", names_to = "year_type")  %>%
  pivot_longer(c(ns, dem, cs), values_to = "outcome", names_to = "outcome_variable") %>%
  mutate(Residential = as.character(Residential)) %>%
  mutate(year_type = as.character(year_type)) %>%
  mutate(outcome_variable = as.character(outcome_variable)) %>%
  ungroup() %>%
  group_by(neighborhood_name, Residential, year_type, Year, outcome_variable) %>%
  summarise(outcome = sum(outcome)) %>%
  ungroup() %>%
  mutate(ntaname = as.character(neighborhood_name))

write.csv(nta_permit_information, "Permits by Neighborhood.csv", row.names = FALSE)


  
#### Income-Specific Data
setwd("~/ZIP Code Income Data")
file.names <- dir("~/ZIP Code Income Data", pattern ="zip")
income_data = NULL

for(i in 1:length(file.names)){
  file <- read.csv(file.names[i],header=TRUE, stringsAsFactors=FALSE)
  file = file %>%
    select(zipcode, starts_with("agi_c"), starts_with("agi_s"), n1, a00100, a00200) %>%
    filter(zipcode >= 10000 & zipcode <= 11699) %>%
    filter(!(zipcode >= 10600 & zipcode <= 10999)) %>%
    mutate(year = as.numeric(substr(file.names[i], 8, 11)))
  income_data <- plyr::rbind.fill(income_data, file)
}

rm(file, file.names, i)

income_data = income_data %>%
  mutate(agi_group = ifelse(is.na(agi_stub), agi_class, agi_stub))  %>%
  select(-agi_stub, -agi_class) %>%
  rename(total_income = a00100, wage_income = a00200) %>%
  pivot_longer(-c(zipcode, year, agi_group), values_to = "value", names_to = "value_label")  %>%
  pivot_wider(names_from = c(value_label, agi_group), values_from = value) %>%
  mutate(total_income = rowSums(.[grep("total_income", names(.))])) %>%
  mutate(wage_income = rowSums(.[grep("wage_income", names(.))])) %>%
  mutate(total_households = n1_1 + n1_2 + n1_3 + n1_4 + n1_5 + n1_6 + n1_7) %>%
  select(zipcode, year, total_households, starts_with("n1"), total_income, wage_income)

cpi <- read.csv("CPI.csv",header=TRUE)

income_data = merge(income_data, cpi, by.x="year", by.y = "ï..Year")

income_data = income_data %>%
  mutate(total_income = total_income / CPI.Value * 100, wage_income = wage_income / CPI.Value * 100) %>%
  select(-CPI.Value)

write.csv(income_data, "Income_Info.csv", row.names = FALSE)

#### Industry-Specific Data

setwd("~/Industry Data")
file.names <- dir("~/Industry Data", pattern ="detail.csv")
industry_data = NULL

for(i in 1:length(file.names)){
  file <- read.csv(file.names[i],header=TRUE, stringsAsFactors=FALSE)
  file = file %>%
    select(zip, naics, est, n500_999, n1000) %>%
    filter(zip >= 10000 & zip <= 11699) %>%
    filter(!(zip >= 10600 & zip <= 10999)) %>%
    filter(str_detect(naics, "----")) %>%
    mutate(year = as.numeric(substr(file.names[i], 4, 5)) + 2000) %>%
    mutate(large = n500_999 + n1000) %>%
    select(-"n500_999", -"n1000") %>%
    mutate(
      naics = case_when(
        naics == "------" ~ "total",
        naics == "11----" ~ "agriculture",
        naics == "21----" ~ "mining",
        naics == "22----" ~ "utilities",
        naics == "23----" ~ "construction",
        naics == "31----" ~ "manufacturing",
        naics == "42----" ~ "wholesale",
        naics == "44----" ~ "retail",
        naics == "48----" ~ "logistics",
        naics == "51----" ~ "information",
        naics == "52----" ~ "finance",
        naics == "53----" ~ "real_estate",
        naics == "54----" ~ "professional",
        naics == "55----" ~ "management",
        naics == "56----" ~ "administration",
        naics == "61----" ~ "education",
        naics == "62----" ~ "health_care",
        naics == "71----" ~ "arts",
        naics == "72----" ~ "accomedation",
        naics == "81----" ~ "services",
        naics == "99----" ~ "government",
      )
    )
  industry_data <- rbind(industry_data, file)
}

rm(file, file.names, i)

industry_data = industry_data %>%
  pivot_longer(-c(zip, year, naics), values_to = "value", names_to = "value_label") %>%
  pivot_wider(names_from = c(naics, value_label), values_from = value) %>%
  arrange(zip, year) %>%
  select(-starts_with("mining"), -starts_with("agriculture"), -starts_with("utilit")) %>%
  mutate(entertainment_est = accomedation_est + arts_est) %>%
  mutate(entertainment_large = accomedation_large + arts_large) %>%
  mutate(professional_large = professional_large + information_large + finance_large + real_estate_large + professional_large + management_large + administration_large) %>%
  mutate(professional_est = professional_est + information_est + finance_est + real_estate_est + professional_est + management_est + administration_est) %>%
  select(-starts_with("accomedation"), -starts_with("arts"), -starts_with("information"), -starts_with("finance"), -starts_with("real_estate"), -starts_with("management"), -starts_with("administration"))  

write.csv(industry_data, "Aggregated_Industry_Info.csv", row.names = FALSE)

#### Aggregate Information about Employment and Earnings

setwd("~/Industry Data")
file.names <- dir("~/Industry Data", pattern ="totals.csv")
employment_info = NULL

for(i in 1:length(file.names)){
  file <- read.csv(file.names[i],header=TRUE, stringsAsFactors=FALSE)
  file = file %>%
    select(zip, emp, ap, qp1) %>%
    filter(zip >= 10000 & zip <= 11699) %>%
    filter(!(zip >= 10600 & zip <= 10999)) %>%
    mutate(year = as.numeric(substr(file.names[i], 4, 5)) + 2000)
  employment_info <- rbind(employment_info, file)
}

rm(file, file.names, i)
 
cpi <- read.csv("CPI.csv",header=TRUE)

employment_info = merge(employment_info, cpi, by.x="year", by.y = "ï..Year")

employment_info = employment_info %>%
  mutate(ap = ap / CPI.Value * 100, qp1 = qp1 / CPI.Value * 100) %>%
  select(-CPI.Value)
  
write.csv(employment_info, "Aggregated_Employment_Info.csv", row.names = FALSE)
