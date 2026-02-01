library(sf)
library(here)
library(dplyr)
library(readr)
library(tmap)
library(janitor)
library(tidyr)
library(stringr)
library(ggplot2)

##################################
#   URBAN DATA VIZ ASSESSMENT 1
##################################

#read in affordable housing data
nyc_affordable_housing <- read_csv(here::here("Affordable_Housing_Production_by_Building_20260201.csv"))%>%
  clean_names()
#read in census tract spatial data
nyc_census_tracts <- st_read(here::here("nyct2020_25d", "nyct2020.shp"))

#read in NTA spatial data
nyc_nta <- st_read(here::here("nyc2020_nta", "geo_export_54c96106-6959-44fc-b2fd-e41880b19a75.shp"))

#read in CD spatial data
#NOTE: nyc_affordable_housing df does not include BoroCD for merging purposes, might have to go with NTA scale due to this
nyc_cd <- st_read(here::here("nycd_25d", "nycd.shp"))

#join NTA spatial data to affordable housing data
nyc_affordable_housing <- nyc_affordable_housing%>%
  left_join(
    nyc_nta%>%select(nta2020, ntaname, cdta2020, cdtaname, shape_leng, shape_area, geometry),
    by = c("nta_neighborhood_tabulation_area" = "nta2020")
  )

#select columns to make df easier for computer to handle
nyc_affordable_housing <- nyc_affordable_housing%>%
  select(project_id, project_completion_date, building_id, borough, postcode, community_board, council_district, census_tract, nta_neighborhood_tabulation_area, latitude, longitude, total_units, ntaname, cdtaname, cdta2020, shape_leng, shape_area, geometry)

#set total_units for incomplete projects to 0
nyc_affordable_housing <- nyc_affordable_housing %>%
  mutate(
    total_units = if_else(
      is.na(project_completion_date),
      0L,
      total_units
    )
  )
