library(sf)
library(here)
library(dplyr)
library(readr)
library(tmap)
library(janitor)
library(tidyr)
library(stringr)
library(ggplot2)
library(lubridate)

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

#create new df aggregating total unit counts by year
nta_year_totals <- nyc_affordable_housing %>%
  mutate(
    comp_year = year(mdy(project_completion_date))  # use ymd() if your dates are already "YYYY-MM-DD"
  ) %>%
  filter(!is.na(comp_year), comp_year >= 2014) %>%
  group_by(nta_neighborhood_tabulation_area, ntaname, comp_year) %>%
  summarise(
    units = sum(total_units, na.rm = TRUE),
    .groups = "drop"
  )

#make sure every NTA has every year
nta_year_totals_complete <- nta_year_totals %>%
  tidyr::complete(
    nta_neighborhood_tabulation_area,
    comp_year = 2014:max(comp_year, na.rm = TRUE),
    fill = list(units = 0)
  )

#create new df with cumulative totals after each year
nta_year_cum <- nta_year_totals_complete %>%
  arrange(nta_neighborhood_tabulation_area, comp_year) %>%
  group_by(nta_neighborhood_tabulation_area) %>%
  mutate(units_cum = cumsum(units)) %>%
  ungroup()

#create df with city-wide totals for each year
city_year_totals <- nta_year_cum %>%
  group_by(comp_year) %>%
  summarise(
    city_units = sum(units, na.rm = TRUE),
    city_units_cum = sum(units_cum, na.rm = TRUE),
    .groups = "drop"
  )

#join city totals back to NTA df
nta_year_cum <- nta_year_cum %>%
  left_join(city_year_totals, by = "comp_year")

#creating centroids for circle locations on Mapbox
nta_points <- nyc_nta %>%
  st_make_valid() %>% #safe-guard for weird geometries
  st_transform(4326) %>% #Mapbox wants lon/lat (WGS84)
  st_centroid() %>%           
  select(nta2020, ntaname, geometry)

#join back to nta_year_cum
nta_year_cum_points <- nta_points %>%
  left_join(
    nta_year_cum,
    by = c("nta2020" = "nta_neighborhood_tabulation_area")
  )

#lookup table to attach nta names to new rows
nta_lookup <- nyc_nta %>%
  st_drop_geometry() %>%
  select(nta2020, ntaname)

#joining back 
nta_year_totals_complete <- nta_year_totals_complete %>%
  left_join(nta_lookup, by = c("nta_neighborhood_tabulation_area" = "nta2020"))

################################
#   EXPORTING FOR WEB MAPPING
################################

#geojson for nta_year_cum_points
dir.create(here::here("output"), showWarnings = FALSE) #creates folder called output

st_write(
  nta_year_cum_points,
  here::here("output", "nta_year_cum_points.geojson"), #inputs new, exported geojson into output folder
  delete_dsn = TRUE
)

#json for city_year_totals
jsonlite::write_json(
  city_year_totals,
  path = here::here("output", "city_year_totals.json"),
  pretty = TRUE,
  auto_unbox = TRUE
)

