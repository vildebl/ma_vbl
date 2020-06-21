## MASTER SCRIPT ## 
# Used to create final dataset #
rm(list=ls())
library(cshapes)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(tmap)
library(lubridate)
library(zoo)
library(spacetime)

priogrid <- sf::read_sf("C:/Users/villar/Dropbox/MA_Vilde/PRIO-GRID/priogrid_cell.shp")

acled <- read.csv("C:/Users/villar/Dropbox/MA_Vilde/ACLED/2010-01-01-2018-12-31.csv", stringsAsFactors = FALSE)

#### PROTEST EVENTS IN COUNTRIES CODED FROM 2010 ####

africa <- acled %>%
  filter(str_detect(region, "Africa")) %>%
  filter(sub_event_type != "Mob violence") %>%
  mutate(sub_event_type = factor(sub_event_type, 
                                 levels = c("Peaceful protest", 
                                            "Protest with intervention",
                                            "Excessive force against protesters",
                                            "Violent demonstration"))) %>%
  select(data_id, iso, event_id = event_id_no_cnty, event_date, year, country, region, admin1, admin2, event_type, sub_event_type, fatalities,
         inter1, inter2, interaction, actor1, assoc_actor_1, actor2, assoc_actor_2, time_precision, geo_precision, longitude, latitude, notes) %>%
  filter(geo_precision != 3, time_precision != 3) 

asia <- acled %>% 
  filter(country == "Pakistan" | country == "Sri Lanka" | country == "Bangladesh" | country == "Nepal" |
           country == "Cambodia" | country == "Laos" | country == "Myanmar" | country == "Thailand" | country == "Vietnam") %>%
  filter(sub_event_type != "Mob violence") %>%
  mutate(sub_event_type = factor(sub_event_type, 
                                 levels = c("Peaceful protest", 
                                            "Protest with intervention",
                                            "Excessive force against protesters",
                                            "Violent demonstration"))) %>%
  select(data_id, iso, event_id = event_id_no_cnty, event_date, year, country, region, admin1, admin2, event_type, sub_event_type, fatalities,
         inter1, inter2, interaction, actor1, assoc_actor_1, actor2, assoc_actor_2, time_precision, geo_precision, longitude, latitude, notes) %>%
  filter(geo_precision != 3, time_precision != 3)


combined <- rbind(africa, asia)

combined <- combined %>%
  mutate(assoc_actor_1 = ifelse(assoc_actor_1 == "", NA, assoc_actor_1),
         assoc_actor_2 = ifelse(assoc_actor_2 == "", NA, assoc_actor_2)) 

pg_sp <- sf::as_Spatial(priogrid)

combined_sf <- combined %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  sf::st_set_crs(st_crs(priogrid))

combined_sp <- sf::as_Spatial(combined_sf)

acled_pg_sf <- sp::over(combined_sp, pg_sp)

acled_pg <- cbind(acled_pg_sf, combined)

# save(acled_pg, file = "acled_pg.RData")


#### GRID CELL-WEEK STRUCTURE ####
# Observations in same grid cell grouped by week 

acled_pg_w <- acled_pg %>%
  mutate(event_date = as.Date(acled_pg$event_date, "%d %B %Y")) %>%
  mutate(week = floor_date(event_date, unit = "week", week_start = 1))

# Remove observations identified to take place in a different country than the majority of events

filtered_acled_pg_w <- acled_pg_w %>%
  filter(!(gid == 123511 & country == "Republic of Congo" & week == "2012-06-18"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2012-10-08"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2014-05-05"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2014-05-12"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2014-05-19"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2014-05-26"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2014-09-22"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2015-06-01"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2015-08-24"),
         !(gid == 123511 & country == "Democratic Republic of Congo" & week == "2015-09-14"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2016-04-04"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2016-05-30"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2016-10-31"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2017-03-13"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2017-06-26"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2017-11-27"),
         !(gid == 124979 & country == "Burundi" & week == "2013-07-08"),
         !(gid == 124979 & country == "Democratic Republic of Congo" & week == "2015-09-07"),
         !(gid == 124979 & country == "Democratic Republic of Congo" & week == "2016-02-15"),
         !(gid == 124979 & country == "Democratic Republic of Congo" & week == "2017-11-27"),
         !(gid == 124979 & country == "Democratic Republic of Congo" & week == "2018-04-16"),
         !(gid == 124979 & country == "Democratic Republic of Congo" & week == "2018-06-11"),
         !(gid == 124979 & country == "Democratic Republic of Congo" & week == "2018-07-02"),
         !(gid == 124979 & week == "2018-09-17"),
         !(gid == 124979 & week == "2018-09-24"),
         !(gid == 127139 & country == "Rwanda" & week == "2015-09-21"),
         !(gid == 127860 & country == "Rwanda" & week == "2015-06-22"),
         !(gid == 130749 & week == "2014-10-06"),
         !(gid == 130749 & week == "2014-10-13"),
         !(gid == 138603 & country == "Ghana" & week == "2016-01-18"),
         !(gid == 138603 & country == "Ghana" & week == "2017-03-13"),
         !(gid == 143727 & country == "Ethiopia" & week == "2018-05-07"),
         !(gid == 153918 & week == "2018-02-05"),
         !(gid == 153918 & week == "2018-02-12"),
         !(gid == 159665 & week == "2018-10-29"),
         !(gid == 159665 & week == "2018-11-12"),
         !(gid == 178936 & week == "2017-02-20"),
         !(gid == 178936 & week == "2017-03-06"),
         !(gid == 181097 & country == "Algeria" & week == "2017-05-08"),
         !(gid == 182538 & country == "Algeria" & week == "2016-07-25"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2015-12-07"),
         !(gid == 123511 & country == "Republic of Congo" & week == "2018-02-26"),
         !(gid == 124259 & week == "2018-05-14"), 
         !(gid == 124259 & week == "2015-05-28"),
         !(gid == 130749 & week == "2014-06-16"),
         !(gid == 130749 & week == "2014-06-30"))


# Create count variables for each gid per week
freq_var_w1 <- filtered_acled_pg_w %>%
  mutate(nonviolent = ifelse(sub_event_type == "Peaceful protest" | sub_event_type == "Protest with intervention" |
                               sub_event_type == "Excessive force against protesters", 1, 0),
         violent = ifelse(sub_event_type == "Violent demonstration", 1, 0),
         intervention = ifelse(interaction == 16 & sub_event_type == "Protest with intervention", 1, 0),
         excessive_force = ifelse(interaction == 16 & sub_event_type == "Excessive force against protesters", 1, 0),
         repression_viol = ifelse(interaction == 15, 1, 0)) %>% 
  mutate(organized_nv = ifelse(nonviolent == 1 & grepl("Protesters", actor1) & !is.na(assoc_actor_1), 1, 
                               ifelse(nonviolent == 1 & grepl("Protesters", actor2) & !is.na(assoc_actor_2) & !grepl("Civilians", assoc_actor_2), 1, 0))) %>%
  mutate(organized_v = ifelse(violent == 1 & grepl("Rioters", actor1) & !is.na(assoc_actor_1), 1,
                              ifelse(violent == 1 & grepl("Rioters", actor2) & !is.na(assoc_actor_2) & !grepl("Civilians", assoc_actor_2), 1, 0)))
  
  
freq_var_w <- freq_var_w1 %>% 
  group_by(gid, week) %>% # create count variables, number of unique events per week per cell
  summarize(nonviolent_freq = sum(nonviolent), 
            violent_freq = sum(violent),
            interv_freq = sum(intervention),
            exforc_freq = sum(excessive_force),
            repviol_freq = sum(repression_viol),
            fatalities = sum(fatalities),
            country = paste(sort(unique(country)), collapse = ", "),
            organized_protest = sum(organized_nv)) %>%
  arrange(gid, week) %>%
  ungroup()

# Create PG-cell-week structure #
week_gid <- acled_pg_w %>%
  select(gid) %>%
  distinct() %>%
  rowwise() %>%
  mutate(week_structure = list(seq(floor_date(as.Date("2010-01-01"), unit = "week", week_start = 1), 
                                   floor_date(as.Date("2018-12-31"), unit = "week", week_start = 1), by = "weeks"))) %>% # List of all weeks from 2010-2018 for each cell
  unnest(week_structure) %>%
  arrange(gid, week_structure) 

acled_pg_weeks <- left_join(week_gid, freq_var_w, by = c("gid" = "gid", "week_structure" = "week")) # Join on grid cell and week, keep all weeks from 2010-2018

cellweek_prot <- acled_pg_weeks %>% 
  mutate(nonviolent_freq = replace_na(nonviolent_freq, 0),
         violent_freq = replace_na(violent_freq, 0),
         interv_freq = replace_na(interv_freq, 0),
         exforc_freq = replace_na(exforc_freq, 0),
         repviol_freq = replace_na(repviol_freq, 0),
         fatalities = replace_na(fatalities, 0)) %>%
  mutate(total_prot_ev = nonviolent_freq + violent_freq) %>%  # Total number of protest events in cell-week
  mutate(prop_viol = violent_freq/total_prot_ev) %>% # Fraction of protest events that are violent per cell-week
  mutate(prop_viol = ifelse(is.nan(prop_viol), NA, prop_viol)) %>% 
  mutate(prop_organized = organized_protest/total_prot_ev) %>% # Fraction of protest events that are organized per cell-week
  rename(week = week_structure)

# save(cellweek_prot, file = "cellweek_100220.RData")


#### PROTEST SPELL STRUCTURE ####
# Two week threshold: Protest spells defined as consecutive weeks of protest activity, allowing for one week break

# load("cellweek_100220.RData") # Dataset with all weeks for all cells
base_data <- cellweek_prot %>%
  mutate(prot_dummy = ifelse(total_prot_ev != 0, 1, 0)) %>% # Protest occurrence dummy GCW
  arrange(gid, week) %>%
  group_by(gid) %>%
  mutate(nth_prot = cumsum(prot_dummy)) %>% # How many protests-weeks (consecutive and single) have occurred in past GCW
  mutate(prev_wk_prot = lag(prot_dummy, n = 1, default = 0)) %>% # Was there protest in previous week
  mutate(next_wk_prot = lead(prot_dummy, n = 1, default = 0)) %>% # Was there protest in next week
  ungroup() 

# save(base_data, file = "base_100220.RData") # base for creating different lags

# Defining protests cycles with two-week threshold
# load("base_100220.RData")
two_week <- base_data %>%
  dplyr::rename(one_wk_prior = prev_wk_prot,
                one_wk_post = next_wk_prot) %>%
  arrange(gid, week) %>%
  group_by(gid) %>%
  mutate(two_wk_prior = lag(prot_dummy, n = 2, default = 0)) %>% # Was there protest two weeks prior
  mutate(two_wk_post = lead(prot_dummy, n = 2, default = 0)) %>% # Was there protest two weeks after
  ungroup()

two_week_filtered <- two_week %>%
  mutate(single_prot_week = ifelse(two_wk_prior == 0 & one_wk_prior == 0 & prot_dummy == 1 & one_wk_post == 0 & two_wk_post == 0, 1, 0)) %>%
  filter(single_prot_week != 1) %>%
  mutate(break_week = ifelse(one_wk_prior == 1 & prot_dummy == 0 & one_wk_post == 1, 1, 0)) %>% # Weeks without reported activity that are preceded AND succeeded by protest weeks
  filter(!(break_week == 0 & prot_dummy == 0)) # Weeks without activity are included IF AND ONLY IF they are NOT preceded AND succeeded by protest  


two_week2 <- two_week_filtered %>% 
  arrange(gid, week) %>%
  mutate(start_onewk = ifelse(one_wk_prior == 0 & prot_dummy == 1, 1, 0), # No protest in previous week, define as strict start week, one week threshold
         end_onewk = ifelse(one_wk_post == 0 & prot_dummy == 1, 1, 0)) %>% # No protest in next week, define as strict end week, one week threshold
  mutate(start_twowk = ifelse(two_wk_prior == 0 & one_wk_prior == 0 & prot_dummy == 1, 1, 0)) %>% # No protest two weeks ago, no protest in previous week, protest in current week - defined as start week
  mutate(end_twowk = ifelse(two_wk_post == 0 & one_wk_post == 0 & prot_dummy == 1, 1, 0)) %>% # No protest two weeks later, no protest in next week, protest in current week - defined as end week
  mutate(start_twowk = ifelse(is.na(start_twowk) & week == as.Date("2009-12-28", "%Y-%m-%d") & prot_dummy == 1, 1, start_twowk), # If there is protest in first week observed, define as start week
         end_twowk = ifelse(is.na(end_twowk) & prot_dummy == 1 & week == as.Date("2018-12-31", "%Y-%m-%d"), 1, end_twowk)) %>% # If there is protest in last week observed, define as end week
  mutate(start_onewk = ifelse(is.na(start_onewk) & week == as.Date("2009-12-28", "%Y-%m-%d") & prot_dummy == 1, 1, start_onewk),
         end_onewk = ifelse(is.na(end_onewk) & week == as.Date("2018-12-31", "%Y-%m-%d"), 1, end_onewk))


two_week3 <- two_week2 %>%
  mutate(start_twowk = ifelse(is.na(start_twowk) & one_wk_prior == 0 & start_onewk == 1, 1, start_twowk), # Two weeks into dataset, define start date in same manner as with one week threshold 
         end_twowk = ifelse(is.na(end_twowk) & one_wk_post == 0 & end_onewk == 1, 1, end_twowk)) %>% # If there is no protest in final week in dataset, define second last week as end date 
  mutate(start_week_two = ifelse(start_twowk == 1, format(as.Date(week), "%Y-%m-%d"), NA),
         end_week_two = ifelse(end_twowk == 1, format(as.Date(week), "%Y-%m-%d"), NA)) %>%
  mutate(start_week_two = as.Date(start_week_two),
         end_week_two = as.Date(end_week_two))


gcw_twowk <- two_week3 %>%
  arrange(gid, week) %>%
  mutate(start_week_two = na.locf(start_week_two, na.rm = FALSE), # Spells have same start month
         end_week_two = na.locf(end_week_two, na.rm = FALSE, fromLast = TRUE)) %>% # Spells have same end month
  mutate(group_id = group_indices(., gid, start_week_two, end_week_two)) %>% 
  group_by(group_id) %>%
  filter(!any(prop_viol == 1 & start_twowk == 1)) %>% # Remove observations that begin 100 % violently
  ungroup() %>%
  mutate(new_group_id = group_indices(., gid, start_week_two, end_week_two)) %>% # Unique ID for each spell
  select(-group_id) %>%
  rename(group_id = new_group_id) %>% 
  arrange(gid, week) %>%
  group_by(group_id) %>%
  mutate(duration = as.numeric(difftime(week, start_week_two, units = "weeks"))) %>% # Duration variable
  mutate(prot_length = as.numeric(difftime(end_week_two, start_week_two, units = "weeks") + 1)) %>% # Number of total protest weeks in spell
  ungroup() 

# save(gcw_twowk, file = "gcw_v3_240220.RData")


#### DEFINING VARIABLES ####

# Add country codes (ACLED) for easy merge with other data

# load("gcw_v3_240220.RData")
acled_cc <- read.csv("C:/Users/villar/Dropbox/MA_Vilde/ACLED/2010-01-01-2018-12-31.csv", stringsAsFactors = FALSE)
acled_cc <- acled_cc %>%
  select(iso, country) %>%
  distinct()

acled <- gcw_twowk %>% left_join(acled_cc, by = "country") %>%
  group_by(group_id) %>%
  mutate(country = na.locf(country),
         iso = na.locf(iso)) %>% 
  ungroup() %>%
  mutate(year = lubridate::year(week))

# save(acled, file = "acled_240220.RData")

# load("acled_210220.RData")


# GDP PER CAPITA (UN estimate) 

un_gdp <- readxl::read_xlsx("C:/Users/villar/Dropbox/MA_Vilde/iv_data/gdp/Download-GDPPCconstant-USD-countries.xlsx", sheet = 1, skip = 2)

un <- un_gdp %>% 
  janitor::clean_names() %>%
  select(iso = country_id, x2009:x2018) %>%
  pivot_longer(cols = x2009:x2018,
               names_to = "year",
               values_to = "gdp_cap") %>%
  mutate(year = gsub("x", "", year)) %>%
  mutate(year = as.numeric(year),
         iso = as.numeric(iso)) %>%
  mutate(iso = ifelse(iso == 835, 834, iso)) %>%
  filter(iso %in% acled$iso)

acled_gdppc <- left_join(acled, un, by = c("year", "iso"))


# TRAVEL TIME TO NEAREST URBAN CENTER (average)

access <- raster::raster("C:/Users/villar/Dropbox/MA_Vilde/iv_data/2015_accessibility_to_cities_v1.0/2015_accessibility_to_cities_v1.0.tif")

devtools::install_github("prio-data/priogrid")
devtools::load_all()

prio_aggregate_raster <- function(x, fun){
  
  fact <- priogrid::prio_resolution() / raster::res(x) 
  
  res <- raster::aggregate(x,fact=fact,fun=fun)
  
  raster::crs(res) <- priogrid::prio_crs()
  
  pg <- priogrid::prio_blank_grid()
  raster::values(pg) <- NA
  
  raster::origin(res) <- raster::origin(pg)
  
  res <- raster::merge(res, pg, overlap = FALSE)
  
  res
}

tst <- prio_aggregate_raster(access, fun = "mean") # Aggregate raster cells to size of PRIO-GRID cells; compute mean travel time in each cell

pg1 <- priogrid::prio_blank_grid() # Add grid cell number from PRIO-GRID

pg_access <- raster::addLayer(pg1, tst)

access1 <- raster::rasterToPolygons(pg_access)

access_sf <- st_as_sf(access1) %>%
  filter(!is.na(layer.2)) %>%
  select(gid = layer.1, ttime_avg = layer.2)

acled_access <- left_join(acled_gdppc, access_sf, by = "gid") 


# TIME SINCE PAST PROTEST

tspp <- acled_access %>%
  group_by(gid) %>%
  arrange(gid, week) %>%
  mutate(tspp = lag(week, n = 1)) %>% # Previous protest week in grid cell
  ungroup() %>%
  mutate(start_end = interval(as.Date(start_week_two), as.Date(end_week_two))) %>%
  mutate(tspp1 = ifelse(tspp %within% start_end, 1, 0)) %>% 
  mutate(tspp2 = ifelse(tspp1 == 1, NA, format(as.Date(tspp, format = "%Y-%m-%d"), "%Y-%m-%d"))) %>%
  mutate(time_since_past_protest = signif(as.numeric(difftime(start_week_two, tspp2, units = "weeks")))) %>% # Number of weeks between end of last protest spell and start of current spell in grid cell
  group_by(gid, group_id) %>%
  mutate(time_since_past_protest = na.locf(time_since_past_protest, na.rm = FALSE)) %>% # TODO: Maybe change this so that it is cumulative?
  mutate(time_since_past_protest = ifelse(is.na(time_since_past_protest), 0, time_since_past_protest)) %>% # Cases of single and first occurrences, 0
  select(-tspp, -tspp1, -tspp2) %>%
  ungroup()



# V-DEM LIBERAL DEMOCRACY

vdem <- read.csv("C:/Users/villar/Dropbox/MA_Vilde/iv_data/Country_Year_V-Dem_Full+others_CSV_v9 2/V-Dem-CY-Full+Others-v9.csv", 
                 stringsAsFactors = FALSE)

v2xlibdem <- vdem %>%
  select(country_id, country = country_name, year, v2x_libdem) %>%
  filter(year >= 2009) %>%
  mutate(country = ifelse(country == "Swaziland", "eSwatini", country),
         country = ifelse(country == "Democratic Republic of the Congo", "Democratic Republic of Congo", country),
         country = ifelse(country == "Republic of the Congo", "Republic of Congo", country),
         country = ifelse(country == "The Gambia", "Gambia", country),
         country = ifelse(country == "Burma/Myanmar", "Myanmar", country))

acled_vdem <- tspp %>% left_join(v2xlibdem, by = c("country", "year")) 
acled_vdem$libdem_sq <- acled_vdem$v2x_libdem^2



# FOOD PRICE CHANGE - MONTHLY (ILO)

ilo <- read.csv("C:/Users/villar/Dropbox/MA_Vilde/iv_data/ilo/ilostat-2020-02-24.csv", 
                stringsAsFactors = FALSE)

ilo2 <- ilo %>% janitor::clean_names() %>%
  rename(country = i_ref_area_label) %>%
  filter(str_detect(classif1_label, "Food")) %>% # Food prices
  mutate(country = ifelse(str_detect(country, "Tanzania"), "Tanzania", country),
         country = ifelse(str_detect(country, "Congo, Democratic"), "Democratic Republic of Congo", country),
         country = ifelse(str_detect(country, "d'Ivoire"), "Ivory Coast", country),
         country = ifelse(country == "Viet Nam", "Vietnam", country)) %>%
  arrange(country) %>%
  separate(time, c('year', 'month'), sep = "M") %>%
  mutate(month = as.Date(paste(year, month, "01", sep = "-"))) %>%
  select(country, month, ch_fp = obs_value)

missing <- acled %>% anti_join(ilo2, by = "country") %>% distinct(country) # Estimate missing for seven countries - use as robustness test
# Angola, Central African Republic, Chad, eSwatini, Libya, Republic of Congo, Somalia

acled_ilo <- acled_vdem %>% 
  mutate(month = floor_date(week, unit = "months")) %>%
  left_join(ilo2, by = c("country", "month"))


# UNEMPLOYMENT RATE (World Bank: ILO estimate)

unemp <- read.csv("C:/Users/villar/Dropbox/MA_Vilde/iv_data/unemp/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_712954/API_SL.UEM.TOTL.ZS_DS2_en_csv_v2_712954.csv", skip = 4,
                  stringsAsFactors = FALSE)

unemp1 <- unemp %>% janitor::clean_names() %>%
  select(country = country_name, x2009:x2018) %>%
  pivot_longer(cols = x2009:x2018,
               names_to = "year",
               values_to = "unemp_rate") %>%
  mutate(year = as.numeric(gsub("x", "", year))) %>%
  arrange(country, year) %>%
  mutate(country = ifelse(country == "Eswatini", "eSwatini", country),
         country = ifelse(country == "Congo, Dem. Rep.", "Democratic Republic of Congo", country),
         country = ifelse(country == "Congo, Rep.", "Republic of Congo", country),
         country = ifelse(country == "Cote d'Ivoire", "Ivory Coast", country),
         country = ifelse(country == "Egypt, Arab Rep.", "Egypt", country),
         country = ifelse(country == "Gambia, The", "Gambia", country))


acled_unemp <- left_join(acled_ilo, unemp1, by = c("country", "year"))

acled_ctrl <- acled_unemp

# save(acled_ctrl, file = "acled_ctrl_240220.RData")
# load("acled_ctrl_240220.RData")


# REPRESSION IN Wt-1

acled_ctrl$geometry <- NULL
acled_rep <- acled_ctrl %>%
  group_by(gid, group_id) %>%
  mutate(interv_lag = lag(interv_freq, n = 1), 
         exforc_lag = lag(exforc_freq, n = 1),
         repviol_lag = lag(repviol_freq, n = 1)) %>%
  mutate(interv_dummy_lag = ifelse(interv_lag >= 1, 1, 0),
         exforc_dummy_lag = ifelse(exforc_lag >= 1, 1, 0),
         repviol_dummy_lag = ifelse(repviol_lag >= 1, 1, 0)) %>%
  group_by(gid) %>%
  arrange(week) %>%
  mutate(prev_interv = cumsum(replace_na(interv_lag, 0)),
         prev_exforc = cumsum(replace_na(exforc_lag, 0)),
         prev_repviol = cumsum(replace_na(repviol_lag, 0))) %>%
  ungroup() %>%
  arrange(group_id, week)
  


# CHANGE IN PROPORTION OF VIOLENT EVENTS WEEK


acled_viol <- acled_rep %>%
  group_by(gid, group_id) %>%
  mutate(prop_viol = ifelse(is.na(prop_viol), 0, prop_viol)) %>%
  mutate(lag_viol = dplyr::lag(prop_viol, n = 1)) %>%
  mutate(chg_viol = prop_viol - lag_viol) %>%
  ungroup()



# save(acled_viol, file = "acled_260220.RData")
# load("acled_260220.RData")

# POLITICAL OPPORTUNITY STRUCTURE (REIGN DATA)

## REIGN DATA

reign <- read.csv("C:/Users/villar/Dropbox/MA_Vilde/reign/REIGN_2020_3.csv", stringsAsFactors = FALSE)

reign_cleaned <- reign %>%
  mutate(country = ifelse(country == "Swaziland", "eSwatini", 
                          ifelse(country == "Cen African Rep", "Central African Republic", 
                                 ifelse(country == "Congo/Zaire", "Democratic Republic of Congo",
                                        ifelse(country == "Congo-Brz", "Republic of Congo",
                                               ifelse(country == "Guinea Bissau", "Guinea-Bissau", country)))))) %>% 
  filter(year >= 2009, country %in% acled_viol$country) %>% 
  mutate(month = as.Date(paste(year, month, "01", sep = "-"))) %>% 
  select(country, month, elected, tenure_months) %>%
  distinct(country, month, elected, tenure_months, .keep_all = T) %>%
  group_by(country, month) %>%
  slice(which.min(tenure_months)) %>% # In cases of duplicate rows, leader change occurred during the month, choose the one with the lowest number (will be lagged)
  ungroup() %>%
  distinct(country, month, .keep_all = TRUE) %>%
  mutate(tenure_lag = lag(tenure_months, n = 1),
         elected_lag = lag(elected, n = 1))



acled_full <- left_join(acled_viol, reign_cleaned, by = c("country", "month")) %>% 
  filter(break_week != 1) %>%
  select(prot_cycle = group_id, gid, week, month, year, country, 
         chg_viol, prop_viol, start = start_week_two, end = end_week_two,
         nonviolent_freq, violent_freq, interv_freq, exforc_freq, repviol_freq, total_prot_ev, 
         prop_organized, fatalities, duration, prot_length,
         lag_viol, prev_interv, prev_exforc, prev_repviol,
         interv_lag, exforc_lag, repviol_lag,
         interv_dummy_lag, exforc_dummy_lag, repviol_dummy_lag,
         gdp_cap, ttime_avg, time_since_past_protest, libdem = v2x_libdem, libdem_sq,
         foodprice_ch = ch_fp, unemp_rate, tenure_lag, elected_lag)


# NEIGHBORING GRID CELLS

acled_gids <- acled_full %>%
  dplyr::select(gid) %>%
  dplyr::distinct(gid)

tst <- priogrid::pgneighbors_v(acled_gids$gid, ncol = 720, nrow = 360, asmat = FALSE)

longer <- as.data.frame(tst) %>%
  pivot_longer(cols = V1:V980,
               values_to = "nbs",
               names_to = "group") %>%
  group_by(group) %>%
  summarize(neighbors = paste(sort(unique(nbs)), collapse = ", ")) %>%
  ungroup()

ac_gid <- acled_gids %>%
  mutate(name = 1:nrow(acled_gids)) %>%
  pivot_wider(values_from = gid,
              names_from = name,
              names_prefix = "V") %>%
  pivot_longer(cols = V1:V980,
               values_to = "gid",
               names_to = "group")

nbs <- merge(ac_gid, longer, by = "group")
nbs$group <- NULL

# save(nbs, file = "C:/Users/villar/Dropbox/MA_Vilde/R/pgneighbors.RData")

# load("pgneighbors.RData")

acled_nbs <- acled_full %>%
  left_join(nbs, by = "gid")

acled_nbs1 <- acled_nbs %>%
  select(prot_cycle, gid, start, end, neighbors, violent_freq, prop_viol)

acled_nbs2 <- acled_nbs1 %>%
  separate_rows(neighbors, sep = ", ", convert = TRUE) %>%
  left_join(acled_nbs1 %>% select(-neighbors),
            by = c("neighbors" = "gid")) %>%
  distinct() %>%
  filter(!is.na(prot_cycle.y),
         prop_viol.y > 0) %>%
  filter(int_overlaps(interval(start.x, end.x),
                      interval(start.y, end.y))) %>% # Deleting non-overlapping
  mutate(spatial_diff = ifelse(start.x < start.y, 1, 0)) %>%
  mutate(spatial_diff_start = ifelse(start.x <= start.y, format(as.Date(start.y, format = "%Y-%m-%d"), "%Y-%m-%d"), NA)) %>%
  mutate(spatial_diff_end = ifelse(start.x <= start.y, format(as.Date(end.y, format = "%Y-%m-%d"), "%Y-%m-%d"), NA)) %>%
  rename(prot_cycle = prot_cycle.x,
         start = start.x,
         end = end.x,
         nb_start = start.y,
         nb_end = end.y)


acled <- left_join(acled_full, acled_nbs2, by = c("gid", "prot_cycle", "start", "end"))

acled_fin <- acled %>%
  filter(!is.na(nb_start)) %>%
  mutate(ongoing = ifelse(nb_start <= week & nb_end >= week, 1, 0)) %>%
  select(gid, prot_cycle, week, start, end, nb_start, nb_end, prot_cycle.y, ongoing, spatial_diff, spatial_diff_start, spatial_diff_end, violent_freq.y, prop_viol.y) %>%
  filter(ongoing == 1) %>%
  group_by(gid, prot_cycle, week) %>%
  summarize(n_ongoing_nbs = n_distinct(prot_cycle.y)) %>% # Number of neighboring grid cells with ongoing protest 
  ungroup() %>%
  right_join(acled_full, by = c("gid", "prot_cycle", "week"))  %>%
  mutate(n_ongoing_nbs = ifelse(is.na(n_ongoing_nbs), 0, n_ongoing_nbs)) %>%
  group_by(gid, prot_cycle) %>%
  mutate(n_ongoing_lag = lag(n_ongoing_nbs, n = 1)) %>%
  mutate(diffusion = n_ongoing_nbs - n_ongoing_lag) %>% # Spread of violent protest from last week to current week
  mutate(diffusion_lag = lag(diffusion, n = 1)) %>%  # Spread of violent protest, lagged one week 
  ungroup() %>%
  mutate(gdp_cap_log = log(gdp_cap))

acled_fin$duration_sq <- acled_fin$duration^2
acled_fin$ttime_avg_log <- log(acled_fin$ttime_avg)

# save(acled_fin, file = "acled_290320.RData")


# load("R/acled_290320.RData")


acled_fin <- acled_fin %>%
  group_by(prot_cycle) %>%
  mutate(total_prot_ev_lag = lag(total_prot_ev, n = 1),
         prop_viol_lag = lag(prop_viol, n = 1)) %>%
  ungroup()

# Remove missing values, key variables
# Clean dataset to use as pred df
acled_cleaned <- acled_fin %>% 
  filter(!is.na(chg_viol),
         !is.na(interv_lag), 
         !is.na(exforc_lag),
         !is.na(repviol_lag),
         !is.na(prop_organized), 
         !is.na(duration), 
         !is.na(duration_sq),
         !is.na(n_ongoing_lag), 
         !is.na(diffusion), 
         !is.na(lag_viol), 
         !is.na(gdp_cap_log), 
         !is.na(ttime_avg_log),
         !is.na(libdem), 
         !is.na(libdem_sq), 
         !is.na(tenure_lag), 
         !is.na(elected_lag), 
         !is.na(unemp_rate),
         !is.na(total_prot_ev_lag))

# Repression variables as proportion of total events

acled_cleaned <- acled_cleaned %>%
  mutate(interv_lag_prop = interv_lag/total_prot_ev_lag,
         exforc_lag_prop = exforc_lag/total_prot_ev_lag,
         repviol_lag_prop = repviol_lag/total_prot_ev_lag,
         duration_log = log(duration),
         region = ifelse(country == "Bangladesh" | country == "Myanmar" | country == "Nepal" | country == "Pakistan" | country == "Thailand" | country == "Sri Lanka" |
                           country == "Cambodia" | country == "Vietnam", "Asia", "Africa")) 

acled_cleaned <- acled_cleaned %>%
  group_by(prot_cycle) %>%
  mutate(lag_dv = lag(chg_viol, n = 1)) %>%
  ungroup()

# save(acled_cleaned, file = "R/acled_210420.RData") # Final dataset

