##########
## MAPS ##
##########

# Load packages
library(cshapes)
library(sf)
library(RColorBrewer)
library(tmap)
library(lubridate)
library(tidyverse)
library(raster)

# Load dataset
load("C:/Users/villar/Dropbox/MA_Vilde/R/acled_210420.RData")

## Figure 1: Protest events in South Africa
world <- cshapes::cshp(date = as.Date("2015-01-01"))

acled_full <- read.csv("C:/Users/villar/Dropbox/MA_Vilde/ACLED/1997-01-01-2019-12-31-Eastern_Africa-Middle_Africa-Northern_Africa-Southern_Africa-Western_Africa.csv", stringsAsFactors = FALSE) # Load full protest data
sa_full <- acled_full %>% 
  filter(country == "South Africa" & year == 2015) 
sa_full <- sa_full %>% 
  mutate(event_date = as.Date(sa_full$event_date, "%d %B %Y")) %>%
  mutate(week = floor_date(event_date, unit = "week", week_start = 1)) %>% 
  filter(week == "2015-10-12" | week == "2015-10-19" | week == "2015-10-26") %>% 
  filter(sub_event_type != "Mob violence") %>%
  mutate(event = ifelse(event_type == "Protests", "Peaceful protest", "Violent protest"))

southa_chsp <- world[world$COWCODE == 560, ]
southa_chsp1 <- crop(southa_chsp, extent(16, 37, -40, -20))

fmf <- sa_full %>%
  group_by(longitude, latitude, event, week) %>%
  dplyr::summarize(events = n()) %>%
  ungroup() %>%
  sf::st_as_sf(coords = c("longitude", "latitude")) %>%
  sf::st_set_crs(value = 4032) 
max_size <- max(fmf$events)

# First week
tm_shape(southa_chsp1) + tm_fill(col = "gray99") + tm_borders(col = "gray70") + tm_layout(frame = FALSE) +
  tm_shape(fmf[fmf$event == "Peaceful protest" & fmf$week == "2015-10-12", ]) + tm_bubbles(size = "events", col = "cyan4", title.size = "Nonviolent protests", size.max = max_size, legend.size.show = FALSE) +
  tm_shape(fmf[fmf$event == "Violent protest" & fmf$week == "2015-10-12", ]) + tm_symbols(size = "events", col = "coral", shape = 8, title.size = "Violent protests", size.max = max_size, legend.size.show = FALSE) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.5, legend.frame = F, legend.stack = "horizontal", legend.outside = TRUE, legend.outside.position = "bottom",
            title = "Oct. 12-18, 2015", title.size = 1, fontfamily = "serif")

# Second week
tm_shape(southa_chsp1) + tm_fill(col = "gray99") + tm_borders(col = "gray70") + tm_layout(frame = FALSE) + 
  tm_shape(fmf[fmf$event == "Peaceful protest" & fmf$week == "2015-10-19", ]) + tm_bubbles(size = "events", col = "cyan4", title.size = "Nonviolent protests", size.max = max_size, legend.size.show = FALSE) +
  tm_shape(fmf[fmf$event == "Violent protest" & fmf$week == "2015-10-19", ]) + tm_symbols(size = "events", col = "coral", shape = 8, title.size = "Violent protests", size.max = max_size, legend.size.show = FALSE) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.5, legend.frame = F, legend.stack = "horizontal", legend.outside = TRUE, legend.outside.position = "bottom",
            title = "Oct. 19-25, 2015", title.size = 1, fontfamily = "serif")

# Third week
tm_shape(southa_chsp1) + tm_fill(col = "gray99") + tm_borders(col = "gray70") + tm_layout(frame = FALSE) + 
  tm_shape(fmf[fmf$event == "Peaceful protest" & fmf$week == "2015-10-26", ]) + tm_bubbles(size = "events", col = "cyan4", title.size = "Nonviolent protests", size.max = max_size, legend.size.show = F) +
  tm_shape(fmf[fmf$event == "Violent protest" & fmf$week == "2015-10-26", ]) + tm_symbols(size = "events", col = "coral", shape = 8, title.size = "Violent protests", size.max = max_size, legend.size.show = F) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.5, legend.frame = F, legend.stack = "horizontal", legend.outside = TRUE, legend.outside.position = "bottom",
            title = "Oct. 26-Nov. 1, 2015", title.size = 1, fontfamily = "serif")

# Legend
tm_shape(southa_chsp1) + tm_fill(col = "gray99") + tm_borders(col = "gray70") + tm_layout(frame = FALSE) +
  tm_shape(fmf[fmf$event == "Peaceful protest" & fmf$week == "2015-10-12", ]) + tm_bubbles(size = "events", col = "cyan4", title.size = "Nonviolent protests", size.max = max_size) +
  tm_shape(fmf[fmf$event == "Violent protest" & fmf$week == "2015-10-12", ]) + tm_symbols(size = "events", col = "coral", shape = 8, title.size = "Violent protests", size.max = max_size) +
  tm_layout(legend.bg.color = "white", legend.bg.alpha = 0.5, legend.frame = F, legend.stack = "vertical", legend.outside = F, legend.position = c("center", "center"),
            legend.only = TRUE, fontfamily = "serif")


## Figure 2: Protest events in Africa, 1997-2018

africa9719 <- read.csv("ACLED/1997-01-01-2019-12-31-Eastern_Africa-Middle_Africa-Northern_Africa-Southern_Africa-Western_Africa.csv", stringsAsFactors = FALSE) # Download and load full dataset from 1997-2019
africa9719 <- africa9719 %>%
  filter(str_detect(region, "Africa")) %>%
  filter(sub_event_type != "Mob violence") %>%
  mutate(long = longitude, lat = latitude) %>%
  mutate(sub_event_type = factor(sub_event_type, 
                                 levels = c("Peaceful protest", 
                                            "Protest with intervention", 
                                            "Excessive force against protesters",
                                            "Violent demonstration"))) %>%
  filter(year != 2019)

palette <- tmaptools::get_brewer_pal("GnBu", n = 4, contrast = c(0.35, 1))

freq_plot <- ggplot(africa9719, aes(x = year, fill = sub_event_type)) +
  geom_bar(position = "stack") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()) +
  scale_x_continuous(breaks = c(1997:2018)) +
  scale_fill_manual(values = palette) +
  xlab("Year") + ylab("Number of events") +
  labs(fill = "Type of event") +
  theme(axis.line = element_line(color = "gray")) +
  scale_y_continuous(expand = c(0,0), breaks = c(0, 1000, 2000, 3000, 4000, 5000, 6000)) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  guides(fill = guide_legend(nrow = 2)) 


## Figure 3: Number of protest weeks, Africa
world <- cshapes::cshp(date = as.Date("2015-01-01"))

list_africa <- c(402:626, 651)
africa_cshp <- world[world$COWCODE %in% list_africa, ]

clvl_af <- acled_cleaned %>%
  dplyr::filter(region == "Africa") %>% 
  dplyr::group_by(country) %>%
  dplyr::summarize(n_cycles = n_distinct(prot_cycle),
                   n_weeks = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(country = ifelse(country == "Democratic Republic of Congo", "Congo, DRC", 
                                 ifelse(country == "Republic of Congo", "Congo",
                                        ifelse(country == "Gambia", "The Gambia",
                                               ifelse(country == "eSwatini", "Swaziland", 
                                                      ifelse(country == "Ivory Coast", "Cote d'Ivoire", country))))))

map_clvl_af <- merge(africa_cshp, clvl_af, by.x = "CNTRY_NAME", by.y = "country") 

tmap::tm_shape(africa_cshp) + tmap::tm_fill(col = "gray99") + tmap::tm_borders(col = "gray66") + tmap::tm_layout(frame = FALSE) + 
  tmap::tm_shape(map_clvl_af) + tmap::tm_fill(col = "n_weeks", palette = c("lightcyan", "darkslategray3", "cyan4", "darkslategray", "gray15"),
                                              breaks = c(1, 20, 100, 200, 500, 700, 1000, 8000, Inf),
                                              colorNA = "white",
                                              title = "Number of protest weeks",
                                              textNA = "Not included in sample") + tmap::tm_borders() +
  tmap::tm_layout(legend.outside = TRUE, legend.outside.position = "left", fontfamily = "serif")


# Figure 4: Number of protest weeks, Asia
list_asia <- c(750:817)
asia_cshp <- world[world$COWCODE %in% list_asia, ]

clvl_as <- acled_cleaned %>% 
  filter(region == "Asia") %>% 
  dplyr::group_by(country) %>%
  dplyr::summarize(n_cycles = n_distinct(prot_cycle),
                   n_weeks = n()) %>%
  dplyr::ungroup() 

map_clvl_as <- merge(asia_cshp, clvl_as, by.x = "CNTRY_NAME", by.y = "country")

tmap::tm_shape(asia_cshp) + tmap::tm_fill(col = "gray99") + tmap::tm_borders(col = "gray66") + tmap::tm_layout(frame = FALSE) + 
  tmap::tm_shape(map_clvl_as) + tmap::tm_fill(col = "n_weeks", palette = c("lightcyan", "darkslategray3", "cyan4", "darkslategray", "gray15"),
                                              colorNA = "white",
                                              title = "Number of protest weeks",
                                              breaks = c(1, 20, 100, 200, 500, 700, 1000, 8000, Inf),
                                              textNA = "Not included in sample") + tmap::tm_borders() +
  tmap::tm_layout(legend.outside = TRUE, legend.outside.position = "left", fontfamily = "serif") 

# Figure 5: Protest cycles, Africa
total_ev_africa <- acled_cleaned %>%
  dplyr::filter(region == "Africa") %>%
  dplyr::group_by(gid) %>%
  dplyr::summarize(n_cycles = n_distinct(prot_cycle),
                   n_weeks = n()) %>%
  dplyr::ungroup() 

pg_shp <- sf::st_read("C:/Users/villar/Dropbox/MA_Vilde/PRIO-GRID/priogrid_cell.shp")

pg_totev_af <- merge(pg_shp, total_ev_africa, by = "gid")

max_legend <- max(c(pg_totev_af$n_cycles, pg_totev_as$n_cycles))

tmap::tm_shape(africa_cshp) + tmap::tm_fill(col = "gray99") + tmap::tm_borders(col = "gray70") + tmap::tm_layout(frame = FALSE) + 
  tmap::tm_shape(pg_totev_af) + tmap::tm_polygons(col = "n_cycles", border.alpha = 0, palette = c("cyan4", "darkolivegreen2", "coral", "red"), 
                                                  title = "Number of protest cycles", legend.hist = F, breaks = seq(1, max_legend, by = 5)) + 
  tmap::tm_layout(legend.outside = TRUE, legend.outside.position = "left", fontfamily = "serif")

# Figure 6: Protest cycles, Asia

total_ev_asia <- acled_cleaned %>%
  filter(region == "Asia") %>%
  group_by(gid) %>%
  summarize(n_cycles = n_distinct(prot_cycle),
            n_weeks = n()) %>%
  ungroup() 

pg_totev_as <- merge(pg_shp, total_ev_asia, by = "gid") 

tmap::tm_shape(asia_cshp) + tmap::tm_fill(col = "gray99") + tmap::tm_borders(col = "gray70") + tmap::tm_layout(frame = FALSE) + 
  tmap::tm_shape(pg_totev_as) + tmap::tm_polygons(col = "n_cycles", border.alpha = 0, palette = c("cyan4", "darkolivegreen2", "coral", "red"), 
                                                  title = "Number of protest cycles", legend.hist = F, breaks = seq(1, max_legend, by = 5)) +
  tmap::tm_layout(legend.outside.position = "left", legend.outside = TRUE, fontfamily = "serif")