# Script authored by Harry Gray Summer 2025
# This script transforms EA and SMART riverfly data ready for analysis 


library(tidyverse)
library(magrittr)
library(sf)
library(leaflet)
library(fuzzyjoin)
library(psych)

# Merge SMART Data; can only be pulled of Cartographer in small chunks 
A <- read.csv("C:/Users/hg000051/Downloads/SmartRivers1921/Per-Survey Data.csv")
B <- read.csv("C:/Users/hg000051/Downloads/SmartRivers2123/Per-Survey Data.csv")
C <- read.csv("C:/Users/hg000051/Downloads/SmartRivers2324/Per-Survey Data.csv")

SMART <- rbind(A,B,C)

# Import EA Data associated with SMART sites

# Bounding box from SMART portal we chose to filter SMART data by we also filter EA data.
bbox <- c(ymin = 50.454578, xmin = -3.647665, ymax = 51.551322, xmax = -0.886754)

bbox_sf <- st_as_sfc(st_bbox(bbox, crs = st_crs(4326)))
bbox_sf <- st_sf(geometry = bbox_sf) 

EA_Invert1 <- read.csv("C:/Users/hg000051/Downloads/INV_OPEN_DATA (5)/INV_OPEN_DATA_SITE.csv") 
EA_Invert2 <- read.csv("C:/Users/hg000051/Downloads/INV_OPEN_DATA (5)/INV_OPEN_DATA_METRICS.csv")

# Join EA invert site and measures data to get full dataset.
EA_Invert <- inner_join(EA_Invert1, EA_Invert2, by = 'SITE_ID') %>% 
  st_as_sf(coords = c("FULL_EASTING", "FULL_NORTHING"), crs=27700) %>% 
  st_transform(4326) 


# Convert SMART to sf object
SMART %<>% st_as_sf(coords = c("Location..Longitude", "Location..Latitude"), crs=(4326)) 

#Matt Owen Farmers pre-selected Tier 1 sites

# Make sure all date ranges are the same so we compare over the same time-window,
# this minimises time as a dependent variable.

date_ranges <- SMART %>%
  group_by(Site) %>%
  summarize(
    start_date = min(Recorded..Date, na.rm = TRUE),
    end_date = max(Recorded..Date, na.rm = TRUE)
  )

# filter by site ID Matt OF provided.
# Change date format and create a new column with a matching site name for SMART so we link sites.
# filter so that site dates match

EA_T1 <- EA_Invert %>% filter(SITE_ID %in% c(43407, 194671,43091, 
                                             158095, 44146)) %>% 
  mutate(
    Date = dmy(SAMPLE_DATE),
    # Give EA sites matching SMART names from sites supplied by Matt O-F so comparable
    SMART_Site = case_when(
      SITE_ID == 43407 ~ "Borough Bridge",
      SITE_ID == 194671 ~ "Pinglestone",
      SITE_ID == 43091 ~ "Itchen St Cross",
      SITE_ID == 158095 ~ "Broadlands",
      SITE_ID == 44146 ~ "Ironbridge"
    ),
    # Rename variables so they match between SMART & EA
    BMWP = BMWP_TOTAL,
    NTAXA = BMWP_N_TAXA,
    ASPT = BMWP_ASPT,
    WHPT = WHPT_TOTAL,
    WHPT.ASPT = WHPT_ASPT,
    PSI = EPSI_MIXED_LEVEL_SCORE,
    CCI = CCI,
    LIFE = LIFE_SPECIES_INDEX
  ) %>% 
  filter(
    SMART_Site %in% date_ranges$Site, 
    Date >= date_ranges$start_date[match(SMART_Site, date_ranges$Site)] &
      Date <= date_ranges$end_date[match(SMART_Site, date_ranges$Site)]
  )


# Filter SMART data by sites Matt O-F selected based on surveyor/ datetime
SMART_T1 <- SMART %>% filter(Site %in% c("Borough Bridge", "Pinglestone", 
                                         "Itchen St Cross", "Broadlands", "Ironbridge")) %>% 
  mutate(
    Date = ymd(Recorded..Date),
    SMART_Site = Site
  )