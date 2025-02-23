
library(tidyverse)
library(magrittr)
library(sf)
library(leaflet)

# Merge SMART Data
A <- read.csv("C:/Users/hg000051/Downloads/SmartRivers1921/Per-Survey Data.csv")
B <- read.csv("C:/Users/hg000051/Downloads/SmartRivers2123/Per-Survey Data.csv")
C <- read.csv("C:/Users/hg000051/Downloads/SmartRivers2324/Per-Survey Data.csv")

SMART <- rbind(A,B,C)

#write.csv(merge, "C:/Users/hg000051/Downloads/Per_Survey_Data.csv")

# Import EA Data associated with SMART sites

# Bounding box we chose to filter SMART data by we also filter EA data.
bbox <- c(ymin = 50.454578, xmin = -3.647665, ymax = 51.551322, xmax = -0.886754)

st_sf(geometry = st_sfc(square_polygon, crs = 4326))


bbox_sf <- st_as_sfc(st_bbox(bbox, crs = st_crs(4326)))
bbox_sf <- st_sf(geometry = bbox_sf) 

EA_Invert1 <- read.csv("C:/Users/hg000051/Downloads/INV_OPEN_DATA (5)/INV_OPEN_DATA_SITE.csv") 
EA_Invert2 <- read.csv("C:/Users/hg000051/Downloads/INV_OPEN_DATA (5)/INV_OPEN_DATA_METRICS.csv")

EA_Invert <- inner_join(EA_Invert1, EA_Invert2, by = 'SITE_ID') %>% 
  st_as_sf(coords = c("FULL_EASTING", "FULL_NORTHING"), crs=27700) %>% 
  st_transform(4326) 


# Convert SMART to sf object
SMART %<>% st_as_sf(coords = c("Location..Longitude", "Location..Latitude"), crs=(4326)) 

#Matt's Tier 1 sites

# Make sure all date ranges are the same so we compare over the same time-window.

date_ranges <- SMART %>%
  group_by(Site) %>%
  summarize(
    start_date = min(Recorded..Date, na.rm = TRUE),
    end_date = max(Recorded..Date, na.rm = TRUE)
  )

# filter by site ID Matt OF provided.
# Change date format and create a new column with a matching site name for SMART so we link sites.
# filter so that sites dates match

EA_T1 <- EA_Invert %>% filter(SITE_ID %in% c(43407, 194671,43091, 
                                          158095, 44146)) %>% 
                      mutate(
                        SAMPLE_DATE = dmy(SAMPLE_DATE),
                        # Give EA sites matching SMART names from sites supplied by Matt O-F
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
                            SAMPLE_DATE >= date_ranges$start_date[match(SMART_Site, date_ranges$Site)] &
                              SAMPLE_DATE <= date_ranges$end_date[match(SMART_Site, date_ranges$Site)]
                          )


SMART_T1 <- SMART %>% filter(Site %in% c("Borough Bridge", "Pinglestone", 
                                       "Itchen St Cross", "Broadlands", "Ironbridge")) %>% 
                      mutate(
                        Recorded..Date = ymd(Recorded..Date),
                        SMART_Site = Site
                      )



# BMWP NTAXA
names(EA_T1)

ggplot()+geom_point(data=EA_T1, aes(x = SAMPLE_DATE, y= BMWP_N_TAXA), col= "seagreen")+
  geom_line(data=EA_T1, aes(x = SAMPLE_DATE, y= BMWP_N_TAXA), col= "seagreen")+ 
  geom_point(data=SMART_T1, aes(x = Recorded..Date, y = NTAXA), col = "blue")+
  geom_line(data=SMART_T1, aes(x = Recorded..Date, y = NTAXA), col = "blue")+ 
  facet_wrap(~SMART_Site)+
  theme_bw()+
  labs(title="BMWP Test & Itchen SMART - EA Riverfly Sites")+
  theme(plot.title = element_text(hjust = 0.5)) 


ggplot()+geom_point(data=EA_T1, aes(x = SAMPLE_DATE, y= BMWP_N_TAXA), col= "seagreen")+
  geom_line(data=EA_T1, aes(x = SAMPLE_DATE, y= BMWP_N_TAXA), col= "seagreen")+ 
  geom_point(data=SMART_T1, aes(x = Recorded..Date, y = NTAXA), col = "blue")+
  geom_line(data=SMART_T1, aes(x = Recorded..Date, y = NTAXA), col = "blue")+ 
  facet_wrap(~SMART_Site)+
  theme_bw()+
  labs(title="BMWP Test & Itchen SMART - EA Riverfly Sites")+
  theme(plot.title = element_text(hjust = 0.5)) 



# PSI ?

ggplot()+geom_line(data=EA_T1, aes(x = SAMPLE_DATE, y= PSI), col= "seagreen")+ 
  geom_line(data=SMART_T1, aes(x = Recorded..Date, y = EPSI_MIXED_LEVEL_SCORE), col = "blue")+ facet_wrap(~Site)


## Further filtering 
# Match EA sites to SMART sites where samples were taken on the same time 

# QA to make sure have right dates

# Check dates are same format

class(EA_T1$SAMPLE_DATE) == class(SMART_T1$Recorded..Date)

qq_1 <- EA_T1 %>% 
  st_drop_geometry() %>% 
  distinct() %>% 
  group_by(SMART_Site) %>% 
  left_join(SMART_T1 %>% st_drop_geometry(),
            by = c("SAMPLE_DATE" = 'Recorded..Date'))

omlete= EA_T1[EA_T1$SMART_Site == "Itchen St Cross",]
itch=SMART_T1[SMART_T1$SMART_Site == "Itchen St Cross",]

# Finds if dates match
common_dates <- intersect(omlete$SAMPLE_DATE, itch$Recorded..Date)
common_dates

library(fuzzyjoin)

fuzzy_match <- fuzzy_left_join(
  omlete, itch, 
  by = c("SAMPLE_DATE" = "Recorded..Date"),
  match_fun = function(x, y) abs(difftime(x, y, units = "days")) <= 7
)
fuzzy_match

Fuz <- fuzzy_match %>% filter(!is.na(VLOOKUP.Key))







sort(omlete$SAMPLE_DATE)
sort(itch$Recorded..Date)

egg <- distinct(omlete)


EA_T1_Dates <- EA_T1 %>%
  filter(SMART_Site %in% SMART_T1$SMART_Site & SAMPLE_DATE %in% SMART_T1$Recorded..Date)




leaflet() %>% addTiles() %>% 
  addCircleMarkers(data= EA_T1,
                   col = "green",
                   popup = ~SMART_Site) %>% 
  addCircleMarkers(data= SMART_T1,
                   col = "blue",
                   popup = ~Site) %>% 
  addScaleBar()





# Find EA sites within 350m of SMART data
within_distances <- st_is_within_distance(EA_Invert, SMART, dist = 350)

# Convert the result of st_within_distance into a flat data frame using purrr::map_dfr()
results_df <- map_dfr(seq_along(within_distances), ~{
  tibble(
    SMART_index = .x,
    df_sf_index = within_distances[[.x]]
  )
})

# Isolate matched ARMI and df_sf sites within 350m. There are repeats so remove these.
SMART_Twin <- distinct(SMART[results_df$SMART_index, ])
EA_Twin <- distinct(EA_Invert[results_df$df_sf_index, ])


leaflet() %>% addTiles() %>% 
  addCircleMarkers(data= EA_Twin,
                   col = "green",
                   popup = ~WATER_BODY) %>% 
  addCircleMarkers(data= SMART_Twin,
                   col = "blue",
                   popup = ~Site) %>% 
  addScaleBar()


