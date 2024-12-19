
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
bbox <- c(ymin = 50.454578, xmin = -3.647665, ymax = 51.551322, xmax = -0.886754))

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

# Make sure all date ranges are the same.

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
                        SMART_Site = case_when(
                                            SITE_ID == 43407 ~ "Borough Bridge",
                                            SITE_ID == 194671 ~ "Pinglestone",
                                            SITE_ID == 43091 ~ "Itchen St Cross",
                                            SITE_ID == 158095 ~ "Broadlands",
                                            SITE_ID == 44146 ~ "Ironbridge"
                                        )
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

ggplot()+geom_line(data=EA_T1, aes(x = SAMPLE_DATE, y= BMWP_N_TAXA), col= "seagreen")+ 
  geom_line(data=SMART_T1, aes(x = Recorded..Date, y = NTAXA), col = "blue")+ facet_wrap(~Site)

# PSI ?

ggplot()+geom_line(data=EA_T1, aes(x = SAMPLE_DATE, y= PSI), col= "seagreen")+ 
  geom_line(data=SMART_T1, aes(x = Recorded..Date, y = EPSI_MIXED_LEVEL_SCORE), col = "blue")+ facet_wrap(~Site)





leaflet() %>% addTiles() %>% 
  addCircleMarkers(data= EA_T1,
                   col = "green",
                   popup = ~SMART_Site) %>% 
  addCircleMarkers(data= SMART_T1,
                   col = "blue",
                   popup = ~Site) %>% 
  addScaleBar()





# Find EA sites within 350m of SMART using st_within_distance()
within_distances <- st_is_within_distance(EA_Invert, SMART, dist = 350)

# Convert the result of st_within_distance into a flat data frame using purrr::map_dfr()
results_df <- map_dfr(seq_along(within_distances), ~{
  tibble(
    SMART_index = .x,
    df_sf_index = within_distances[[.x]]
  )
})

# Step 5: Isolate matched ARMI and df_sf sites within 50m using the indices
SMART_Twin <- SMART[results_df$SMART_index, ]
EA_Twin <- EA_Invert[results_df$df_sf_index, ]
