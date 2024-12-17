
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
  st_as_sf(coords = c("EASTING", "NORTHING"), crs=27700) %>% 
  st_transform(4326) 

# Convert SMART to sf object

SMART %<>% st_as_sf(coords = c("Location..Latitude", "Location..Longitude"), crs=(4326)) 

# Find ARMI sites within 50m of df_sf using st_within_distance()
within_distances <- st_is_within_distance(EA_Invert, SMART, dist = 500)

# Step 4: Convert the result of st_within_distance into a flat data frame using purrr::map_dfr()
results_df <- map_dfr(seq_along(within_distances), ~{
  tibble(
    ARMI_index = .x,
    df_sf_index = within_distances[[.x]]
  )
})

# Step 5: Isolate matched ARMI and df_sf sites within 50m using the indices
ARMI_Twin <- SMART[results_df$ARMI_index, ]
EA_Twin <- EA_Invert[results_df$df_sf_index, ]