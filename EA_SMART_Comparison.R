
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

#write.csv(merge, "C:/Users/hg000051/Downloads/Per_Survey_Data.csv")

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

#Matt's Tier 1 sites

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

# Isolate determinants to plot
    deters <- tail(names(EA_T1), n=7)
    culr <- c("EA" = "seagreen", "SMART" = "blue" )

# Plot each matching determinant     
for (y in deters){    
  
  lm_ea <- lm(get(y) ~ Date , data= EA_T1)
  
  lm_smart <- lm(get(y) ~ Date , data= SMART_T1)
  
  # Get fitted values
  pred_ea <- broom::augment(lm_ea) %>% mutate(Survey = "EA")
  pred_smart <- broom::augment(lm_smart) %>% mutate(Survey = "SMART")
  
  
   a <-  ggplot() +
      geom_point(data=EA_T1, aes(x = Date, y = get(y), color = "EA")) +
      geom_line(data=EA_T1, aes(x = Date, y = get(y), color = "EA")) +
     geom_line(data=pred_ea, aes(x= Date, y=.fitted), col="seagreen2", linetype = "dashed")+
      geom_point(data=SMART_T1, aes(x = Date, y = get(y), color = "SMART")) +
      geom_line(data=SMART_T1, aes(x = Date, y = get(y), color = "SMART")) +
      geom_line(data=pred_smart, aes(x= Date, y=.fitted), col="steelblue", linetype = "dashed")+
      scale_colour_manual(values = culr) +
      facet_wrap(~SMART_Site) + 
      labs(title = paste0(y, " Test & Itchen SMART - EA Riverfly Sites"),
           y = y,
           color = "Survey") + 
      theme(plot.title = element_text(hjust = 0.5))+
      theme_bw()
   

   # Only calculates a linear regression for 1 site then puts it on all sites.
   
   ggsave(filename = paste0(y,".pdf"), plot = a)
   
   combined <- bind_rows(
     EA_T1 %>% mutate(Survey = "EA"),
     SMART_T1 %>% mutate(Survey = "SMART")
   )
   
   lm_compare <- lm(get(y) ~ Date * Survey, data = combined)
   print(summary(lm_compare)$coefficients)
   
  }

    
    ggplot() +
      geom_point(data=EA_T1, aes(x = Date, y = get(y), color = "EA")) +
      geom_line(data=EA_T1, aes(x = Date, y = get(y), color = "EA")) +
      geom_smooth(data=EA_T1, aes(x= Date, y = get(y), col="red"), method = "lm")+
      geom_point(data=SMART_T1, aes(x = Date, y = get(y), color = "SMART")) +
      geom_line(data=SMART_T1, aes(x = Date, y = get(y), color = "SMART")) +
      geom_smooth(data=SMART_T1, aes(x= Date, y = get(y), col="blue"), method = "lm")
    scale_colour_manual(values = culr) +
      facet_wrap(~SMART_Site) + 
      labs(title = paste0(y, " Test & Itchen SMART - EA Riverfly Sites"),
           y = y,
           color = "Survey") + 
      theme(plot.title = element_text(hjust = 0.5))+
      theme_bw()
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
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
            by = 'Date')


fuzzy_match <- fuzzy_left_join(
  EA_T2,
  SMART_T2,
  by = c("Site" = "Site", "Date" = "Date"),
  match_fun = list(`==`, function(x, y) abs(difftime(x, y, units = "days")) <= 7)
)%>%
  # Change names to match dfs
  rename_with(~ gsub("\\.x$", "_ea", .x), ends_with(".x")) %>%
  rename_with(~ gsub("\\.y$", "_smart", .x), ends_with(".y"))


icc_data <- fuzzy_match %>%
  filter(
    !is.na(WHPT.ASPT_ea), 
    !is.na(WHPT.ASPT_smart),
    Site_ea == Site_smart,       # Ensure exact site match
    Date_ea == Date_smart        # Ensure exact date match (or use a tolerance if needed)
  ) %>%
  select(Site_ea, Date_ea, WHPT.ASPT_ea, WHPT.ASPT_smart)

icc_result_psi <- ICC(fuzzy_match %>% select(PSI_ea, PSI_smart))
icc_result_aspt <- ICC(fuzzy_match %>% select(ASPT_ea, ASPT_smart))
icc_result_ntaxa <- ICC(fuzzy_match %>% select(NTAXA_ea, NTAXA_smart))
icc_result_life <- ICC(fuzzy_match %>% select(LIFE_ea, LIFE_smart))
icc_result_whpt.aspt <- ICC(fuzzy_match %>% select(Site_ea, Date_ea, WHPT.ASPT_ea, WHPT.ASPT_smart))

icprint(icc_result)

##############################################################################################
# Merge datasets to we can compare them statistically 

EA_T2 <-  EA_T1 %>% 
  mutate(SAMPLE_DATE = dmy(SAMPLE_DATE),
         Site = SMART_Site) %>% 
  select(Site, Date, BMWP, ASPT, NTAXA, WHPT, WHPT.ASPT, PSI, LIFE) %>% 
  st_drop_geometry()


SMART_T2 <-   SMART_T1 %>% 
  mutate(Date = ymd(Recorded..Date),
         Time = Recorded..Time) %>% 
  select(Site, Date, Recorded..Time, BMWP, ASPT, NTAXA, WHPT, WHPT.ASPT, PSI, LIFE) %>% 
  st_drop_geometry()


model = lm(ASPT ~ Date, data=)
model <- lm(ASPT ~ Year * Surveyor, data = df)
summary(model)


#compare across multiple sites
library(lme4)
model <- lmer(ASPT ~ Year * Surveyor + (1|Site), data = df)
summary(model)





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


