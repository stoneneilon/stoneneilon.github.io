#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
##--##--##--##--##--##
##  Project Script  ## 
##--##--##--##--##--##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# Start Date: 12/11/24
# Class: American Core
# Project: "Your Political Network May Be Physically Defined - How The Built Environment Shapes Who You Discuss Politics With"
# Prof: Josh Strayhorn 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
##--##--##--##--##--##
##      Setup       ## 
##--##--##--##--##--##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
setwd("~/Stone_Website/research/final_core/data")

library(tidyverse)
library(stargazer)
library(foreign)
library(tidycensus)
library(haven)
#install.packages("tigris")
library(tigris)
#install.packages("sf")
library(sf)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
##--##--##--##--##--##
##   Data Cleaning  ## 
##--##--##--##--##--##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
#read data using haven package
data.18 <- read_sav("CCES18_UGA_OUTPUT_vv.sav")

#inputzip will be variable that geolocates respondant.
summary(data.18$inputzip) #462 NA zip code values

#Left with 300 obs dropping these NA values. 
data.18 <- data.18 %>% 
  drop_na(inputzip)

data.18$inputzip <- as.character(data.18$inputzip)
class(data.18$inputzip)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
##--##--##--##--##--##
##   Pol Het Code   ## 
##--##--##--##--##--##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# need to create new variable 'pol_hetero' 
# if respondent has heterogeneity in network ~ 1 
# if no heterogeneity ~ 0 
data.18 <- data.18 %>%
  mutate(
    # Recode invalid values as NA
    UGA319A = if_else(UGA319A %in% c(9, 98, 99), NA_real_, UGA319A),
    UGA319B = if_else(UGA319B %in% c(9, 98, 99), NA_real_, UGA319B),
    UGA319C = if_else(UGA319C %in% c(9, 98, 99), NA_real_, UGA319C)
  ) %>%
  rowwise() %>%
  mutate(
    # Combine valid responses into a list
    party_list = list(na.omit(c(UGA319A, UGA319B, UGA319C))),
    
    # Check for political heterogeneity
    pol_hetero = if_else(
      length(unique(party_list)) > 1,  # More than one unique party value
      1,                              # Assign 1 for heterogeneity
      0                               # Assign 0 for no heterogeneity
    )
  ) %>%
  ungroup() #%>%
# select(-party_list)  # Remove intermediate column
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
##--##--##--##--##--##
##    Pol Sorting   ## 
##--##--##--##--##--##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# Read in Brown & Enos data for zip code exposire to cross partisans. 
# need to figure out which measure to use. 
# whatever measure chosen - I need to combine the measure from the respective zip code and add to main dataframe 

load("relative-exposure-aggregate-data.Rdata")

# remove 'RFZIP_' from the zip codes. 
zip <- zip %>%
  mutate(zip = str_remove(zip, "^RFZIP_"))

# Now perform the left join
merged_data <- data.18 %>%
  left_join(zip, by = c("inputzip" = "zip"))

merged_data <- merged_data %>% 
  relocate(inputzip, .before = birthyr) %>% 
  relocate(pol_hetero,.before = birthyr) %>% 
  relocate(UGA319A, .before = birthyr) %>% 
  relocate(UGA319B, .before = birthyr) %>% 
  relocate(UGA319C, .before = birthyr) %>% 
  relocate(urbancity, .before = birthyr) %>% 
  relocate(pid3, .before = birthyr) %>% 
  relocate(pid7, .before = birthyr) %>% 
  relocate(ownhome, .before = birthyr)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
##--##--##--##--##--##
##Population Density## 
##--##--##--##--##--##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
# need to grab city classification from census 

census_api_key("de919ae9b9b9ee696bd99508214f0f31a980aad8", install = TRUE, overwrite = TRUE)

population_data_2018 <- get_acs(
  geography = "zcta",
  variables = c("B01003_001"),  # Total population
  year = 2018
)

zcta_shapes <- zctas(year = 2018)  # Get ZCTA shapes for 2018
zcta_shapes <- st_as_sf(zcta_shapes)  # Convert to an sf object (requires `sf` package)


# Calculate land area in square kilometers
zcta_shapes <- zcta_shapes %>%
  mutate(land_area_km2 = as.numeric(st_area(geometry)) / 1e6)


population_density_data <- population_data_2018 %>%
  select(GEOID, estimate) %>%  # Keep GEOID and population estimate
  rename(population = estimate) %>%
  left_join(zcta_shapes, by = c("GEOID" = "GEOID10"))  # Join with land area data


population_density_data <- population_density_data %>%
  mutate(population_density = population / land_area_km2)  # People per square kilometer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 
##--##--##--##--##--##
##Population Density## 
##--##--##--##--##--##
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# 

# Now perform the left join
final_merged_data <- merged_data %>%
  left_join(population_density_data, by = c("inputzip" = "GEOID")) 


final_merged_data$age <- (2018 - final_merged_data$birthyr)

final_merged_data <- as.data.frame(final_merged_data)
final_merged_data = data.frame(lapply(final_merged_data, as.character), stringsAsFactors=FALSE)

write.csv(final_merged_data, file = "final_paper_core_data.csv")

data$pid3
data$pid7



model3 <- lm(pol_hetero ~ population_density*d.exp.r + age + religpew + gender + race + educ + ownhome, data = final_merged_data)


