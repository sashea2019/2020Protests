### City Check with Google API
library(ggmap)

# AIzaSyD9VCokeOwl_UROkqeXPNGkZTsU4HUd8Cg

# For ggmap
#register_google(key = "AIzaSyD9VCokeOwl_UROkqeXPNGkZTsU4HUd8Cg")

# For mapsapi (store key in a variable)
#google_key <- "AIzaSyD9VCokeOwl_UROkqeXPNGkZTsU4HUd8Cg"

non_joined.ccc.acs <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/non_joined.ccc.acs.csv")
non_joinedmapping.acs.csv <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/non_joinedmapping.acs.csv")




register_google(key = <Google API Key>)

#Get a data frame of distinct locations:
  
#  library(ggmap)
#library(dplyr)


# get the distinct locations from the dataset and create a data frame
locations_txt <- distinct(non_joined.ccc.acs, City)
locations_txt <- as.data.frame(locations_txt)

# Test location extraction
loc_text3 <- as.data.frame(locations_txt[1:4,])

locations_geo <- mutate_geocode(loc_text3, locations_txt[1:4, ])


