### City Check with Google API
library(ggmap)
library(dplyr)

# Shea's Google Maps API Key: AIzaSyD9VCokeOwl_UROkqeXPNGkZTsU4HUd8Cg

# For mapsapi (store key in a variable)
google_key <- "AIzaSyD9VCokeOwl_UROkqeXPNGkZTsU4HUd8Cg"

# For ggmap
register_google(key = google_key)



#Since we each have different usernames and file names why don't we each set our own working directories and only pull the file names instead of the full path when reading CSV
#What do you think?

directory_shea<- "C:/Users/sashea/Dropbox (Personal)/Academic/Projects/BLM Tweets/2020 Protest Shea Analysis/2020Protests"
directory_arica <- "/Users/aricaschuett/Documents/protest/Shea + Arica"
setwd(directory_shea)

# non_joined.ccc.acs <- read.csv("non_joined.ccc.acs.csv")
# non_joinedmapping.acs.csv <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/non_joinedmapping.acs.csv")



# #Get a data frame of distinct locations:
#   
# #  library(ggmap)
# #library(dplyr)
# 
# 
# # get the distinct locations from the dataset and create a data frame
# locations_txt <- distinct(non_joined.ccc.acs, City)
# locations_txt <- as.data.frame(locations_txt)
# 
# # Test location extraction
# loc_text3 <- as.data.frame(locations_txt[1:4,])
# 
# locations_geo <- mutate_geocode(loc_text3, locations_txt[1:4, ])


#Add latitude and longitude information to mapping data

mapping<- read.csv("Mapping Police Violence.csv")
names(mapping)

#Create a variable that has the street adress, city, state and zip code of the police killing location
mapping$full_location<- paste(mapping$street_address, mapping$city, mapping$state, mapping$zip, sep = " ")


# Create a 5-digit ID for each observation
mapping$id <- sprintf("%05d", seq_along(mapping$full_location))


# Initialize an empty data frame to store errors
error_log <- data.frame(
  id = character(),
  location = character(),
  error_message = character(),
  stringsAsFactors = FALSE
)

# Function to safely geocode a location and append errors to the error log
safe_geocode <- function(location, id) {
  safe_result <- tryCatch({
    result <- geocode(location)
    return(result)
  }, error = function(e) {
    # Append to error log
    error_log <<- rbind(error_log, data.frame(id = id, location = location, error_message = e$message, stringsAsFactors = FALSE))
    return(data.frame(lon = NA, lat = NA))
  })
  return(safe_result)
}

# Use the function to get lat/lon for each full_location and pass the ID
mapping_latlon <- do.call(rbind, mapply(
  safe_geocode,
  location = mapping$full_location,
  id = mapping$id,
  SIMPLIFY = FALSE
))

# Combine the lat/lon data with the original data
mapping <- cbind(mapping, mapping_latlon)

# View the updated mapping data frame
print(mapping)

# View the error log data frame
print(error_log)


