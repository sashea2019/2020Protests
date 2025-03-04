library(lubridate)
library(tidyverse)
library(tidycensus)
library(tigris)


######## Access Census Data #####
census_api_key("47674665e11654113f17b2b82d1a791a88f289b4", overwrite=TRUE)
acs_vars <- load_variables(2019, "acs5", cache = TRUE)

acs_data <- get_acs(geography = "place", 
                    variables = c(population = "B01003_001", 
                                  BlackPop = "B02001_003",
                                  BlackPov = "B17020B_001",
                                  Bachelors = "B15003_022", 
                                  Pop25Plus = "B15003_001",
                                  Masters = "B15003_023", 
                                  ProfDegree = "B15003_024", 
                                  Doctorate = "B15003_025",
                                  CollegeStudents = "B14001_008"),
                    year = 2019, #2015-2019 5-year ACS
                    survey = "acs5")
acs_data <- acs_data %>% dplyr::select(-moe)

################

acs_wide <- acs_data %>%
  pivot_wider(names_from = variable, values_from = estimate)


#acs_wide <- acs_wide %>%
#  filter( population > 30000)

any(is.na(acs_wide$population))
any(is.na(acs_wide$BlackPop))
sum(acs_wide$population) # 156,127,165
sum(acs_wide$BlackPop) # 25,497,289


ccc <- read.csv("/Users/aricaschuett/Downloads/ccc_compiled_20172020.csv")
ccc_sub <- ccc[ccc$date >= "2020-03-01" & ccc$date <= "2020-09-01", ]

# policing, race #starting keywords not patriotism Not Pro-police not anti-mask or anti
mapping <- read.csv("/Users/aricaschuett/Downloads/Mapping Police Violence.csv")

mapping <- mapping %>%
  filter(!grepl("Vehicle", cause_of_death))


ccc_sub <- ccc_sub %>%
  filter(grepl("racism", issues )) # I had trouble filtering on racism or policing, but I dont think this is necessary and think this gets what I need. 

ccc_sub <- ccc_sub %>%
  mutate(state = case_when(
    state == "AL" ~ "Alabama",
    state == "AK" ~ "Alaska",
    state == "AZ" ~ "Arizona",
    state == "AR" ~ "Arkansas",
    state == "CA" ~ "California",
    state == "CO" ~ "Colorado",
    state == "CT" ~ "Connecticut",
    state == "DC" ~ "District of Columbia",
    state == "DE" ~ "Delaware",
    state == "FL" ~ "Florida",
    state == "GA" ~ "Georgia",
    state == "HI" ~ "Hawaii",
    state == "ID" ~ "Idaho",
    state == "IL" ~ "Illinois",
    state == "IN" ~ "Indiana",
    state == "IA" ~ "Iowa",
    state == "KS" ~ "Kansas",
    state == "KY" ~ "Kentucky",
    state == "LA" ~ "Louisiana",
    state == "ME" ~ "Maine",
    state == "MD" ~ "Maryland",
    state == "MA" ~ "Massachusetts",
    state == "MI" ~ "Michigan",
    state == "MN" ~ "Minnesota",
    state == "MS" ~ "Mississippi",
    state == "MO" ~ "Missouri",
    state == "MT" ~ "Montana",
    state == "NE" ~ "Nebraska",
    state == "NV" ~ "Nevada",
    state == "NH" ~ "New Hampshire",
    state == "NJ" ~ "New Jersey",
    state == "NM" ~ "New Mexico",
    state == "NY" ~ "New York",
    state == "NC" ~ "North Carolina",
    state == "ND" ~ "North Dakota",
    state == "OH" ~ "Ohio",
    state == "OK" ~ "Oklahoma",
    state == "OR" ~ "Oregon",
    state == "PA" ~ "Pennsylvania",
    state == "RI" ~ "Rhode Island",
    state == "SC" ~ "South Carolina",
    state == "SD" ~ "South Dakota",
    state == "TN" ~ "Tennessee",
    state == "TX" ~ "Texas",
    state == "UT" ~ "Utah",
    state == "VT" ~ "Vermont",
    state == "VA" ~ "Virginia",
    state == "WA" ~ "Washington",
    state == "WV" ~ "West Virginia",
    state == "WI" ~ "Wisconsin",
    state == "WY" ~ "Wyoming",
    TRUE ~ NA_character_  # Handle missing or unrecognized abbreviations
  ))

mapping <- mapping %>%
  mutate(state = case_when(
    state == "AL" ~ "Alabama",
    state == "AK" ~ "Alaska",
    state == "AZ" ~ "Arizona",
    state == "AR" ~ "Arkansas",
    state == "CA" ~ "California",
    state == "CO" ~ "Colorado",
    state == "CT" ~ "Connecticut",
    state == "DC" ~ "District of Columbia",
    state == "DE" ~ "Delaware",
    state == "FL" ~ "Florida",
    state == "GA" ~ "Georgia",
    state == "HI" ~ "Hawaii",
    state == "ID" ~ "Idaho",
    state == "IL" ~ "Illinois",
    state == "IN" ~ "Indiana",
    state == "IA" ~ "Iowa",
    state == "KS" ~ "Kansas",
    state == "KY" ~ "Kentucky",
    state == "LA" ~ "Louisiana",
    state == "ME" ~ "Maine",
    state == "MD" ~ "Maryland",
    state == "MA" ~ "Massachusetts",
    state == "MI" ~ "Michigan",
    state == "MN" ~ "Minnesota",
    state == "MS" ~ "Mississippi",
    state == "MO" ~ "Missouri",
    state == "MT" ~ "Montana",
    state == "NE" ~ "Nebraska",
    state == "NV" ~ "Nevada",
    state == "NH" ~ "New Hampshire",
    state == "NJ" ~ "New Jersey",
    state == "NM" ~ "New Mexico",
    state == "NY" ~ "New York",
    state == "NC" ~ "North Carolina",
    state == "ND" ~ "North Dakota",
    state == "OH" ~ "Ohio",
    state == "OK" ~ "Oklahoma",
    state == "OR" ~ "Oregon",
    state == "PA" ~ "Pennsylvania",
    state == "RI" ~ "Rhode Island",
    state == "SC" ~ "South Carolina",
    state == "SD" ~ "South Dakota",
    state == "TN" ~ "Tennessee",
    state == "TX" ~ "Texas",
    state == "UT" ~ "Utah",
    state == "VT" ~ "Vermont",
    state == "VA" ~ "Virginia",
    state == "WA" ~ "Washington",
    state == "WV" ~ "West Virginia",
    state == "WI" ~ "Wisconsin",
    state == "WY" ~ "Wyoming",
    TRUE ~ NA_character_  # Handle missing or unrecognized abbreviations
  ))

ccc_sub$City<-paste(ccc_sub$locality, ccc_sub$state, sep=", ")
mapping$City<-paste(mapping$city, mapping$state, sep=", ")

mapping$City <- tolower(mapping$City)


ccc_sub <- ccc_sub %>% dplyr::select(date, City, issues, actors, claims, valence, 
                              size_low, size_high, size_mean, fips_code)
mapping <- mapping %>% dplyr::select(name, age, gender, race, date, City, 
                              cause_of_death, wapo_armed, wapo_threat_level, 
                              officer_known_past_shootings )

####  threat
mapping$wapo_threat_level[ mapping$wapo_threat_level=="Attack"|
                             mapping$wapo_threat_level=="Brandished Weapon" |
                             mapping$wapo_threat_level=="Sudden Threatening Movement" |
                             mapping$wapo_threat_level=="Used Weapon"]<-"High"   ### unclear if including brandishing or suddent threatening movement as HIGH threat

mapping$date <- mdy(mapping$date)

mappingBlackOnly <- mapping %>%
  filter(grepl("Black", race))
#mappingBlackOnly$date <- mdy(mappingBlackOnly$date)

# Date 
mappingPreGF <- mapping[mapping$date >= "2017-01-01" & mapping$date <= "2020-05-20", ] #started this in 2017
mappingPostGF <- mapping[mapping$date >= "2020-05-20" & mapping$date <= "2020-09-01", ] # During protest period

mappingBlackOnlyPreGF <- mappingBlackOnly[mappingBlackOnly$date >= "2017-01-01" & mappingBlackOnly$date <= "2020-05-20", ] #started this in 2017
mappingBlackOnlyPostGF <- mappingBlackOnly[mappingBlackOnly$date >= "2020-05-20" & mappingBlackOnly$date <= "2020-09-01", ] # During protest period

##### Clean --Name corrections #####
acs_wide <- acs_wide %>%
  rename(City = NAME)

acs_wide$City <- tolower(acs_wide$City)
ccc_sub$City <- tolower(ccc_sub$City)

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " city,", ","))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " city (balance),", ","))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " municipality,", ","))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " cdp,", ","))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "-davidson metropolitan government (balance), ", ", "))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "\\/jefferson county metro government \\(balance\\),", ",")) 

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " town,", ","))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "st\\. ", "saint "))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "st\\. ", "saint "))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "charter ", ""))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " borough,", ","))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " city \\(balance\\),", ",")) 

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "urban honolulu,", "honolulu,"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "waikiki,", "honolulu,"))
ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "south sacramento,", "sacramento,"))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "\\-fayette urban county,", ","))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " village,", ",")) 

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, " village,", ","))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " city and, ", ", "))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " city,", ","))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, " city,", ","))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, " township,", ","))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "\\-clarke county unified government \\(balance\\),", ","))
acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "\\-richmond county consolidated government \\(balance\\),", ","))
acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "\\-silver bow \\(balance\\),", ","))
acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "\\-davidson metropolitan government \\(balance\\),", ","))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "ogen,", "ogden,"))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "kailua cdp \\(hawaii county\\),", "kailua-kona,"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "kailua cdp \\(hawaii county\\),", "kailua-kona,"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "tallahasee,", "tallahassee,"))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "san buenaventura \\(ventura\\), ", "ventura, "))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, " town, ", " , ")) 

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " ,", ","))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, " ,", ",")) 

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "\\-bibb county,", ","))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "wasau,", "wausau,"))
ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, " center, ", ", "))
acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " center, ", ", "))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "west hollywood, ", "hollywood, "))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, " ,", ","))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "jamaica, ", "new york, "))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "long island, ", "new york, "))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "staten island, ", "new york, "))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "fairview cdp \\(westchester county\\), ", "westchester, "))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "greenville cdp \\(westchester county\\), ", "westchester, "))

acs_wide <- acs_wide %>%
  mutate(City = str_replace(City, "el paso de robles \\(paso robles\\), ", "paso robles, "))	
ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "san bernadino, ", "san bernardino, "))	
ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "mendocino, colorado", "mendocino, california"))	

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "albuqeurque", "albuquerque"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "maui", "kahului"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "corpus cristi", "corpus christi"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "colorado springs, california", "colorado springs, colorado"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "pheonix, arizona", "phoenix, arizona"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "lacrosse, wisconsin", "la crosse, wisconsin"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "pittsfield, michigan", "pittsfield, massachusetts"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "sherman oaks, california", "los angeles, california"))

ccc_sub <- ccc_sub %>%
  mutate(City = str_replace(City, "upper darby, pennsylvania", "darby, pennsylvania"))

ccc.acs <- left_join(ccc_sub, acs_wide, by= "City")
length(unique(ccc.acs$City)) # 3234
non_joined <- ccc.acs [is.na(ccc.acs $population), ]
length(unique(non_joined$City)) #344- when filtering on > 30k, 2227 when not there are about 2k places with protest below 30k with protests in 2020

write.csv(ccc.acs, "/Users/aricaschuett/Documents/protest/Shea + Arica/ccc-acs.csv")

## Descriptive tables-- Victims by type by city

## Victims per city Total
VictimCount <- mappingPreGF %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
##write.csv(VictimCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCount.csv")

##Victims per city Black 
BlackVictimCount <- mappingBlackOnly %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
#write.csv(BlackVictimCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/BlackVictimCount.csv")

## Pre-GF Victims per city  
VictimCountPreGF <- mappingPreGF %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
#write.csv(VictimCountPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCountPreGF.csv")

## Pre-GF Victims per city Black 
BlackVictimCountPreGF <- mappingBlackOnlyPreGF %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
#write.csv(BlackVictimCountPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/BlackVictimCountPreGF.csv")

##High Threat Victims Per City 
VictimHighThreat <- mappingPreGF %>%
  filter(wapo_threat_level == "High") %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
#write.csv(VictimHighThreat, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimHighThreat.csv")

## Descriptive tables-- Victims by type by city
##Protest Totals Per city 
Protest <- ccc.acs %>%
  group_by(City) %>%
  summarize(ProtestCount = n())
head(Protest)
#write.csv(Protest, "/Users/aricaschuett/Documents/protest/Shea + Arica/Protest.csv")

ProtestMean <- ccc.acs %>%
  group_by(City) %>%
  summarize(ProtestSize = mean(size_mean))
sum(ProtestMean$ProtestSize, na.rm= T)
#write.csv(ProtestMean, "/Users/aricaschuett/Documents/protest/Shea + Arica/ProtestMean.csv")

# Match Protests by Police Killing
ProtestCountByKillingCount <- merge(Protest, VictimCount, by ="City") 

ProtestCtReg <- lm(ProtestCount ~ VictimsCount, data = ProtestCountByKillingCount)
summary(ProtestCtReg)

tweets <- read.csv("/Users/aricaschuett/Documents/protest/PCsDupesRemoved - PCs2-12-25.csv")

ProtestByCity <- left_join(Protest, acs_wide, by= "City")
non_joinedPBC <- ProtestByCity[is.na(ProtestByCity$population), ]
length(unique(non_joinedPBC$City)) #2227 when filtering >30000, 344 when doing them all 

ProtestByCity <- ProtestByCity %>%
  mutate(CollegeEdTotal = Bachelors + Masters + ProfDegree + Doctorate)

ProtestByCity$EduRate <- ProtestByCity$CollegeEdTotal/ProtestByCity$Pop25Plus
ProtestByCity$BlackPopPct <- ProtestByCity$BlackPop / ProtestByCity$population
ProtestByCity$BlackPovRate <- ProtestByCity$BlackPov / ProtestByCity$BlackPop

ProtestByCity <- left_join(ProtestByCity, VictimCountPreGF, by= "City")
ProtestByCity <- ProtestByCity %>%
  rename(VictimsCountPreGF = VictimsCount)

ProtestByCity <- left_join(ProtestByCity, BlackVictimCountPreGF, by= "City")
ProtestByCity <- ProtestByCity %>%
  rename(VictimsCountPreGFBlack = VictimsCount )

ProtestByCity <- left_join(ProtestByCity, VictimHighThreat, by= "City")
ProtestByCity <- ProtestByCity %>%
  rename(VictimsCountPreGFHighThreat = VictimsCount )
write.csv(ProtestByCity, "/Users/aricaschuett/Documents/protest/ProtestByCity.csv")

ProtestKitchenSink <- lm(ProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSink)

ProtestKitchenSinkPct <- lm(ProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimsCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSinkPct)
