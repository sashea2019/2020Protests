library(tidyverse)
library(lubridate)

#ccc.acs_complete <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ccc.acs_complete.csv")
#ccc.acs <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ccc-acsTotal.csv")
#mapping.acs <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/mapping.acs.csv")
#mapping <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/mapping.csv")
#acs_wide <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/acs_wide.csv")

### This data will contain every ACS observation, the count of police killings jan 2017-before GF, count of police killings after GF (but before Sep 2020), 
### Count of general left-wing protests (Valence in CCC, check code book) jan 2014-pre GF, 
### Democratic vote share 2016 MITESL, FIPS 

ccc_spatial_join <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ccc_spatial_join2.csv", colClasses = c("GEOID" = "character"))
ccc_spatial_join$PLACEFIPS <- ccc_spatial_join$GEOID
mapping_acs_join <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ACS_Mapping.csv", colClasses = c("PLACEFIPS" = "character"))

mapping <- mapping_acs_join
ccc <- ccc_spatial_join

mapping <- mapping %>%
  mutate(date = ymd(date))

ccc <- ccc %>%
  mutate(date = mdy(date))

mapping <- mapping %>%
  mutate(
    date = ymd(date),  # Convert to Date type
    year = year(date  ))      # Extract year


ccc <- ccc %>%
  mutate(
    date = ymd(date),  # Convert to Date type. # Begins Jan 1, 2017
    year = ymd(date  ))      # Extract year


# filter date by 
mapping <- mapping %>%
  mutate(mappingPreGF = ifelse(date >= "2017-01-01" & date <= "2020-05-20", 1, 0),
         mappingPostGF = ifelse(date>= "2020-05-20" & date <= "2020-09-01", 1, 0),
         mappingBlackOnlyPreGF = ifelse(race == "Black" & date >= "2017-01-01" & date <= "2020-05-20", 1, 0),
         mappingBlackOnlyPostGF = ifelse(race == "Black" & date >= "2020-05-20" & date <= "2020-09-01", 1, 0))


mapping <- mapping %>%
  select(name, age ,gender, race, date, city, state, zip, county, agency_responsible, 
         cause_of_death, officer_charged, allegedly_armed, wapo_armed, wapo_threat_level, wapo_flee, 
         wapo_body_camera, wapo_id, off_duty_killing, officer_known_past_shootings, mappingPreGF, 
         mappingPostGF, mappingBlackOnlyPostGF, mappingBlackOnlyPreGF, POPULATION, POP_SQMI, year, 
         population, BlackPop, BlackPov, Bachelors, Masters,
         ProfDegree, Pop25Plus, ProfDegree, Doctorate, CollegeStudents, PLACEFIPS)

ccc <- ccc %>%
  select(date, locality, state, population, type, actors, claims, valence, issues, size_mean, arrests_any, 
         injuries_crowd_any, injuries_police_any, property_damage_any, PLACEFIPS )

ccc <- ccc %>%
  mutate(cccPreGF = ifelse(date >= "2017-01-01" & date <= "2020-05-20", 1, 0),
         cccPostGF = ifelse(date>= "2020-05-20" & date <= "2020-09-01", 1, 0))

## Descriptive tables-- Victims by type by city
## Victims per city Total
VictimCount <- mapping %>%
  group_by(PLACEFIPS) %>%
  summarize(VictimsCount = n())
#VC <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCount.csv")
write.csv(VictimCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCountFull.csv")

##Victims per city Black 
BlackVictimCount <- mapping %>%
  filter(race == "Black") %>%
  group_by(PLACEFIPS) %>%
  summarize(BlackVictimCount = n())
write.csv(BlackVictimCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/BlackVictimCountFull.csv")

## Pre-GF Victims per city  
VictimCountPreGF <- mapping %>%
  filter(mappingPreGF == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(VictimCountPreGF = n())
write.csv(VictimCountPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCountPreGFFull.csv")

## Pre-GF Victims per city Black 
BlackVictimCountPreGF <- mapping %>%
  filter(mappingBlackOnlyPreGF == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(BlackVictimCountPreGF = n())
write.csv(BlackVictimCountPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/BlackVictimCountPreGFFull.csv")

## Pre-GF Victims per city 2020
VictimsPreGF2020 <- mapping %>%
  filter( year == 2020 & mappingPreGF == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(VictimsPreGF2020 = n())
write.csv(VictimsPreGF2020, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimsPreGF2020Full.csv")

## Pre-GF Victims per city 2020 Black 
VictimsPreGF2020Blk <- mapping %>%
  filter(race == "Black" & year == 2020 & mappingPreGF == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(VictimsPreGF2020Blk = n())
write.csv(VictimsPreGF2020Blk, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimsPreGF2020BlkFull.csv")

##High Threat Victims Per City  Pre GF add armed vs unarmed
VictimHighThreat <- mapping %>%
  filter(wapo_threat_level == "High" & mappingPreGF == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(VictimHighThreat = n())
write.csv(VictimHighThreat, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimHighThreatFull.csv")

## Descriptive tables-- Victims by type by city
##Protest Totals Per city Pre GF and left wing valence protest totals per city
ProtestTotal <- ccc %>%
  filter(date <= "2020-05-25") %>%
  group_by(PLACEFIPS) %>%
  summarize(ProtestTotal = n())
head(ProtestTotal)
write.csv(ProtestTotal, "/Users/aricaschuett/Documents/protest/Shea + Arica/ProtestTotalFull.csv")

AntiTrumpProtestPreGF <- ccc %>%
  filter(date <= "2020-05-25" & valence == 1) %>%
  group_by(PLACEFIPS) %>%
  summarize(AntiTrumpProtestPreGFCount = n())
write.csv(AntiTrumpProtest, "/Users/aricaschuett/Documents/protest/Shea + Arica/ProtestMeanFull.csv")


PostGFProtestCount <- ccc %>%
  filter(date > ymd("2020-05-25")) %>%
  group_by(PLACEFIPS) %>%
  summarize(PostGFProtestCount = n())
head(PostGFProtestCount)
write.csv(PostGFProtestCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/PostGFProtestCount.csv")


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

acs_wide <- acs_wide %>%
  filter( population > 500)

any(is.na(acs_wide$population))
any(is.na(acs_wide$BlackPop))
sum(acs_wide$population) 
sum(acs_wide$BlackPop) 

head(ProtestTotal)
head(mapping)
ProtestByCity <- left_join(ProtestTotal, PostGFProtestCount, by= "PLACEFIPS", all = T) 
ProtestByCity <- left_join(ProtestByCity, VictimCount, by= "PLACEFIPS", all = T)
ProtestByCity <- left_join(ProtestByCity, BlackVictimCount, by= "PLACEFIPS", all = T)
ProtestByCity <- left_join(ProtestByCity, VictimCountPreGF, by= "PLACEFIPS", all = T)
ProtestByCity <- left_join(ProtestByCity, BlackVictimCountPreGF, by= "PLACEFIPS", all = T)
ProtestByCity <- left_join(ProtestByCity, VictimsPreGF2020, by= "PLACEFIPS", all = T)
ProtestByCity <- left_join(ProtestByCity, VictimHighThreat, by= "PLACEFIPS", all = T)
ProtestByCity <- left_join(ProtestByCity, AntiTrumpProtestPreGF, by= "PLACEFIPS", all = T)


acs_wide$PLACEFIPS <- acs_wide$GEOID

ProtestByCity <- left_join(acs_wide, ProtestByCity, by= "PLACEFIPS", all = T)



ProtestByCity <- ProtestByCity %>%
  mutate(CollegeEdTotal = Bachelors + Masters + ProfDegree + Doctorate)

ProtestByCity$EduRate <- ProtestByCity$CollegeEdTotal/ProtestByCity$Pop25Plus
ProtestByCity$BlackPopPct <- ProtestByCity$BlackPop / ProtestByCity$population
ProtestByCity$BlackPovRate <- ProtestByCity$BlackPov / ProtestByCity$BlackPop


write.csv(ProtestByCity, "/Users/aricaschuett/Documents/protest/ProtestByCity3-19.csv")
