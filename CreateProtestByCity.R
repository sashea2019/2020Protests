library(tidyverse)

ccc.acs_complete <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ccc.acs_complete.csv")
ccc.acs <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/ccc-acsTotal.csv")
#mapping.acs <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/mapping.acs.csv")
mapping <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/mapping.csv")
acs_wide <- read.csv("/Users/aricaschuett/Documents/protest/Shea + Arica/acs_wide.csv")


## Descriptive tables-- Victims by type by city
## Victims per city Total
VictimCount <- mapping %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(VictimCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCount.csv")

##Victims per city Black 
BlackVictimCount <- mapping %>%
  filter(race == "Black") %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(BlackVictimCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/BlackVictimCount.csv")

## Pre-GF Victims per city  
VictimCountPreGF <- mapping %>%
  filter(mappingPreGF == 1) %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(VictimCountPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCountPreGF.csv")

## Pre-GF Victims per city Black 
BlackVictimCountPreGF <- mapping %>%
  filter(mappingBlackOnlyPreGF == 1) %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(BlackVictimCountPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/BlackVictimCountPreGF.csv")

## Pre-GF Victims per city 2020
VictimsPreGF2020 <- mapping %>%
  filter( year == 2020 & mappingPreGF == 1) %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(VictimsPreGF2020, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimsPreGF2020.csv")

## Pre-GF Victims per city 2020 Black 
VictimsPreGF2020Blk <- mapping %>%
  filter(race == "Black" & year == 2020 & mappingPreGF == 1) %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(VictimsPreGF2020Blk, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimsPreGF2020Blk.csv")

##High Threat Victims Per City 
VictimHighThreat <- mapping %>%
  filter(wapo_threat_level == "High") %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(VictimHighThreat, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimHighThreat.csv")

## Descriptive tables-- Victims by type by city
##Protest Totals Per city 
ProtestTotal <- ccc.acs_complete %>%
  group_by(City) %>%
  summarize(ProtestCount = n())
head(ProtestTotal)
write.csv(ProtestTotal, "/Users/aricaschuett/Documents/protest/Shea + Arica/ProtestTotal.csv")

ProtestMean <- ccc.acs_complete %>%
  group_by(City) %>%
  summarize(ProtestSize = mean(size_mean))
sum(ProtestMean$ProtestSize, na.rm= T)
write.csv(ProtestMean, "/Users/aricaschuett/Documents/protest/Shea + Arica/ProtestMean.csv")


PostGFProtestCount <- ccc.acs_complete %>%
  filter(date > ymd("2020-05-25")) %>%
  group_by(City) %>%
  summarize(ProtestCount = n())
head(PostGFProtestCount)



ProtestByCity <- left_join(ProtestTotal, acs_wide, by= "City", all = T)
#non_joinedProtestByCity <- ProtestByCity[is.na(ProtestByCity$population), ]
#length(unique(non_joinedProtestByCity$City)) # 0

ProtestByCity <- ProtestByCity %>%
  mutate(CollegeEdTotal = Bachelors + Masters + ProfDegree + Doctorate)

ProtestByCity$EduRate <- ProtestByCity$CollegeEdTotal/ProtestByCity$Pop25Plus
ProtestByCity$BlackPopPct <- ProtestByCity$BlackPop / ProtestByCity$population
ProtestByCity$BlackPovRate <- ProtestByCity$BlackPov / ProtestByCity$BlackPop

VictimCountPreGF <- VictimCountPreGF %>%
  rename(VictimsCountPreGF = VictimsCount)
ProtestByCity <- left_join(ProtestByCity, VictimCountPreGF, by= "City")

BlackVictimCountPreGF <- BlackVictimCountPreGF %>%
  rename(VictimsCountPreGFBlack = VictimsCount )
ProtestByCity <- left_join(ProtestByCity, BlackVictimCountPreGF, by= "City")

PostGFProtestCount <- PostGFProtestCount %>%
  rename(PostGFProtest = ProtestCount)
ProtestByCity <- left_join(ProtestByCity, PostGFProtestCount, by = "City")

VictimsPreGF2020 <- VictimsPreGF2020 %>%
  rename(VictimsCountPreGF20 = VictimsCount)
ProtestByCity <- left_join(ProtestByCity, VictimsPreGF2020, by = "City")

VictimsPreGF2020Blk <- VictimsPreGF2020Blk %>%
  rename(VictimsCountPreGF20Blk = VictimsCount)
ProtestByCity <- left_join(ProtestByCity, VictimsPreGF2020Blk, by = "City")

# Some of these may genuinely not have police shootings, others are joining errors
non_joinedProtestByCityNA <- ProtestByCity[is.na(ProtestByCity$VictimsCountPreGF), ] 
#write.csv(ProtestByCity, "/Users/aricaschuett/Documents/protest/ProtestByCity01-01-2017-12-31-20.csv")
