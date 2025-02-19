library(lubridate)


ccc <- read.csv("/Users/aricaschuett/Downloads/ccc_compiled_20172020.csv")

ccc_sub <- ccc[ccc$date >= "2020-03-01" & ccc$date <= "2020-09-01", ]

# policing, race #starting keywords not patriotism Not Pro-police not anti-mask or anti

mapping <- read.csv("/Users/aricaschuett/Downloads/Mapping Police Violence.csv")

mapping <- mapping %>%
  filter(!grepl("Vehicle", cause_of_death))

ccc_sub <- ccc_sub %>%
  filter(grepl("racism", issues )) # I had trouble filtering on racism or policing, but I dont think this is necessary and think this gets what I need. 

ccc_sub$City<-paste(ccc_sub$locality, ccc_sub$state, sep=", ")
mapping$City<-paste(mapping$city, mapping$state, sep=", ")

ccc_sub <- ccc_sub %>% select(date, City, issues, actors, claims, valence, 
                              size_low, size_high, size_mean)
mapping <- mapping %>% select(name, age, gender, race, date, City, 
                              cause_of_death, wapo_armed, wapo_threat_level, 
                              officer_known_past_shootings )


####  5 PROVIDING LEGAL SUPPORT #####
mapping$wapo_threat_level[ mapping$wapo_threat_level=="Attack"|
                             mapping$wapo_threat_level=="Brandished Weapon" |
                             mapping$wapo_threat_level=="Sudden Threatening Movement" |
                             mapping$wapo_threat_level=="Used Weapon"]<-"High"   ### unclear if including brandishing or suddent threatening movement as HIGH threat

mappingBlackOnly <- mapping %>%
  filter(grepl("Black", race))

# Date 
mappingPreGF <- mapping[mapping$date >= "2017-01-01" & mapping$date <= "2020-05-20", ] #started this in 2017
mappingPostGF <- mapping[mapping$date >= "2020-05-20" & mapping$date <= "2020-09-01", ] # During protest period

mappingBlackOnlyPreGF <- mappingBlackOnly[mappingBlackOnly$date >= "2017-01-01" & mappingBlackOnly$date <= "2020-05-20", ] #started this in 2017
mappingBlackOnlyPostGF <- mappingBlackOnly[mappingBlackOnly$date >= "2020-05-20" & mappingBlackOnly$date <= "2020-09-01", ] # During protest period


mapping$date <- mdy(mapping$date)

#ccc_sub$date <- mdy(ccc_sub$date) #I don't know why this isn't working. 



## Descriptive tables-- Victims by type by city

## Victims per city Total
VictimCount <- mapping %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(VictimCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCount.csv")

##Victims per city Black 
BlackVictimCount <- mappingBlackOnly %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(BlackVictimCount, "/Users/aricaschuett/Documents/protest/Shea + Arica/BlackVictimCount.csv")

## Pre-GF Victims per city  
VictimCountPreGF <- mappingPreGF %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(VictimCountPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimCountPreGF.csv")

## Pre-GF Victims per city Black 
BlackVictimCountPreGF <- mappingBlackOnlyPreGF %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(BlackVictimCountPreGF, "/Users/aricaschuett/Documents/protest/Shea + Arica/BlackVictimCountPreGF.csv")

##High Threat Victims Per City 
VictimHighThreat <- mapping %>%
  filter(wapo_threat_level == "High") %>%
  group_by(City) %>%
  summarize(VictimsCount = n())
write.csv(VictimHighThreat, "/Users/aricaschuett/Documents/protest/Shea + Arica/VictimHighThreat.csv")

## Descriptive tables-- Victims by type by city
##Protest Totals Per city 
Protest <- ccc_sub %>%
  group_by(City) %>%
  summarize(ProtestCount = n())
write.csv(Protest, "/Users/aricaschuett/Documents/protest/Shea + Arica/Protest.csv")

ProtestMean <- ccc_sub %>%
  group_by(City) %>%
  summarize(ProtestSize = mean(size_mean))
write.csv(ProtestMean, "/Users/aricaschuett/Documents/protest/Shea + Arica/ProtestMean.csv")

# Match Protests by Police Killing
ProtestCountByKillingCount <- merge(Protest, VictimCount, by ="City")

ProtestCtReg <- lm(ProtestCount ~ VictimsCount, data = ProtestCountByKillingCount)
summary(ProtestCtReg)

tweets <- read.csv("/Users/aricaschuett/Documents/protest/PCsDupesRemoved - PCs2-12-25.csv")

