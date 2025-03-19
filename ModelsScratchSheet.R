ProtestByCity <- read.csv("/Users/aricaschuett/Documents/protest/ProtestByCity3-19.csv", row.names = NULL)


ProtestLimited <- lm(PostGFProtestCount ~ population , data = ProtestByCity)
nobs(ProtestLimited) #2874
summary(ProtestLimited)

ProtestKitchenSink <- lm(PostGFProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsPreGF2020, data = ProtestByCity)
nobs(ProtestKitchenSink) #221obs
used_data <- model.frame(ProtestKitchenSink)
dropped_obs <- anti_join(ProtestByCity, used_data, by = c("PostGFProtestCount","VictimsPreGF2020"))
summary(ProtestKitchenSink)

ProtestKitchenSink <- lm(PostGFProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsPreGF2020, data = ProtestByCity)
summary(ProtestKitchenSink)

ProtestKitchenSinkPct <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimsPreGF2020, data = ProtestByCity)
summary(ProtestKitchenSinkPct)

ProtestKitchenSinkPct <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSinkPct)

ProtestKitchenSinkPct <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSinkPct)

ProtestKitchenSinkPct <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + AntiTrumpProtestPreGFCount + BlackVictimCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSinkPct)

ProtestByCity$PoliceDeathsRate <- ProtestByCity$VictimCountPreGF/ProtestByCity$population
ProtestByCity$BlackVictimsRate <- ProtestByCity$VictimCountPreGF/ProtestByCity$population

ProtestKitchenSinkPctDeathRate <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + AntiTrumpProtestPreGFCount+ PoliceDeathsRate, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRate)

ProtestKitchenSinkPctDeathRateBlk <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimsRate, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRateBlk)

ProtestByCity$VictimRaceRatio <- ProtestByCity$BlackVictimCountPreGF/ ProtestByCity$VictimsCountPreGF
ProtestKitchenSinkPctDeathRaceRatio <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimRaceRatio, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRaceRatio)


### Post GF Protest as DV
ProtestKitchenSink <- lm(PostGFProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSink)

ProtestKitchenSink <- lm(PostGFProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsPreGF2020, data = ProtestByCity)
summary(ProtestKitchenSink)
nobs(ProtestKitchenSink) #712 obs


ProtestKitchenSink <- lm(PostGFProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsPreGF2020Blk, data = ProtestByCity)
summary(ProtestKitchenSink)


ProtestKitchenSinkPct <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSinkPct)

ProtestKitchenSinkPct <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSinkPct)


ProtestByCity$PoliceDeathsRate <- ProtestByCity$VictimCountPreGF/ProtestByCity$population
ProtestByCity$BlackVictimsRate <- ProtestByCity$BlackVictimCountPreGF/ProtestByCity$population

ProtestKitchenSinkPctDeathRate <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + PoliceDeathsRate, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRate)

ProtestKitchenSinkPctDeathRateBlk <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimsRate, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRateBlk)

ProtestByCity$VictimRaceRatio <- ProtestByCity$BlackVictimCountPreGF/ ProtestByCity$VictimCountPreGF

ProtestKitchenSinkPctDeathRaceRatio <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimRaceRatio, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRaceRatio)

ProtestKitchenSinkPctDeathRaceRatio <- lm(PostGFProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate +
                                            AntiTrumpProtestPreGFCount + VictimRaceRatio, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRaceRatio)
