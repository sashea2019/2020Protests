ProtestByCity <- read.csv("/Users/aricaschuett/Documents/protest/ProtestByCity01-01-2017-12-31-20.csv")


ProtestLimited <- lm(ProtestCount ~ population , data = ProtestByCity)
nobs(ProtestLimited) #1045obs
summary(ProtestLimited)

ProtestKitchenSink <- lm(ProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsCountPreGF, data = ProtestByCity)
nobs(ProtestKitchenSink) #712 obs
used_data <- model.frame(ProtestKitchenSink)
dropped_obs <- anti_join(ProtestByCity, used_data, by = c("ProtestCount","VictimsCountPreGF"))
summary(ProtestKitchenSink)

ProtestKitchenSink <- lm(ProtestCount ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSink)

ProtestKitchenSinkPct <- lm(ProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimsCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSinkPct)

ProtestKitchenSinkPct <- lm(ProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimsCountPreGFBlack, data = ProtestByCity)
summary(ProtestKitchenSinkPct)

ProtestByCity$PoliceDeathsRate <- ProtestByCity$VictimsCountPreGF/ProtestByCity$population
ProtestByCity$BlackVictimsRate <- ProtestByCity$VictimsCountPreGFBlack/ProtestByCity$population

ProtestKitchenSinkPctDeathRate <- lm(ProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + PoliceDeathsRate, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRate)

ProtestKitchenSinkPctDeathRateBlk <- lm(ProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimsRate, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRateBlk)

ProtestByCity$VictimRaceRatio <- ProtestByCity$VictimsCountPreGFBlack/ ProtestByCity$VictimsCountPreGF
ProtestKitchenSinkPctDeathRaceRatio <- lm(ProtestCount ~ population + BlackPopPct + BlackPovRate + EduRate + VictimRaceRatio, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRaceRatio)


### Post GF Protest as DV
ProtestKitchenSink <- lm(PostGFProtest ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSink)

ProtestKitchenSink <- lm(PostGFProtest ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsCountPreGF20, data = ProtestByCity)
summary(ProtestKitchenSink)
nobs(ProtestKitchenSink) #712 obs


ProtestKitchenSink <- lm(PostGFProtest ~ population + BlackPop + BlackPov + CollegeEdTotal + VictimsCountPreGF20Blk, data = ProtestByCity)
summary(ProtestKitchenSink)


ProtestKitchenSinkPct <- lm(PostGFProtest ~ population + BlackPopPct + BlackPovRate + EduRate + VictimsCountPreGF, data = ProtestByCity)
summary(ProtestKitchenSinkPct)

ProtestKitchenSinkPct <- lm(PostGFProtest ~ population + BlackPopPct + BlackPovRate + EduRate + VictimsCountPreGFBlack, data = ProtestByCity)
summary(ProtestKitchenSinkPct)

ProtestByCity$PoliceDeathsRate <- ProtestByCity$VictimsCountPreGF/ProtestByCity$population
ProtestByCity$BlackVictimsRate <- ProtestByCity$VictimsCountPreGFBlack/ProtestByCity$population

ProtestKitchenSinkPctDeathRate <- lm(PostGFProtest ~ population + BlackPopPct + BlackPovRate + EduRate + PoliceDeathsRate, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRate)

ProtestKitchenSinkPctDeathRateBlk <- lm(PostGFProtest ~ population + BlackPopPct + BlackPovRate + EduRate + BlackVictimsRate, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRateBlk)

ProtestByCity$VictimRaceRatio <- ProtestByCity$VictimsCountPreGFBlack/ ProtestByCity$VictimsCountPreGF
ProtestKitchenSinkPctDeathRaceRatio <- lm(PostGFProtest ~ population + BlackPopPct + BlackPovRate + EduRate + VictimRaceRatio, data = ProtestByCity)
summary(ProtestKitchenSinkPctDeathRaceRatio)

