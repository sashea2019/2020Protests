ccc <- read.csv("/Users/aricaschuett/Downloads/ccc_compiled_20172020.csv")

ccc_sub <- ccc[ccc$date >= "2020-03-01" & ccc$date <= "2020-09-01", ]

# policing, race #starting keywords not patriotism Not Pro-police not anti-mask or anti

mapping <- read.csv("/Users/aricaschuett/Downloads/Mapping Police Violence.csv")
