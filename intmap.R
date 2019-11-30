#----------------------
# CI3 CRA project
# interactive CRA
# rating map
# IS
#----------------------

#install packages
libs <- c("dplyr", "rgdal", "ggplot2",
          "leaflet", "shiny", "DT", "RColorBrewer",
          "scales", "lattice", "readxl")
install.packages(libs)

# load libraries
lapply(libs, library, character.only = TRUE)

# import cra coordinates dataset
path <- "/Users/ishashah/Documents/CI3"
setwd(path)

crasample <- read.csv("cracoord.csv", stringsAsFactors = FALSE)

# import CDFI dataset
cdfi <- read_xls("cdfi_awards.xls")
cdfi <- cdfi %>%
  mutate(upcity = toupper(City))

# are there dup ids in the cra dataset?
chkcra = crasample %>% group_by(ID, BankName, City, State) %>%
  summarize(n = n()) %>%
  arrange(-n)
table(chkcra$n)

# yes, there are duplicate ids  - banks that have the same ID and name, in the same city, state
# take only the one from the latest exam date?
# note that the exam method might be different on different dates

install.packages("lubridate")
library(lubridate)
library(stringr)

crasampledd <- crasample %>% 
  mutate(ExamDate_d = parse_date_time(ExamDate, "%m/%d/%y")) %>%
  # arrange(ID, BankName, City, State, ExamDate_d) %>%
  group_by(ID, BankName, City, State) %>%
  filter(ExamDate_d == max(ExamDate_d))

# now, collapse both cra and cdfi to city level
cracity <- crasampledd %>%
  group_by(City, State, MeanRankingbyCity, GEOID, UATYPE, LAT, LON) %>%
  summarize(TotalAssets = sum(AssetSize),
           NumBanks = n(),
           # NumRegulators = nunique(Regulator),
           AvgRating = mean(Rating),
          Banks = paste(BankName, collapse = ", ")
          )

cdficity <- cdfi %>%
  mutate(City = trimws(toupper(City))) %>%
  group_by(City, State) %>%
  summarize(cdfi_amt = sum(Amount),
            CDFIs = paste(Awardee, collapse = ", "))

# merge
cra_cdfi <- full_join(cracity, cdficity, by = c("City", "State"))
# extract all cities without lat/long

missingcoords <- cra_cdfi %>% ungroup() %>% filter(is.na(LAT)) %>% 
  select(City, State) %>% distinct()

# export
write.csv(missingcoords, "missingcoords.csv", row.names = FALSE)

# import back
filledcoords <- read.csv("filledcoords.csv")

filledcoords <- filledcoords %>%
  select(City, State, Latitude, Longitude)

cra_cdfi <- left_join(ungroup(cra_cdfi), filledcoords, by = c("City", "State")) %>%
  mutate(LAT = ifelse(is.na(LAT), Latitude, LAT),
         LON = ifelse(is.na(LON), Longitude, LON)) %>%
  select(-Latitude, -Longitude)

# export
write.csv(cra_cdfi, "cra_cdfi.csv")

runApp("int_map")