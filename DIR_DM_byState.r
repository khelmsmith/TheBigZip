
# This script joins data from the Drought Impact Reporter (go.unl.edu/DIRdash) and the U.S. Drought Monitor (droughtmonitor.unl.edu) to list impacts that have occurred in each state at each level of drought: https://droughtmonitor.unl.edu/DmData/StateImpacts.aspx
# Through Nov. 20, 2025. 
# The DIR data through Nov. 20, 2025, is available via SANDY -- ADD LINK. 
# USDM data used for this analysis is from https://droughtmonitor.unl.edu/DmData/DataDownload/ComprehensiveStatistics.aspx
# and https://droughtmonitor.unl.edu/DmData/OCONUSDroughtStatus.aspx

library(dplyr)
library(lubridate)
library(stringr)
library(data.table)
library(tidyr)

# read in impacts
DIRdata <- read.csv("data/Drought_Impact_Reporter_data_2005_2025.csv", stringsAsFactors = FALSE) # 151450

# convert from character to date format
DIRdata$Start_Date <- parse_date_time(DIRdata$Start_Date, "mdy")
DIRdata$End_Date <- parse_date_time(DIRdata$End_Date, "mdy")

# weekstart provides a date to match USDM weekly data 
DIRdata$weekstart <- floor_date(DIRdata$Start_Date, unit = "weeks", week_start = 2)

# USDM records start in 2000 so we're eliminating ones before then
DIRdata <- DIRdata %>% filter(weekstart > "2000-01-01") #149019

# omit impacts with Jan. 1 start dates as that tends to refer to a data collection interval rather than an onset date
DIRdata$month <- month(DIRdata$Start_Date)
DIRdata$dayofmonth <- mday(DIRdata$Start_Date)

# but it may be interesting to see what's being excluded
Jan1starts <- DIRdata %>% filter(month == 1 & dayofmonth == 1) # 10106

'%ni%' <- Negate('%in%') # a handy function for getting rid of things, in this case impacts w/ Jan. 1 start dates
DIRdata <- DIRdata %>% filter(Impact_ID %ni% Jan1starts$Impact_ID) # 138913 

# what about omitting impacts that are a year or longer? 
# Omits too many good ones so not implementing.
# DIRdata$weeks_diff <- round(difftime(DIRdata$End_Date, DIRdata$Start_Date, units = "weeks"),0) 
# longones <- DIRdata %>% filter(weeks_diff > 52)
# DIRdata <- DIRdata %>% filter(weeks_diff < 52)  

# cut by level, for join with U.S. Drought Monitor state data, and for filtering by State_Scale
DIRstate <- DIRdata %>% filter(Impact_Level == "State") # 12377
unique(DIRstate$State_Name) 
# 53 with DC, Puerto Rico and Virgin Islands
DIRlocal <- DIRdata %>% filter(Impact_Level == "County"|Impact_Level == "City") # 126536
unique(DIRlocal$State_Name) 
# 55 with DC, PR, Northern Mariana Islands, Micronesia and Virgin Islands

# Eliminate macroeconomic impacts and others that cover many states
# DIRstate$State_Scale <- as.numeric(DIRstate$State_Scale)
# It may be interesting and useful to see what's omitted. Also used to filter city/county impacts.
DIRdata.freq <- DIRstate[DIRstate$State_Scale >= 10,] # 6701
# for state impacts
DIRstate <- DIRstate[DIRstate$State_Scale < 10,] # 5676
# for local (city or county) impacts
DIRlocal <- DIRlocal %>% filter(Impact_ID %ni% DIRdata.freq$Impact_ID) # 126338

# get rid of extra columns 
DIRstate <- DIRstate %>% select(c("State_Name","StateAbbreviation"="State_Abbr","State_FIPS","Impact_ID","Title", "Description","Start_Date","End_Date", 
                                  "agriculture","business_industry","energy","fire",
                                  "plants_wildlife", "relief_response_restrictions","society_public_health",
                                  "tourism_recreation","water_supply_quality","weekstart", "month"))

# read in U.S. Drought Monitor data by state
DMstate <- read.csv("data/dm_state_20000101_20251120.csv", stringsAsFactors = FALSE)
# has PR and DC but not VI

# get rid of extra column
DMstate <- DMstate %>% select(-c("StatisticFormatID"))

# convert character string to date
DMstate$weekstart <- parse_date_time(DMstate$ValidStart, "ymd")

# assign a U.S. Drought Monitor status based on the highest category of drought anywhere in the state

# Caution! The numbering is counter-intuitive. It may make more sense for your purposes to use D0-D4 or labels. 
# D0 (Abnormally Dry) = 1
# D1 (Moderate Drought) = 2
# D2 (Severe Drought) = 3
# D3 (Extreme Drought) = 4
# D4 (Exceptional Drought) = 5
# None = 6

DMstate <- DMstate %>%
 mutate(DMstatus = ifelse(D4 > 0, "5",
                      ifelse(D3 > 0, "4",
                      ifelse(D2 > 0, "3",
                      ifelse(D1 > 0, "2",
                      ifelse(D0 > 0, "1", "6"))))))

# Also create a binary drought/non-drought variable, with drought defined as D1 or worse. Skip this if you aren't interested in the number of weeks in drought.
DMstate <- DMstate %>%
  mutate(drought = ifelse(D1 > 0, 1, 0))

# identify "runs" of drought and non-drought
DMstate <- DMstate %>%
  arrange(StateAbbreviation, weekstart) %>% 
  group_by(StateAbbreviation) %>% 
  mutate(run = rleid(drought))

# count the number of weeks of drought
DMstate <- DMstate %>% 
  arrange(StateAbbreviation, weekstart) %>%
  group_by(StateAbbreviation, run) %>%
  mutate(weeks = ifelse(drought == 1, row_number(run), 0))

# get start and end dates for runs
DMstate <- DMstate %>%
  arrange(StateAbbreviation, weekstart) %>%
  group_by(StateAbbreviation, run) %>%
  mutate(start = min(weekstart), end = max(weekstart))

DMstate <- ungroup(DMstate)

# get rid of columns we don't need anymore
DMstate <- DMstate %>% select(c("StateAbbreviation","weekstart","DMstatus","weeks","start","end"))

stateDIR_DM <- left_join(DIRstate, DMstate, by = c("weekstart","StateAbbreviation")) 

stateDIR_DM <- stateDIR_DM %>% filter(StateAbbreviation != "VI") # 5671
# Eliminates 5 Virgin Island impacts but same impacts are listed by county.
# USDM data is in a different format for islands, dealt with below.
unique(stateDIR_DM$State_Name)
# 52 with DC and PR

# Now join local impacts with U.S. Drought Monitor county data

# get rid of extra columns
DIRlocal <- DIRlocal %>% select(c("State_Name","StateAbbreviation"="State_Abbr","State_FIPS","County_FIPS","County_Name","Impact_ID","Title", "Description","Start_Date","End_Date", "agriculture","business_industry","energy","fire",
                                    "plants_wildlife", "relief_response_restrictions","society_public_health",
                                    "tourism_recreation","water_supply_quality","weekstart","month"))

# make sure FIPS codes are in the right format
DIRlocal$County_FIPS <- as.character(str_pad(DIRlocal$County_FIPS, 5, "left", "0"))

# fixing an Alaska FIPS code. This is imperfect but OK for purposes of state tables. 
DIRlocal$County_FIPS[DIRlocal$County_FIPS == "02261"] <- "02063"

# Read in USDM data by county 

DMcounty <- read.csv("data/dm_county_20000101_20251120.csv", stringsAsFactors = FALSE)
DMcounty$weekstart <- parse_date_time(DMcounty$ValidStart, "ymd")
DMcounty$County_FIPS <- str_pad(DMcounty$FIPS, 5, "left", "0")
DMcounty <- DMcounty %>% select(-c("StatisticFormatID","FIPS"))

DMcounty <- DMcounty %>%
 mutate(DMstatus = ifelse(D4 > 0, "5",
                      ifelse(D3 > 0, "4",
                      ifelse(D2 > 0, "3",
                      ifelse(D1 > 0, "2",
                      ifelse(D0 > 0, "1", "6"))))))

DMcounty <- DMcounty %>%
  mutate(drought = ifelse(D1 > 0, 1, 0))

DMcounty <- DMcounty %>% rename(StateAbbreviation = State)

DMcounty <- DMcounty %>%
  arrange(County_FIPS, weekstart) %>% 
  group_by(County_FIPS) %>% 
  mutate(run = rleid(drought))

DMcounty <- DMcounty %>% 
  arrange(County_FIPS, weekstart) %>%
  group_by(County_FIPS, run) %>%
  mutate(weeks = ifelse(drought == 1, row_number(run), 0))

DMcounty <- DMcounty %>%
  arrange(County_FIPS, weekstart) %>%
  group_by(County_FIPS, run) %>%
  mutate(start = min(weekstart), end = max(weekstart))

DMcounty <- ungroup(DMcounty)

DMcounty <- DMcounty[,c("StateAbbreviation","weekstart","County_FIPS","DMstatus","weeks","start","end")]

# Using earlier download of USVI and MP stats as there are no new impacts recorded since then.
# Doesn't mean impacts didn't occur, but they may not be well documented or are harder to pick up. 
# U.S. Drought Monitor stats for islands and territories are in a different format.

USVI <- read.csv("data/USVI_0224.csv", stringsAsFactors = FALSE)
USVI$weekstart <- parse_date_time(USVI$Date, "mdy")
USVI$State_Abbr <- "VI"
USVI$State <- "Virgin Islands"
USVI <- pivot_longer(USVI, 2:4, names_to = "County", values_to = "DMstatus")
USVI$County_FIPS <- ifelse(USVI$County == "Saint.Croix", "78010",
                   ifelse(USVI$County == "Saint.John", "78020", "78030"))

MP <- read.csv("data/MP_0224.csv", stringsAsFactors = FALSE)
MP$weekstart <- parse_date_time(MP$Date, "mdy")
MP <-  pivot_longer(MP, 2:4, names_to = "County", values_to = "DMstatus")
MP$County_FIPS <- ifelse(MP$County == "Saipan", "69110",
                   ifelse(MP$County == "Guam", "66010", "69100"))
MP$State_Abbr <- "MP"
MP$State <- "Northern Mariana Islands"

FM <- read.csv("data/FM_1125.csv")
FM$weekstart <- parse_date_time(FM$Date, "mdy")
FM <- FM %>% rename(Yap.Island = Yap)
FM <- pivot_longer(FM, 2:12, names_to = "County", values_to = "DMstatus")
FM$State_Abbr <- "FM"
FM$State <- "Micronesia"
FM <- FM %>% filter(County == "Yap.Island")
FM$County_FIPS <- "64060"

OCONUS <- bind_rows(USVI, MP)
OCONUS <- bind_rows(OCONUS, FM)
OCONUS <- OCONUS %>% filter(DMstatus != "No Data")
OCONUS <- OCONUS %>% select(-c("Date"))
OCONUS <- rename(OCONUS, dstat = DMstatus)

OCONUS <- OCONUS %>%
 mutate(DMstatus = ifelse(dstat == "D4", "5",
                      ifelse(dstat == "D3", "4",
                      ifelse(dstat == "D2", "3",
                      ifelse(dstat == "D1", "2",
                      ifelse(dstat == "D0", "1", "6"))))))

OCONUS <- OCONUS %>%
 mutate(drought = ifelse(dstat == "D4", 1,
                      ifelse(dstat == "D3", 1,
                      ifelse(dstat == "D2", 1,
                      ifelse(dstat == "D1", 1,
                      ifelse(dstat == "D0", 0, 0))))))

OCONUS <- OCONUS %>% rename(StateAbbreviation = State_Abbr)

OCONUS <- OCONUS %>%
  arrange(County_FIPS, weekstart) %>% 
  group_by(County_FIPS) %>% 
  mutate(run = rleid(drought))

OCONUS <- OCONUS %>% 
  arrange(County_FIPS, weekstart) %>%
  group_by(County_FIPS, run) %>%
  mutate(weeks = ifelse(drought == 1, row_number(run), 0))

OCONUS <- OCONUS %>%
  arrange(County_FIPS, weekstart) %>%
  group_by(County_FIPS, run) %>%
  mutate(start = min(weekstart), end = max(weekstart))

OCONUS <- OCONUS[,c("StateAbbreviation","weekstart","County_FIPS","DMstatus","weeks","start","end")]

DMcounty <- bind_rows(DMcounty, OCONUS) 

localDIR_DM <- left_join(DIRlocal, DMcounty, by = c("weekstart","County_FIPS", "StateAbbreviation"), relationship = "many-to-many") # 126338

# If all is well up to this point, the number of observations stays the same.
localDIR_DM <- localDIR_DM[complete.cases(localDIR_DM),] # 126338 


# Combine levels

all <- bind_rows(stateDIR_DM, localDIR_DM) # 132009

# Remove duplicates, i.e., impacts that have occurred in multiple counties within a state, etc.
# get down to one per USDM status per state

states <- unique(all$State_Name)
listOfStates <- vector("list", length=length(states))

for(i in seq_along(states)){
  state.i <- states[i]
data.i <- all[all$State_Name == state.i,]
data.i <- data.i %>% group_by(Impact_ID) %>% distinct(DMstatus, .keep_all = TRUE)
listOfStates[[i]] <- data.i
}

all <- bind_rows(listOfStates) # 34512

# add cool stuff

all <- all %>% mutate(season = ifelse(month == 12 | month == 1 | month == 2, "winter",
                                      ifelse(month == 3 | month == 4 | month == 5, "spring",
                                             ifelse(month == 6 | month == 7 | month == 8, "summer", "fall"))))

all <- all %>% mutate(drought_period = paste0(start," to ",end))


# clean up

all <- all[,c("Impact_ID","Title","Description","Start_Date","State_Name","StateAbbreviation","State_FIPS","agriculture","business_industry","energy","fire",
              "plants_wildlife", "relief_response_restrictions","society_public_health",
              "tourism_recreation","water_supply_quality","DMstatus","weeks","season","drought_period")]

all <- rename(all, weeksInDrought = weeks)

all$Start_Date <- as.Date(all$Start_Date)

all <- all[rev(order(all$Start_Date)),]
all$Start_Date <- as.character(all$Start_Date)

all$State_FIPS <- as.character(str_pad(all$State_FIPS, 2, "left", "0"))

# library(writexl)

# write_xlsx(all, "output/all_thru_20251120.slsx")









