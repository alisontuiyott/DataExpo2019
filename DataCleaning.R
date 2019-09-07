# Read in necessary libraries & set directory
library(pacman)
p_load(ggplot2,Stack,dplyr,ggrepel,gridExtra,tidyr,
       leaflet,plotly,readxl,rgdal,tigris,rgdal,rgeos,maptools,sp,scales)

# Read in data
# filename1 <- paste("NYCHVS", 1991, "Occupied File for ASA Challenge_CSV.csv")
# temp1 <- read.csv(filename1,  skip = 1, header = T)
# counter <- 1993
# allData <- {temp1}
# 
# for(i in 1:9){
#   filename <- paste("NYCHVS", counter, "Occupied File for ASA Challenge_CSV.csv")
#   temp <- read.csv(filename,  skip = 1, header = T)
#   allData <- Stack(allData,temp)#colnames(temp))
#   #colnames(temp) <- gsub(colnames(temp)
#   #allData <- rbind(allData, temp)
#   counter <- counter+3
# }
#save(allData, file="AllYears.Rdata")
load("AllYears.Rdata")

# Filter for immigrant households
allData_id <- allData %>%
  mutate(
    immigrant = ifelse(
      ( !(Place.of.Householder.s.Birth %in% c(9,10,98)) & Year.Identifier < 2005 ) |
        ( !(Place.of.Householder.s.Birth %in% c(7,9,98)) & Year.Identifier >= 2005 ), 1,0))

# Complete Data W/ Sampling Weights and Immigration numbers
completeData <- allData_id %>%
  select(Year.Identifier,Borough, Sub.Borough.Area, GEO.id2,immigrant,
         Monthly.contract.rent,
         Total.Household.Income.Recode,
         Household.Sampling.Weight..5.implied.decimal.places.) %>%
  mutate(Year = ifelse(Year.Identifier < 100,1900+Year.Identifier,Year.Identifier)) %>%
  rename(SubBorough = Sub.Borough.Area) %>%
  group_by(Year,Borough, SubBorough, GEO.id2) %>%
  mutate(samplingWeight = Household.Sampling.Weight..5.implied.decimal.places. / 100000,
         immigrantNum = round(sum(immigrant*samplingWeight)),
         totalNum = round(sum(samplingWeight)),
         monthlyContractRent = ifelse(Monthly.contract.rent %in% c(99998,99999),NA,Monthly.contract.rent),
         totalHouseholdIncome = ifelse(Total.Household.Income.Recode %in% c(999998, 9999999), NA,Total.Household.Income.Recode),
         immigrantPct = round((immigrantNum/totalNum * 100), digits = 2),
         avgMonthlyContractRent = round(mean(monthlyContractRent,na.rm = T),digits = 2),
         avgTotalHouseholdIncome = round(mean(totalHouseholdIncome,na.rm = T),digits = 2) 
  ) %>%
  select(Year,Borough, SubBorough, GEO.id2,immigrantNum,fgenNum,
         sgenNum,totalNum,
         immigrantPct,fgenPct,sgenPct,avgMonthlyContractRent,avgTotalHouseholdIncome) %>%
  distinct() 

# Adjust for inflation in income and rent
currentCPI <- 361
completeData <- completeData %>%
  mutate(avgTotalHouseholdIncome2 = case_when(
    Year == 1991 ~ avgTotalHouseholdIncome * (currentCPI / 205.1),
    Year == 1993 ~ avgTotalHouseholdIncome * (currentCPI / 215.5),
    Year == 1996 ~ avgTotalHouseholdIncome * (currentCPI / 231.3),
    Year == 1999 ~ avgTotalHouseholdIncome * (currentCPI / 244.6),
    Year == 2002 ~ avgTotalHouseholdIncome * (currentCPI / 264.2),
    Year == 2005 ~ avgTotalHouseholdIncome * (currentCPI / 286.9),
    Year == 2008 ~ avgTotalHouseholdIncome * (currentCPI / 316.3),
    Year == 2011 ~ avgTotalHouseholdIncome * (currentCPI / 330.5),
    Year == 2014 ~ avgTotalHouseholdIncome * (currentCPI / 348.3),
    Year == 2017 ~ avgTotalHouseholdIncome * (currentCPI / 361) ),
    avgTotalMonthlyRent2 = case_when(
      Year == 1991 ~ avgMonthlyContractRent * (currentCPI / 205.1),
      Year == 1993 ~ avgMonthlyContractRent * (currentCPI / 215.5),
      Year == 1996 ~ avgMonthlyContractRent * (currentCPI / 231.3),
      Year == 1999 ~ avgMonthlyContractRent * (currentCPI / 244.6),
      Year == 2002 ~ avgMonthlyContractRent * (currentCPI / 264.2),
      Year == 2005 ~ avgMonthlyContractRent * (currentCPI / 286.9),
      Year == 2008 ~ avgMonthlyContractRent * (currentCPI / 316.3),
      Year == 2011 ~ avgMonthlyContractRent * (currentCPI / 330.5),
      Year == 2014 ~ avgMonthlyContractRent * (currentCPI / 348.3),
      Year == 2017 ~ avgMonthlyContractRent * (currentCPI / 361) ) ) 