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


#####################
# Crime Data
crime2 <- read_xls("crimeData.xls",skip = 2,n_max=616)
crime2$precinct <- rep(unique(crime2$PCT)[-2],each=8)
crimeData2 <- crime2 %>%
  filter(CRIME =="TOTAL SEVEN MAJOR FELONY OFFENSES") %>%
  dplyr::select(-PCT) %>%
  tidyr::gather(Year,CrimeCount,"2000":"2018") %>%
  mutate(Year = as.numeric(Year)) %>%
  filter(Year %in% c(seq(2002,2017,by=3))) %>%
  rename(crime = CRIME)
#save(crimeData2,file="Crime2.Rdata")


######################
# Education Data
edu <- read_xlsx(path = "gradRates.xlsx",sheet = 6) %>%
  mutate(ClassOf = `Cohort Year`+4,
         Year = case_when(
           ClassOf %in% c(2010,2011) ~ 2011,
           ClassOf %in% c(2012:2014) ~ 2014,
           ClassOf %in% c(2015) ~ 2017)) %>%
  #filter(ClassOf %in% c(2011,2014)) %>%
  rename(TotalAPMCohort =`# APM Cohort`,
         EnteringYear = `Cohort Year`,
         AchievingAPM = `# achieving APM`) %>%
  dplyr::select(DBN,School,ClassOf,Year,EnteringYear,TotalAPMCohort,AchievingAPM) %>%
  mutate(sd = substr(DBN,1,2),
         randomLetter = substr(DBN,3,3),
         sID = substr(DBN,4,7),
         borough = case_when(
           randomLetter == "M" ~ "Manhattan",
           randomLetter == "X" ~ "Bronx",
           randomLetter == "K" ~ "Brooklyn",
           randomLetter == "Q" ~ "Queens",
           randomLetter == "R" ~ "Staten Island",
           TRUE ~ "Else") ) %>%
  dplyr::select(borough,sd,sID,ClassOf,Year,EnteringYear,TotalAPMCohort,AchievingAPM)

edu <- edu %>%
  group_by(Year, sd,borough) %>%
  mutate(AchievingAPM = as.numeric(ifelse(AchievingAPM == "s",NA,AchievingAPM)),
         TotalAPMCohort = as.numeric(ifelse(TotalAPMCohort == "s",NA,TotalAPMCohort))) %>%
  summarise(nSchools = n(),
            APMCohort = sum(TotalAPMCohort,na.rm = T),
            Achieved = sum(AchievingAPM,na.rm = T),
            AchievedRate = Achieved / APMCohort)

######################
# Health Data
mortality <- read.csv("mortality.csv") %>%
  select(Community_District,Age__28days:Age_85_,Total,Year)

AgeLevels <- sort(colnames(mortality)[-c(1,22,23)],decreasing = T)
cdLevels <- as.character(unique(mortality$Community_District))

mortality <- mortality %>%
  gather(AgeBucket,DeathCount,Age__28days:Age_85_) %>%
  mutate(AgeBucket = factor(AgeBucket,levels=AgeLevels)) %>%
  group_by(Community_District,Year) %>%
  arrange(AgeBucket) %>%
  mutate(MedianSearchValue = Total/2,
         DeathCount= ifelse(is.na(DeathCount),0,DeathCount),
         CumulativeDeathCount = cumsum(DeathCount),
         MedianIndicator = ifelse(CumulativeDeathCount >= MedianSearchValue,1,0)) %>%
  filter(MedianIndicator == 1) %>%
  filter(CumulativeDeathCount == min(CumulativeDeathCount)) %>%
  rename(TotalDeaths = Total, 
         MedianDeathAge = AgeBucket) %>%
  dplyr::select(Community_District,Year,TotalDeaths,MedianDeathAge)

######################
# Aggregating Data

# Read in shape file
boros = readOGR("C:/Users/POA5/Documents/MiamiAlison3/ASA_Comp/ASA_Data/BoroughMap/nybb.shp")

# Look for NYC specific places
nyplaces = pumas("NY",year=2017)

# Extract the geoids we need from the NYCHVS data
geoID = c(3603710L, 3603802L, 3603707L, 3604006L, 3604009L, 3604010L, 
           3603801L, 3604007L, 3604114L, 3604016L, 3603804L, 3604105L, 3604015L, 
           3603803L, 3604112L, 3604018L, 3604104L, 3603805L, 3604014L, 3604113L, 
           3604017L, 3604111L, 3604106L, 3604013L, 3603702L, 3603806L, 3604011L, 
           3603704L, 3604103L, 3604008L, 3603703L, 3604108L, 3604012L, 3603808L, 
           3603709L, 3604110L, 3604005L, 3603701L, 3603807L, 3604107L, 3603706L, 
           3604002L, 3604102L, 3604003L, 3603809L, 3603901L, 3603708L, 3604109L, 
           3603902L, 3604004L, 3603705L, 3603810L, 3604101L, 3603903L, 3604001L)

# Extract the geoids we have in the nyplaces
nyGeoID = unique(as.numeric(nyplaces@data$GEOID10))

# Double check if we have all the geoids we need (should be true)
geoID %in% nyGeoID %>% sum ==55

# Extract only the good ones
sub_nyc = nyplaces[nyplaces@data$GEOID10 %in% geoID,]

# outline = gUnaryUnion(boros)
# row.names(sub_nyc@data) = getSpPPolygonsIDSlots(gIntersection(sub_nyc, outline, byid = T)) 
# sub_nyc = SpatialPolygonsDataFrame(
#   gIntersection(sub_nyc, outline, byid = T),
#   data = sub_nyc@data
# )

#save(sub_nyc,file = "sub_borough_shapefile.RData")
load("sub_borough_shapefile.RData")

# Read in individual shape files
community = spTransform(readOGR("ASA_Data/nycd_19a/nycd.shp"),
                        CRSobj = CRS("+init=epsg:2263"))
police =    spTransform(readOGR("ASA_Data/nypp_19a/nypp.shp"),
                        CRSobj = CRS("+init=epsg:2263"))
school =    spTransform(readOGR("ASA_Data/nysd_19a/nysd.shp"),
                        CRSobj = CRS("+init=epsg:2263"))
borough = spTransform(sub_nyc,
                      CRSobj = CRS("+init=epsg:2263"))

# Fix up self intersections by making width=0
community = gBuffer(community, byid = TRUE, width = 0)
police = gBuffer(police, byid = TRUE, width = 0)
school = gBuffer(school, byid = TRUE, width = 0)
borough = gBuffer(borough, byid = TRUE, width = 0)

# Intersect all the files, drop all points/lines and keep only polygons
com_pol = gIntersection(community, police, byid = TRUE, drop_lower_td = TRUE)
com_pol_school = gIntersection(com_pol, school, byid = T, drop_lower_td = TRUE)
com_pol_school= gBuffer(com_pol_school, byid = T,width = 0)
com_pol_school_bor = gIntersection(com_pol_school,borough, byid = T, drop_lower_td = TRUE)
com_pol_school_bor= gBuffer(com_pol_school_bor, byid = T,width = 0)

# Project data back to geographic coordinate system (long/lat)
cps = spTransform(com_pol_school_bor,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# Store the IDs needed for later merging
community@data$c = sapply(community@polygons, function(x) slot(x,"ID"))
police@data$p = sapply(police@polygons, function(x) slot(x,"ID"))
school@data$s = sapply(school@polygons, function(x) slot(x,"ID"))
borough@data$b = sapply(borough@polygons, function(x) slot(x,"ID"))

# Parse the automatically created IDs into useful columns for each shapefile
regions = sapply(cps@polygons, function(x) slot(x,"ID")) %>%
  str_split(" ",simplify = T) %>% as.data.frame() %>% mutate_if(is.factor, as.character)
names(regions) = c("c","p","s","b","id")
#regions <- regions[,-5]

#left join on the real IDs
regions = left_join(regions,community@data %>% dplyr::select(BoroCD,c))
regions = left_join(regions,police@data %>% dplyr::select(Precinct,p))
regions = left_join(regions,school@data %>% dplyr::select(SchoolDist,s))
regions = left_join(regions,borough@data %>% dplyr::select(GEOID10,b) %>% mutate(b=str_split(b," ",simplify = T)[,1],
                                                                                 id=as.character(rep(1,55))))
#regions <- regions[,-5]

#set row names so we can make an spdf
row.names(regions) = paste(regions$c,regions$p,regions$s,regions$b,regions$id)

#this is the final object you need.
#left join all the  data files onto cps_regions@data 
cps_regions = SpatialPolygonsDataFrame(cps,regions)

#check that these both are 1 (data matches order of polygons)
mean(rownames(cps_regions@data) == sapply(cps_regions@polygons, function(x) slot(x,"ID")))
mean(rownames(cps_regions@data) == sapply(cps@polygons, function(x) slot(x,"ID")))


