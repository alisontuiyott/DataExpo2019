# Read in necessary libraries & set directory
setwd("C:/Users/POA5/Documents/MiamiAlison3/ASA_Comp")
library(pacman)
p_load(ggplot2,Stack,dplyr,ggrepel,gridExtra,tidyr,
       leaflet,plotly,readxl,rgdal,tigris,rgdal,rgeos,
       maptools,sp,scales,stringr,raster)

############ Read in data ##############
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

######## Complete Data W/ Sampling Weights and Immigration numbers ##########
completeData <- allData_id %>%
  dplyr::select(Year.Identifier,Borough, Sub.Borough.Area, GEO.id2,immigrant,
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
  dplyr::select(Year,Borough, SubBorough, GEO.id2,immigrantNum,totalNum,
         immigrantPct,avgMonthlyContractRent,avgTotalHouseholdIncome) %>%
  distinct() 

############ Adjust for inflation in income and rent #############
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


########## Crime Data ############

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
#load("Crime2.Rdata")

############ Education Data ###########

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
         TotalAPMCohort = as.numeric(ifelse(TotalAPMCohort == "s",NA,TotalAPMCohort)),
         schooldist = as.numeric(as.character(sd))) %>%
  group_by(Year, schooldist,borough) %>%
  summarise(nSchools = n(),
            APMCohort = sum(TotalAPMCohort,na.rm = T),
            Achieved = sum(AchievingAPM,na.rm = T),
            AchievedRate = Achieved / APMCohort)

######### Health Data ##############

mortality <- read.csv("mortality.csv") %>%
  dplyr::select(Community_District,Age__28days:Age_85_,Total,Year)

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

# Read in birth data
birth <- read.csv("birth.csv")

# Create key file for community district name and code
cdKey <- as.data.frame(cbind(cdName = cdLevels,
                             cdCode = as.numeric(as.character(birth$cd)))) %>%
  distinct()

# Join health related tables
health <- cdKey %>%
  mutate(cdCode = as.numeric(as.character(cdCode))) %>%
  #left_join(birthData,by=c("cdCode"="cd")) %>%
  left_join(mortality, by=c("cdName" = "Community_District"))
#save(health, file="Health.Rdata")

######## Aggregating Data ##############

# Read in shape file
boros = spTransform(readOGR("C:/Users/POA5/Documents/MiamiAlison3/ASA_Comp/ASA_Data/BoroughMap/nybb.shp"),
                    CRSobj = CRS("+init=epsg:2263"))


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
sub_nyc = spTransform(nyplaces[nyplaces@data$GEOID10 %in% geoID,],
                      CRSobj = CRS("+init=epsg:2263"))

outline = gUnaryUnion(boros)
row.names(sub_nyc@data) = getSpPPolygonsIDSlots(gIntersection(sub_nyc, outline, byid = T))
sub_nyc = SpatialPolygonsDataFrame(
  gIntersection(sub_nyc, outline, byid = T),
  data = sub_nyc@data
)

#save(sub_nyc,file = "sub_borough_shapefile.RData")
#load("sub_borough_shapefile.RData")

# Read in individual shape files
borough = spTransform(sub_nyc,
                      CRSobj = CRS("+init=epsg:2263"))
police =    spTransform(readOGR("ASA_Data/nypp_19a/nypp.shp"),
                        CRSobj = CRS("+init=epsg:2263"))
school =    spTransform(readOGR("ASA_Data/nysd_19a/nysd.shp"),
                        CRSobj = CRS("+init=epsg:2263"))
community = spTransform(readOGR("ASA_Data/nycd_19a/nycd.shp"),
                        CRSobj = CRS("+init=epsg:2263"))

# Fix up self intersections by making width=0
borough = gBuffer(borough, byid = TRUE, width = 0)
police = gBuffer(police, byid = TRUE, width = 0)
school = gBuffer(school, byid = TRUE, width = 0)
community = gBuffer(community, byid = TRUE, width = 0)


# Intersect all the files, drop all points/lines and keep only polygons
bor_pol = gIntersection(borough, police, byid = TRUE, drop_lower_td = TRUE)
bor_pol = gBuffer(bor_pol, byid = T, width = 0)
bor_pol_school = gIntersection(bor_pol, school, byid = TRUE, drop_lower_td = TRUE)
bor_pol_school = gBuffer(bor_pol_school, byid = T,width = 0)
bor_pol_school_com = gIntersection(bor_pol_school,community, byid = T, drop_lower_td = TRUE)
bor_pol_school_com = gBuffer(bor_pol_school_com, byid = T,width = 0)


# Project data back to geographic coordinate system (long/lat)
# bp = spTransform(bor_pol,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
# bps = spTransform(bor_pol_school,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
bpsc = spTransform(bor_pol_school_com,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))


# Store the IDs needed for later merging
borough@data$b = sapply(borough@polygons, function(x) slot(x,"ID"))
police@data$p = sapply(police@polygons, function(x) slot(x,"ID"))
school@data$s = sapply(school@polygons, function(x) slot(x,"ID"))
community@data$c = sapply(community@polygons, function(x) slot(x,"ID"))


# # Parse the automatically created IDs into useful columns for each shapefile - borough & police
# bp_regions = sapply(bp@polygons, function(x) slot(x,"ID")) %>%
#   str_split(" ",simplify = T) %>% 
#   as.data.frame() %>% 
#   mutate_if(is.factor, as.character)
# names(bp_regions) = c("b","id","p")
# 
# # Parse the automatically created IDs into useful columns for each shapefile - borough & police & school
# bps_regions = sapply(bps@polygons, function(x) slot(x,"ID")) %>%
#   str_split(" ",simplify = T) %>% 
#   as.data.frame() %>% 
#   mutate_if(is.factor, as.character)
# names(bps_regions) = c("b","id","p","s")

# Parse the automatically created IDs into useful columns for each shapefile - borough & police & school & community
bpsc_regions = sapply(bpsc@polygons, function(x) slot(x,"ID")) %>%
            str_split(" ",simplify = T) %>% 
            as.data.frame() %>% 
            mutate_if(is.factor, as.character)
names(bpsc_regions) = c("b","id","p","s","c")

# # Left join on the real IDs - borough & police regions
# bp_regions = left_join(bp_regions,borough@data %>% dplyr::select(GEOID10,b) %>% 
#                                                     mutate(b=str_split(b," ",simplify = T)[,1],
#                                                           id=as.character(rep(1,55))))
# bp_regions = left_join(bp_regions,police@data %>% dplyr::select(Precinct,p))
# 
# # Left join on the real IDs - borough & police & school regions
# bps_regions = left_join(bps_regions,borough@data %>% dplyr::select(GEOID10,b) %>% 
#                          mutate(b=str_split(b," ",simplify = T)[,1],
#                                 id=as.character(rep(1,55))))
# bps_regions = left_join(bps_regions,police@data %>% dplyr::select(Precinct,p))
# bps_regions = left_join(bps_regions,school@data %>% dplyr::select(SchoolDist,s))

# Left join on the real IDs - borough & police & school & community regions
bpsc_regions = left_join(bpsc_regions,borough@data %>% dplyr::select(GEOID10,b) %>% 
                          mutate(b=str_split(b," ",simplify = T)[,1],
                                 id=as.character(rep(1,55))))
bpsc_regions = left_join(bpsc_regions,police@data %>% dplyr::select(Precinct,p))
bpsc_regions = left_join(bpsc_regions,school@data %>% dplyr::select(SchoolDist,s))
bpsc_regions = left_join(bpsc_regions,community@data %>% dplyr::select(BoroCD,c))


# # Set row names so we can make an spdf - borough & police regions
# row.names(bp_regions) = paste(bp_regions$b, bp_regions$id,
#                               bp_regions$p)
# 
# # Set row names so we can make an spdf - borough & police & school regions
# row.names(bps_regions) = paste(bps_regions$b, bps_regions$id,
#                                bps_regions$p,
#                                bps_regions$s)


# Set row names so we can make an spdf - borough & police & school & community regions
row.names(bpsc_regions) = paste(bpsc_regions$b, bpsc_regions$id,
                                bpsc_regions$p,
                                bpsc_regions$s,
                                bpsc_regions$c)

# Left join all the  data files onto cps_bpsc_regions@data
# bp_regions = SpatialPolygonsDataFrame(bp,bp_regions)
# bps_regions = SpatialPolygonsDataFrame(bps,bps_regions)
bpsc_regions = SpatialPolygonsDataFrame(bpsc,bpsc_regions)

# Check that these both are 1 (data matches order of polygons)
# mean(rownames(bp_regions@data) == sapply(bp_regions@polygons, function(x) slot(x,"ID")))
# mean(rownames(bp_regions@data) == sapply(bp@polygons, function(x) slot(x,"ID")))
# 
# mean(rownames(bps_regions@data) == sapply(bps_regions@polygons, function(x) slot(x,"ID")))
# mean(rownames(bps_regions@data) == sapply(bps@polygons, function(x) slot(x,"ID")))

mean(rownames(bpsc_regions@data) == sapply(bpsc_regions@polygons, function(x) slot(x,"ID")))
mean(rownames(bpsc_regions@data) == sapply(bpsc@polygons, function(x) slot(x,"ID")))

############## Happy Score Calculation #############################

# Join all the data into one table
allYears <- bpsc_regions@data %>%
  mutate(GEOID10 = as.numeric(GEOID10)) %>%
  left_join(completeData,by=c("GEOID10"="GEO.id2")) %>%
  left_join(crimeData2,by=c("Precinct"="precinct",
                              "Year")) %>%
  left_join(edu,by=c("SchoolDist"="schooldist",
                     "Year")) %>%
  left_join(health,by=c("BoroCD"="cdCode",
                        "Year"="Year"))  %>%
  filter(Year == 2014)

# Create happy score index
scaledBoroughs = cbind(data.frame(area = area(bpsc_regions)), allYears) %>%
  group_by(b) %>%
  summarise(crime = weighted.mean(-CrimeCount,w = area,na.rm=T),
            inc = weighted.mean(avgTotalHouseholdIncome2,w = area,na.rm=T),
            rent = weighted.mean(-avgTotalMonthlyRent2/avgTotalHouseholdIncome2,w = area,na.rm=T),
            edu = weighted.mean(AchievedRate,w = area,na.rm=T),
            death = weighted.mean(as.numeric(stringr::str_sub(MedianDeathAge, 5,6)), w=area,na.rm=T)) %>%
  ungroup() %>%
  mutate_if(is.numeric,rescale,to=c(0,1)) %>%
  mutate(happiness = crime+inc+rent+edu+death) 

# Merge happy score w/ immigrant pct from allYears
geoB <- allYears %>%
  ungroup() %>%
  dplyr::select(b,GEOID10) %>%
  distinct()

scaledBoroughs = as.data.frame(scaledBoroughs) %>%
  left_join(geoB,by="b") %>%
  left_join(filter(completeData,Year==2014),by=c("GEOID10"="GEO.id2"))
row.names(scaledBoroughs) = paste(scaledBoroughs$b,rep(1,length(scaledBoroughs$b)))

############# Plot the final result (happy score & immigrant pct) ########

# Create shape and outline
boroughMap = spTransform(borough,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
boroughMap <- SpatialPolygonsDataFrame(boroughMap,scaledBoroughs)

mean(rownames(boroughMap@data) == sapply(boroughMap@polygons, function(x) slot(x,"ID")))
mean(rownames(boroughMap@data) == sapply(boroughMap@polygons, function(x) slot(x,"ID")))

# Labelling (add labels for borough)
nycBoroughs <- data.frame(plotCode=c(0:4),
                          name=c("Manhattan","Bronx","Staten Island",
                                 "Brooklyn", "Queens"),
                          dataCode=c(3,1,5,2,4))


b2 <- merge(fortify(boroughMap), as.data.frame(boroughMap), by.x="id", by.y=0) %>%
  left_join(nycBoroughs, by=c("Borough" = "dataCode"))



b_labels <- b2 %>%
  group_by(Borough) %>%
  summarize(lat = (max(lat) + min(lat)) / 2,
            long = (max(long) + min(long)) / 2)
b_labels<- left_join(b_labels,nycBoroughs,by=c("Borough"="dataCode")) %>%
  mutate(lat = ifelse(name=="Queens",lat+0.05,lat),
         long = ifelse(name=="Bronx",long-0.02,long))

# Save the data
#save.image(file="CleanedData_Sep15.Rdata")
#load("CleanedData_Sep15.Rdata")

# Edited 9/25 data
#save.image(file="CleanedData_Sep25.Rdata")
load("CleanedData_Sep25.Rdata")

# Plot Immigrant Residency, Happiness, Household Income, and Rent
ggplot(b2) +
  geom_polygon(aes(x=long,y=lat,fill=immigrantPct,group = group),color="gray40",alpha=0.7)+
  coord_quickmap()+
  theme_minimal()+
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),color="black",size=5)+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        plot.title = element_text(size=18, hjust=.5))+
  # scale_fill_gradient(name="Happiness\nScore\nIndex",limits=c(0,5),
  #                     high="goldenrod", low="white")
  scale_fill_gradient(name="Immigrant\nResidency",limits=c(15,80),
                      breaks=seq(20,80,by=20),labels=paste0(seq(20,80,by=20),"%"),
                      high="#6e016b", low="white")
# scale_fill_gradient(name="Avg Total\nHousehold\nIncome",limits=c(30000,210000),
#                     breaks=seq(30000,210000,by=45000),labels=paste0("$",seq(30,210,by=45),"K"),
#                     high="#005a32", low="white")
# scale_fill_gradient(name="Avg Total\nMonthly\nRent",limits=c(800,2800),
#                     breaks=seq(800,2800,by=400),labels=c("$800",paste0("$",seq(1.2,2.8,by=.4),"K")),
#                     high="#252525", low="#ffffff")


# Plot Crime
crime <- data.frame(crimeData2) %>%
  filter(Year==2014) 
policeMap = gBuffer(police, byid = TRUE, width = 0)
policeMap = spTransform(policeMap,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# Change the row names of the policeMap to precinct
row.names(policeMap@data) = as.character(policeMap@data$Precinct)
row.names(crime) = as.character(0:76)

# Left join all the  data files onto police@data 
policeMap <- SpatialPolygonsDataFrame(policeMap,crime)
mean(rownames(policeMap@data) == sapply(policeMap@polygons, function(x) slot(x,"ID")))
mean(rownames(policeMap@data) == sapply(policeMap@polygons, function(x) slot(x,"ID")))

crimePlot <- merge(fortify(policeMap), as.data.frame(policeMap), by.x="id", by.y=0)

# Plot Crime data
ggplot(crimePlot) +
  geom_polygon(aes(x=long,y=lat,fill=CrimeCount,group = group),color="gray40",alpha=0.7)+
  coord_quickmap()+
  theme_minimal()+
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),color="black",size=5)+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        plot.title = element_text(size=18, hjust=.5))+
  scale_fill_gradient(name="Total\nMajor\nFelony\nOffenses",limits=c(80,3800),
                      breaks=seq(500,4000,by=1000),labels=c("< 500","1,500","2,500","> 3,500"),
                      high="#cb181d", low="white")

# Plot Education
edu2 <- data.frame(edu) %>%
  filter(Year==2014)
schoolMap = gBuffer(school, byid = TRUE, width = 0)
schoolMap = spTransform(schoolMap,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# Left join all the  data files onto police@data 
schoolMap@data <- left_join(schoolMap@data,edu2,by=c("SchoolDist" = "schooldist"))
eduPlot <- merge(fortify(schoolMap), as.data.frame(schoolMap), by.x="id", by.y=0)
ggplot(eduPlot) +
  geom_polygon(aes(x=long,y=lat,fill=AchievedRate,group = group),color="gray40",alpha=0.7)+
  coord_quickmap()+
  theme_minimal()+
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),color="black",size=5)+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        plot.title = element_text(size=18, hjust=.5))+
  scale_fill_gradient(name="Achieved\nRate",limits=c(0,0.6),
                      breaks=seq(0,0.6,by=0.2),labels=paste0(seq(0,60,by=20),"%"),
                      high="#2171b5", low="white")

# Plot Health
health2 <- data.frame(health) %>%
  filter(Year==2014) %>%
  mutate(MedianDeathAge = as.character(MedianDeathAge),
         MedianDeathAge = as.factor(MedianDeathAge))
communityMap = gBuffer(community, byid = TRUE, width = 0)
communityMap = spTransform(communityMap,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
row.names(health2) = as.character(health2$cdCode)

#left join all the  data files onto police@data 
communityMap@data <- left_join(communityMap@data,health2,by=c("BoroCD" = "cdCode"))
cdPlot <- merge(fortify(communityMap), as.data.frame(communityMap), by.x="id", by.y=0)
ggplot(cdPlot) +
  geom_polygon(aes(x=long,y=lat,fill=MedianDeathAge,group = group),color="gray40",alpha=0.7)+
  coord_quickmap()+
  theme_minimal()+
  geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),color="black",size=5)+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.background = element_rect(colour = "gray20"),
        plot.title = element_text(size=18, hjust=.5))+
  scale_fill_manual(name="Median\nDeath\nAge Range",
                    breaks=c("Age_65_69","Age_70_74","Age_75_79","Age_80_84",NA),
                    labels=c("65-69","70-74","75-79","80-84","NA"),
                    values = c("#fed98e","#fe9929","#d95f0e","#993404","white"))

# Plot aggregate data
# aggregateMap <- merge(fortify(cps_regions), as.data.frame(cps_regions), by.x="id", by.y=0)
# ggplot(aggregateMap) +
#   geom_polygon(aes(x=long,y=lat,group = group),fill="#dfc27d",color="gray20",alpha=0.7)+
#   coord_quickmap()+
#   theme_minimal()+
#   geom_text(data= b_labels, aes(x=long, y=lat, label = name,fontface=2),color="black",size=5)+
#   theme(axis.ticks = element_blank(),
#         axis.text.x = element_blank(), axis.title.x=element_blank(),
#         axis.text.y = element_blank(), axis.title.y=element_blank(),
#         panel.border = element_blank(), 
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank(),
#         legend.background = element_rect(colour = "gray20"),
#         plot.title = element_text(size=18, hjust=.5))

########## Scatter plot ########

# Scatter plot of happy index vs. immigrant pct with area as size
ggplot(data=b2) +
  geom_point(aes(x=immigrantPct,y=happiness,color=name,size=totalNum),alpha=0.7)+
  labs(x="Immigrant Residency", y="Happy Score Index") +
  theme_minimal()+
  theme(legend.background = element_rect(colour = "gray20"),
        legend.text = element_text(size=12),
        axis.title = element_text(size=12),
        axis.text = element_text(size=9))+
  scale_x_continuous(limits=c(15,75),breaks = seq(10,90,by=10), labels=paste0(seq(10,90,by=10),"%"))+
  scale_y_continuous(limits=c(0.75,5),breaks = seq(1,5))+
  scale_color_discrete(name = "Borough")+
  scale_size_continuous(name="Total Households",limits = c(35000,120000),
                        breaks = seq(40000,120000,by=40000),
                        labels = paste0(seq(40,120,by=40),"K"),
                        range = c(1, 10))

########## Three aggregate plots ########

# Select Brooklyn geo id's
brooklyn_geo <- c(3604003, 3604002, 3604018, 3604006, 3604008, 3604014, 
                  3604010, 3604016, 3604017,
                  3604007, 3604009, 3604015, 3604011, 3604012, 
                  3604005, 3604013, 3604004, 3604001)
brooklyn_aggregate <- merge(fortify(bpsc_regions), 
                          as.data.frame(bpsc_regions), by.x="id", by.y=0) %>%
                    filter(GEOID10 %in% brooklyn_geo)
brooklyn_p <- unique(brooklyn_aggregate$p)
brooklyn_s <- unique(brooklyn_aggregate$s)
brooklyn_c <- unique(brooklyn_aggregate$c)

b_bMap <- merge(fortify(borough), 
                as.data.frame(borough), by.x="id", by.y=0) %>%
          filter(GEOID10 %in% brooklyn_geo)

b_pMap <- merge(fortify(police), 
                as.data.frame(police), by.x="id", by.y=0) %>%
          filter(p %in% brooklyn_p, Precinct < 100) 

b_sMap <- merge(fortify(school), 
                as.data.frame(school), by.x="id", by.y=0) %>%
          filter(s %in% brooklyn_s, !SchoolDist %in% c(24,27))

b_cMap <- merge(fortify(community), 
                as.data.frame(community), by.x="id", by.y=0) %>%
          filter(c %in% brooklyn_c, BoroCD < 400)

ggplot() +
  geom_polygon(data=b_bMap,aes(x=long,y=lat,group = group),
               color="#1f78b4", fill="white", size=0.7)+ # blue
  geom_polygon(data=b_pMap,aes(x=long,y=lat,group = group),
               color="#33a02c",fill=NA,size=0.7)+ # green
  geom_polygon(data=b_sMap,aes(x=long,y=lat,group = group),
               color="#d7191c",fill=NA,size=0.7)+ # red
  geom_polygon(data=b_cMap,aes(x=long,y=lat,group = group),
               color="#fdae61",fill=NA,size=0.7)+ # orange
  coord_equal()+
  theme_minimal()+
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(), axis.title.x=element_blank(),
        axis.text.y = element_blank(), axis.title.y=element_blank(),
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(size=18, hjust=.5))

# Save the data for the paper
#save.image(file="CleanedData_Sep25.Rdata")
load(file="CleanedData_Sep25.Rdata")


# Create plot with only color/borough legend
# scatb <- ggplot(data=b2) +
#   geom_point(aes(x=immigrantPct,y=happiness,color=name),alpha=0.7)+
#   labs(x="Immigrant Residency", y="Happy Score Index",
#        title = "Sub-borough Analysis of Immigrant Residency and Happy Score Index") +
#   theme_minimal()+
#   theme(legend.background = element_rect(colour = "gray20"),
#         legend.position = "top",
#         legend.text = element_text(size=12),
#         axis.title = element_text(size=12),
#         axis.text = element_text(size=11))+
#   guides(color = guide_legend(override.aes = list(size = 5)))+
#   scale_x_continuous(limits=c(15,75),breaks = seq(10,90,by=10), 
#                      labels=paste0(seq(10,90,by=10),"%"))+
#   scale_y_continuous(limits=c(0.75,5),breaks = seq(1,5))+
#   scale_color_brewer("",aesthetics = "colour",
#                      palette = "Set2",type="qual")
# 
# # Extract the colour legend - leg1
# leg1 <- gtable_filter(ggplot_gtable(ggplot_build(scatb)), "guide-box") 
# 
# # Create plot with only size legend
# scath <- ggplot(data=b2) +
#   geom_point(aes(x=immigrantPct,y=happiness,size=totalNum),alpha=0.7)+
#   labs(x="Immigrant Residency", y="Happy Score Index",
#        caption = "Figure 9") +
#   theme_minimal()+
#   theme(legend.background = element_rect(colour = "gray20"),
#         legend.position = "bottom",
#         legend.text = element_text(size=12),
#         axis.title = element_text(size=12),
#         axis.text = element_text(size=11))+
#   scale_x_continuous(limits=c(15,75),breaks = seq(10,90,by=10), 
#                      labels=paste0(seq(10,90,by=10),"%"))+
#   scale_y_continuous(limits=c(0.75,5),breaks = seq(1,5))+
#   scale_size_continuous(name="Total Households:",limits = c(35000,120000),
#                         breaks = seq(35000,120000,by=20000),
#                         labels = paste0(seq(35,120,by=20),"K"),
#                         range = c(1, 10))
# 
# # Extract the size legend - leg2
# leg2 <- gtable_filter(ggplot_gtable(ggplot_build(scath)), "guide-box") 
# 
# # Create a plot without any legend and both values
# scat <- ggplot(data=b2) +
#   geom_point(aes(x=immigrantPct,y=happiness,color=name,size=totalNum),alpha=0.7)+
#   labs(x="Immigrant Residency", y="Happy Score Index"#,
#        #title = "Sub-borough Analysis of Immigrant Residency and Happy Score Index",caption = "Figure 9"
#        ) +
#   theme_minimal()+
#   theme(legend.position =  "none",
#         legend.text = element_text(size=12),
#         axis.title = element_text(size=12),
#         axis.text = element_text(size=11))+
#   scale_x_continuous(limits=c(15,75),breaks = seq(10,90,by=10), 
#                      labels=paste0(seq(10,90,by=10),"%"))+
#   scale_y_continuous(limits=c(0.75,5),breaks = seq(1,5))
# 
# # Create one plot, layer on top of another
# scatNew <- arrangeGrob(leg1, scat, 
#          heights = unit.c(leg1$height, unit(1, "npc") - leg1$height), ncol = 1)
# 
# scatNew <- arrangeGrob(scatNew, leg2,
#           heights = unit.c(unit(1, "npc") - leg2$width, leg2$width), ncol = 1)
# 
# grid.newpage()
# grid.draw(scatNew)

