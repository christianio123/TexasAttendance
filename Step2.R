


##################################################################################################
##################################################################################################
# COVID AND DISTRICT LEVEL DATA



# CountyLevel CovidData

CovidCountyLevelData <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv') %>%
  select(-c(3,4,6)) %>%
  rename(Day = 'date', County = 'county', CovidTotalCountyCases = 'cases') %>%
  group_by(County, Day) %>%
  summarize(Day = unique(Day), CovidTotalCountyCases = sum(CovidTotalCountyCases))


# Join Covid County Dataset with District Dataset

DistrictLevelDataset <- left_join(DistrictLevelDataset, CovidCountyLevelData, by = c('Day','County') ) %>%
  group_by(County, District, DistrictNumber) %>%
  mutate(CovidCountyPercentIncrease = ((CovidTotalCountyCases / lag(CovidTotalCountyCases))-1)*100)




# StateLevel CovidData

CovidStateLevelData <- read_csv('https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv')%>% 
  filter(state == 'Texas') %>%
  select(-c(2,3,5)) %>%
  rename(Day = date, CovidTotalStateCases = cases)
  
# Join Covid State Dataset with District Dataset

DistrictLevelDataset <- left_join(DistrictLevelDataset, CovidStateLevelData, by = c('Day')) %>%
  group_by(County, District, DistrictNumber) %>%
  mutate(CovidStatePercentIncrease = ((CovidTotalStateCases / lag(CovidTotalStateCases))-1)*100)




# COVID AND SCHOOL LEVEL DATA

# Covid County Join
SchoolLevelDataset <- left_join(SchoolLevelDataset, CovidCountyLevelData, by = c('Day','County') ) %>%
  group_by(County, School, DistrictNumber) %>%
  mutate(CovidCountyPercentIncrease = ((CovidTotalCountyCases / lag(CovidTotalCountyCases))-1)*100)
# Covid State Join
SchoolLevelDataset <- left_join(SchoolLevelDataset, CovidStateLevelData, by = c('Day')) %>%
  group_by(County, School, DistrictNumber) %>%
  mutate(CovidStatePercentIncrease = ((CovidTotalStateCases / lag(CovidTotalStateCases))-1)*100)

# Join Covid County and State Data with GradeLevelDataset

GradeLevelDataset <- left_join(GradeLevelDataset, CovidCountyLevelData, by = c('Day','County') ) %>%
  group_by(GradeLevel, County, DistrictNumber) %>%
  mutate(CovidCountyPercentIncrease = ((CovidTotalCountyCases / lag(CovidTotalCountyCases))-1)*100)

GradeLevelDataset <- left_join(GradeLevelDataset, CovidStateLevelData, by = c('Day')) %>%
  group_by(GradeLevel, County, DistrictNumber) %>%
  mutate(CovidStatePercentIncrease = ((CovidTotalStateCases / lag(CovidTotalStateCases))-1)*100)



##########################################################################################################
##########################################################################################################

# Join census demographic data on Zip

#Zips <- Zips %>% select()


Zips <- read_xlsx(".\\TexasZips.xlsx")
Info <- Zips[c(2,9,18:26, 46, 52, 54)]
colnames(Info) <- c("School_Zip", "HousholdMediumIncome", "Population", "White",
                    "Black", "Native", "Asian", "Islander", "Other", "Two","Hispanic",
                    "PercentWithoutDegree", "PercentFamilyPoverty", "UnemploymentRate")
Info <- Info[5:1950,]
Info[-1] <- as.data.frame(sapply(Info[-1], as.numeric))
Info <- Info %>%
  mutate(White = (White/Population) * 100, Black = (Black/Population) * 100, Native = (Native / Population) * 100,
         Asian = (Asian / Population) * 100, Islander = (Islander / Population) * 100, Other = (Other / Population) * 100,
         Two = (Two / Population) * 100, Hispanic = (Hispanic / Population) * 100, PercentWithoutDegree = PercentWithoutDegree * 100,
         PercentFamilyPoverty = PercentFamilyPoverty * 100, UnemploymentRate = UnemploymentRate * 100)
Info[-c(1:3)] <- as.data.frame(sapply(Info[-c(1:3)], round, 2))

SchoolLevelDataset <- left_join(SchoolLevelDataset, Info, by = 'School_Zip')



######################################################################################
######################################################################################


# Add longitude and latitude based on zip

# Latitude and Longitude Data join on Zip Code



zips_locations <- read.csv(".\\us-zip-code-latitude-and-longitude.csv", sep=';')[c(1,4,5)] %>%
  rename('School_Zip' = 'Zip') %>%
  mutate('School_Zip' = as.character(School_Zip))


SchoolLevelDataset <- left_join(SchoolLevelDataset, zips_locations, by = c('School_Zip'))




######################################################################################
######################################################################################

# Add Weather Data



Weather_Data <- read_csv('Filtered_Weather_Data.csv') %>%
  mutate(Day = mdy(Day))


CompleteGradeLevelDataset <- left_join(GradeLevelDataset, Weather_Data, by = c('Day','County'))
CompleteSchoolLevelDataset <- left_join(SchoolLevelDataset, Weather_Data, by = c('Day','County'))
CompleteDistrictLevelDataset <- left_join(DistrictLevelDataset, Weather_Data, by = c('Day','County'))




variables <- ls()
all_data <- grepl('Complete', ls())
rm(list = variables[!all_data])


#write.csv(CompleteGradeLevelDataset, "GradeLevelData_V3.csv")
#write.csv(CompleteSchoolLevelDataset, "SchoolLevelData_V3.csv")
#write.csv(CompleteDistrictLevelDataset, "DistrictLevelData_V3.csv")



