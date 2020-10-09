## libraries needed for cleaning

library(tidyverse)
library(lubridate)
library(vroom)
library(sf)
library(RcppRoll)

### code used to standardized fields in each city. 
### code selects long, lat, data, and crime. 
### this is the first in a series of steps to 
### clean crimes across each city. 

## atlanta 1
atlanta18_19 <- read_csv("atlanta18_19.csv")
atlanta18_19 <- atlanta18_19 %>%
  rename(lat = Latitude, long = Longitude, occur_date = `Occur Date`, UC2_Literal = `UCR Literal`) %>%
  write.csv("atlanta18_19.csv")
atlanta00 <- read_csv("atlanta.csv")
atlanta00 %>%
  mutate(occur_date = as.Date(occur_date, "%m/%d/%Y")) %>%
  write.csv("atlanta.csv")
atlanta_files <- c("atlanta.csv","atlanta18_19.csv") ## change caps
atlanta18_00 <- atlanta_files %>% 
  map_dfr(read_csv, col_types = cols(occur_date = col_date()), .id = "source")
atlanta_data <- atlanta18_00 %>%
  select(long,lat,occur_date,UC2_Literal) %>%
  rename(x = long, y = lat, date = occur_date, crime = UC2_Literal) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## austin 2
austin <- vroom("austin.csv")
austin_data <- austin %>% 
  select(`Longitude`, `Latitude`, `Occurred Date Time`, 
         `Highest Offense Description`) %>%
  rename(x = `Longitude`, y =`Latitude`, date = `Occurred Date Time`, 
         crime = `Highest Offense Description`)  %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## baltimore 3
baltimore <- read_csv("baltimore.csv")
baltimore_data <- baltimore %>% 
  select(`Longitude`, `Latitude`, `CrimeDate`, 
         `Description`) %>%
  rename(x = `Longitude`, y =`Latitude`, date = `CrimeDate`, 
         crime = `Description`) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## batonrouge fix x and y with clean 
batonrouge <- read_csv("batonrouge.csv", 
                       col_types = cols(x = col_double(),
                                        y = col_double(),
                                        GEOLOCATION = col_character())
)

batonrouge_data <- batonrouge %>%
  separate(col = GEOLOCATION, sep = "[()]", into = c("address","coords")) %>%
  separate(col = coords, sep = ",", into = c("y","x")) %>% 
  select(x, y, `OFFENSE DATE`, `OFFENSE DESCRIPTION`) %>%
  rename(x = x, y = y, date = `OFFENSE DATE`, crime = `OFFENSE DESCRIPTION`) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  mutate(x = as.double(x),
         y = as.double(y))

## boston 5
boston <- read_csv("boston.csv")
boston_data <-  boston %>%
  select(Long, Lat, `OCCURRED_ON_DATE`, `OFFENSE_DESCRIPTION`) %>%
  rename(x = Long , y = Lat, date = `OCCURRED_ON_DATE`, crime = `OFFENSE_DESCRIPTION`) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  filter(date >= as.Date("2018-01-01")) 

## chicago 6
chicago <- vroom("chicago.csv")
chicago_data <- chicago %>%
  select(Longitude, Latitude, Date, `Primary Type`, Description) %>%
  rename(x = Longitude, y = Latitude, date = Date, crime = `Primary Type`, type =  Description) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))%>%
  filter(date >= as.Date("2018-01-01")) 

## cincinnati 7
cincinnati <- vroom("cincinnati.csv")
cincinnati_data <- cincinnati %>%
  select(LONGITUDE_X, LATITUDE_X, DATE_REPORTED, OFFENSE) %>%
  rename(x = LONGITUDE_X, y = LATITUDE_X, date = DATE_REPORTED, crime = OFFENSE) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  filter(date >= as.Date("2018-01-01"))

## dallas 8
dallas <- read_csv("dallas.csv")
dallas$Location1  <- str_extract_all(dallas$Location1, "\\([^()]+\\)")
dallas$Location1 <- substring(dallas$Location1, 2, nchar(dallas$Location1)-1)
dallas <- separate(dallas, col = Location1, sep = ",", into = c("y", "x")) 
dallas_data <-  dallas %>%
  select(x, y, `Call Date Time`, `NIBRS Crime`) %>%
  rename(x = x, y = y, date = `Call Date Time`, crime = `NIBRS Crime`)%>% 
  mutate(date = as.Date(date, "%Y-%m-%d"))%>% 
  mutate(x = as.double(x)) %>%
  mutate(y = as.double(y))  %>%
  filter(date >= as.Date("2018-01-01"))

## dc 9
dc_files <- c("dc.csv","dc_2018.csv", "dc_2019.csv")
dc_18_00 <- dc_files %>% 
  map_dfr(read_csv, col_types = cols(CCN = col_character()), .id = "source")
dc_data <-  dc_18_00 %>%
  select(LONGITUDE, LATITUDE, REPORT_DAT, OFFENSE) %>%
  rename(x = LONGITUDE, y = LATITUDE, date = REPORT_DAT, crime = OFFENSE) 
dc_data$date <- sub(" .*","", dc_data$date)
dc_data <- dc_data %>% 
  mutate(date = as.Date(date, "%Y/%m/%d"))  %>%
  filter(date >= as.Date("2018-01-01"))

## denver 10
denver <- read_csv("denver.csv")
denver_data <-  denver %>%
  select(GEO_LON, GEO_LAT, REPORTED_DATE, OFFENSE_CATEGORY_ID) %>%
  rename(x = GEO_LON, y = GEO_LAT, date = REPORTED_DATE, crime = OFFENSE_CATEGORY_ID) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## detroit 11
detroit <- read_csv("detroit.csv")
detroit_data <-  detroit %>%
  select(longitude, latitude, incident_timestamp, offense_description) %>%
  rename(x = longitude, y = latitude, date = incident_timestamp, crime = offense_description) %>% 
  mutate(date = as.Date(date, "%Y/%m/%d"))  %>%
  filter(date >= as.Date("2018-01-01"))

## fort_worth 12
fortworth  <- read_csv("fortworth.csv")
fortworth$Location <- str_remove_all(fortworth$Location, "[()]") 
fortworth_data <- fortworth %>% 
  separate(col = Location, sep = ",", into = c("y", "x")) %>%
  select(x, y, `Reported Date`, `Nature Of Call`) %>%
  rename(x = x, y = y, date = `Reported Date`, crime = `Nature Of Call`)  %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         x = as.double(x),
         y = as.double(y)
  )  %>%
  filter(date >= as.Date("2018-01-01"))

## kc_mo 13
kc_mo_files <- c("kc_mo.csv","kc_mo18.csv", "kc_mo19.csv")
kc_mo_18_00 <- kc_mo_files %>% 
  map_dfr(read_csv, col_types = cols(`Report_No`  = col_character(), `Reported_Time`  = col_skip(), 
                                     `From_Time`  = col_skip(), `To_Time`  = col_skip(), 
                                     `Offense`  = col_skip(), `Beat`  = col_skip(), `Firearm Used Flag` = col_skip()), .id = "source") %>%
  select(Location, Reported_Date, Description) 
kc_mo_18_00 <- kc_mo_18_00 %>%
  separate(col = Location, sep = "[()]", into = c("address", "coords")) %>%
  separate(col = coords, sep = ",", into = c("y","x"))
kc_mo_data <-  kc_mo_18_00 %>%
  select(x, y, Reported_Date, Description) %>%
  rename(x = x, y = y, date =  Reported_Date, crime = Description) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         x = as.double(x),
         y = as.double(y))  %>%
  filter(date >= as.Date("2018-01-01"))

## la 14
la_files <- c("la_2020.csv","la_2018_19.csv")
la_18_00 <- la_files %>% 
  map_dfr(read_csv, .id = "source")
la_data <-  la_18_00  %>%
  select(LON, LAT, `Date Rptd`, `Crm Cd Desc`) %>%
  rename(x = LON, y = LAT, date = `Date Rptd`, crime = `Crm Cd Desc` ) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## little_rock 15
little_rock <- read_csv("little_rock.csv")
little_rock_data <-  little_rock %>%
  select(LONGITUDE, LATITUDE, INCIDENT_DATE, OFFENSE_DESCRIPTION) %>%
  rename(x = LONGITUDE, y = LATITUDE, date = INCIDENT_DATE, crime = OFFENSE_DESCRIPTION) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## minneapolis 16
minneapolis_files <- c("minneapolis.csv","minneapolis18.csv", "minneapolis19.csv")
minneapolis_18_00 <- minneapolis_files %>% 
  map_dfr(read_csv,  col_types = cols(precinct  = col_character()), .id = "source")
minneapolis_data <-  minneapolis_18_00 %>%
  select(centerLong, centerLat,reportedDate, description) %>%
  rename(x = centerLong, y = centerLat, date = reportedDate, crime = description) %>%
  mutate(date = as.Date(date, "%Y/%m/%d"))  %>%
  filter(date >= as.Date("2018-01-01"))

## montgomery 17
md_county  <- read_csv("montgomery.csv")
md_county_data <-  md_county  %>%
  select(Longitude, Latitude, `Dispatch Date / Time`, `Crime Name3`) %>%
  rename(x = Longitude, y = Latitude, date = `Dispatch Date / Time`, crime = `Crime Name3`) %>%
  filter(date != "NA") %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## nashville 18
nashville <- vroom("nashville.csv")
nashville_data <- nashville %>%
  select(Longitude, Latitude, `Incident Occurred`, `Offense Description`) %>%
  rename(x = Longitude, y = Latitude, date = `Incident Occurred`, crime = `Offense Description`) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## nola 19
nola_2020 <- read_csv("nola.csv",
                      col_types = cols(Zip = col_skip()
                      ))
nola_2020$Location <- str_remove_all(nola_2020$Location, "[POINT()]")
nola_2020 <- separate(nola_2020, col = Location, sep = " ", into = c("1", "2", "3", "4")) %>%
  select(`2`, `3`, TimeCreate, InitialTypeText) %>%
  rename(x = "2", y = "3" , date = TimeCreate, crime = InitialTypeText) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  mutate(x = as.double(x)) %>%
  mutate(y = as.double(y))  %>%
  filter(date >= as.Date("2018-01-01"))
nola_files <- c("nola18.csv", "nola19.csv")
nola_18_19 <- nola_files %>% 
  map_dfr(read_csv, col_types = cols(Zip = col_skip()), .id = "source")
nola_18_19$Location <- str_remove_all(nola_18_19$Location, "[POINT()]")
nola_18_19 <- separate(nola_18_19, col = Location, sep = " ", into = c("y", "x", "d", "a")) 
new_orleans_18_19 <- nola_18_19 %>%
  separate(col = y, sep = ",", into = c("y","x12")) %>%
  select(`x`, `y`, TimeCreate, InitialTypeText) %>%
  rename(x = x, y = y , date = TimeCreate, crime = InitialTypeText) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>%
  filter(date >= as.Date("2018-01-01")) %>%
  filter(x < 0 & y > 0 ) %>%
  mutate(x = as.double(x),
         y = as.double(y))

new_orleans_data <- bind_rows(nola_2020, new_orleans_18_19)

## nyc 20
nyc_files <- c("nyc.csv","nyc18_19.csv") 
nyc_18_00 <- nyc_files %>% 
  map_dfr(vroom, .id = "source")
nyc_data <- nyc_18_00 %>%
  select(Longitude, Latitude, CMPLNT_FR_DT, OFNS_DESC) %>%
  rename(x = Longitude, y = Latitude, date = CMPLNT_FR_DT, crime = OFNS_DESC) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## oakland 21
oakland <- read_csv("oakland.csv")
oakland_1 <- read_csv("oakland1.csv")
oakland_new <- oakland_1 %>% 
  mutate(DATETIME = as.Date(DATETIME, "%m/%d/%Y")) %>%
  filter(DATETIME > as.Date("2020-06-30")) %>%
  separate(col = Location, sep = "[()]", into = c("address", "coords")) %>% 
  separate(col = coords, sep = ",", into = c("y","x")) %>%
  select(x, y, DATETIME, CRIMETYPE) %>%
  rename(x = x, y = y, date = DATETIME, crime = CRIMETYPE) %>%
  mutate(x = as.double(x),
         y = as.double(y))
oakland_jan_jun <- oakland %>%
  select(x, y, date, crime) %>%
  mutate( date = as.Date(date, "%m/%d/%Y"))
oakland_data <- bind_rows(oakland_jan_jun, oakland_new)  %>%
  filter(date >= as.Date("2018-01-01"))

## omaha 22
omaha_files <- c("omaha.csv","omaha18.csv","omaha19.csv")
omaha_18_00 <- omaha_files %>% 
  map_dfr(read_csv, .id = "source")
omaha_data <-  omaha_18_00  %>%
  select(`Occurred Block LON`,`Occurred Block LAT`,`Reported Date`,`Statute/Ordinance Description` ) %>%
  rename(x = `Occurred Block LON`, y = `Occurred Block LAT`, date = `Reported Date`, crime =`Statute/Ordinance Description`) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## pg_county  23
pg_county  <- read_csv("pg_county.csv")
pg_county_data <-  pg_county  %>%
  select(Longitude, Latitude, Date, Clearance_code_inc_type) %>%
  rename(x = Longitude, y = Latitude, date = Date, crime = Clearance_code_inc_type) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## philly 24
philly_files <- c("philly.csv","philly18.csv","philly19.csv") 
philly_18_00 <- philly_files %>% 
  map_dfr(read_csv, .id = "source")
philly_data <-  philly_18_00 %>%
  select(point_x, point_y, dispatch_date_time, text_general_code) %>%
  rename(x = point_x, y = point_y, date = dispatch_date_time, crime = text_general_code) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

#####
## phoenix 25 -- geocoding addresses needed ignore
phoenix <- read_csv("phoenix.csv")
phoenix_data <- phoenix %>%
  select(lon, lat, `OCCURRED ON`, `UCR CRIME CATEGORY`) %>%
  rename(x = lon, y = lat, date = `OCCURRED ON`, crime = `UCR CRIME CATEGORY`) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

#####

## portland 26
portland_files <- c("portland.csv","portland2019.csv","portland2018.csv")
portland <- portland_files %>% 
  map_dfr(read_csv, col_types = cols(OpenDataLon = col_double(),
                                     OpenDataLat = col_double())
          , .id = "source")
portland_data <-  portland %>%
  select(OpenDataLon, OpenDataLat, OccurDate, OffenseType) %>%
  rename(x = OpenDataLon, y = OpenDataLat, date = OccurDate , crime = OffenseType) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"),
         x = as.double(x),
         y = as.double(y)
  )  %>%
  filter(date >= as.Date("2018-01-01"))


## seattle 27
seattle <- read_csv("seattle.csv")
seattle_data <- seattle %>%
  select(Longitude, Latitude, `Report DateTime`, Offense ) %>%
  rename(x = Longitude, y = Latitude, date = `Report DateTime`, crime = Offense) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## sf 28
sf <- read_csv("sf.csv")
sf_data <-  sf %>%
  select(Longitude, Latitude, `Incident Date`, `Incident Description`) %>%
  rename(x = Longitude, y = Latitude, date = `Incident Date`, crime = `Incident Description`)%>% 
  mutate(date = as.Date(date, "%Y/%m/%d"))  %>%
  filter(date >= as.Date("2018-01-01")) 
#####

## stlouis 29 
setwd("C:/Users/John Jr/Desktop/crime_downloads/st_louis")
setwd("~/Desktop/crime_downloads/st_louis")
st_louis_temp <- list.files(pattern = "*.CSV")
stlouis <- st_louis_temp %>%
  map_dfr(read_csv)
stlouis_data <-  stlouis %>%
  select(XCoord, YCoord, DateOccur, Description) %>%
  rename(x = XCoord, y = YCoord, date = DateOccur, crime = Description) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) 

#####
setwd("C:/Users/John Jr/Desktop/crime_downloads")
setwd("~/Desktop/crime_downloads")
tacoma  <- read_csv("tacoma.csv")
tacoma_data <- tacoma  %>%
  separate(col = intersection, sep = "[()]", into = c("address","coords")) %>%
  separate(col = coords, sep = ",", into = c("y","x")) %>%
  select(x, y, `Occurred On`, `Crime`) %>%
  rename(x = x, y = y, date = `Occurred On`, crime = `Crime`)  %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01")) %>%
  mutate(x = as.double(x),
         y = as.double(y)
  )
## memphis
memphis <- read_csv("memphis.csv")
memphis_data <- memphis  %>%
  select(coord1, coord2, offense_date, agency_crimetype_id) %>%
  rename(x = coord2, y = coord1, date = offense_date, crime = agency_crimetype_id)  %>% 
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## moco_al
montgomery_al  <- read_csv("montgomery_al.csv")
montgomery_al_data <- montgomery_al  %>%
  select(X, Y, Case_Reported_Date_And_Time, Crime_Category) %>%
  separate(col = Case_Reported_Date_And_Time, sep = " ", into = c("date", "time")) %>%
  rename(x = X, y = Y, date = date, crime = Crime_Category)%>% 
  mutate(date = as.Date(date, "%Y/%m/%d")) %>%
  select(x, y, date, crime)  %>%
  filter(date >= as.Date("2018-01-01"))

## pitts 
pitts_files <- c("pitts1.csv","pitts2.csv") 
pitts_18_00 <- pitts_files %>% 
  map_dfr(read_csv, .id = "source")
pitts_data <- pitts_18_00  %>%
  select(CCR, X, Y, INCIDENTTIME, INCIDENTHIERARCHYDESC)%>%
  separate(col = INCIDENTTIME, sep = " ", into = c("date", "time")) %>%
  rename(x = X, y = Y, date = date, crime = INCIDENTHIERARCHYDESC) %>% 
  mutate(date = as.Date(date, "%Y-%m-%d"))%>%
  distinct(CCR, .keep_all = TRUE) %>%
  select(x, y, date, crime)  %>%
  filter(date >= as.Date("2018-01-01"))

## houston 
setwd("~/Desktop/crime_downloads/grave/houston_grave")
setwd("C:/Users/John Jr/Desktop/crime_downloads/grave/houston_grave")
houston <- vroom("houston_parse_2.csv")
houston_data <- houston %>%
  select(x, y, date, crime) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))

## vegas
setwd("C:/Users/John Jr/Desktop/crime_downloads")
setwd("~/Desktop/crime_downloads")
vegas <- read_csv("vegas.csv")
vegas_data <- vegas  %>%
  select(LONG, LAT, Event_Date, Type_Description) %>%
  separate(Event_Date, sep = " ", into = c("date","time")) %>%
  rename(x = LONG, y = LAT, date = date, crime = Type_Description)  %>% 
  select(x, y, date, crime) %>%
  mutate(date = as.Date(date, "%m/%d/%Y"))  %>%
  filter(date >= as.Date("2018-01-01"))

## milwaukee
milwaukee_files <- c("milwaukee2020.csv","milwaukee18_19.csv") 
milwaukee_18_00 <- milwaukee_files %>% 
  map_dfr(read_csv, col_types = cols(IncidentNum = col_skip()), .id = "source")
milwaukee_data <- milwaukee_18_00  %>%
  gather(`Arson`, `AssaultOffense`, `Burglary`, `Homicide`, `Robbery`, `SexOffense`, `Theft`, `VehicleTheft`, key = `crime`, value = `cases`) %>%
  filter(cases > 0) %>%
  select(RoughX, RoughY, ReportedDateTime, crime) %>%
  separate(col = ReportedDateTime, sep = " ", into = c("date", "time")) %>%
  rename(x = RoughX, y = RoughY, date = date, crime = crime) %>% 
  mutate(date = as.Date(date, "%Y-%m-%d")) %>%
  select(x, y, date, crime)  %>%
  filter(date >= as.Date("2018-01-01"))

## merge datasets together into from frame
## check to ensure fields designations match -- error possible here. 

raw_merge_datebase_18_20 <- bind_rows(atlanta_data, austin_data, baltimore_data, batonrouge_data, boston_data, chicago_data, cincinnati_data, dallas_data, dc_data,
                                      denver_data, detroit_data, fortworth_data, kc_mo_data, la_data, little_rock_data, minneapolis_data, md_county_data, nashville_data, 
                                      new_orleans_data, nyc_data, oakland_data, omaha_data, pg_county_data, philly_data, portland_data, seattle_data, sf_data, tacoma_data, 
                                      memphis_data, montgomery_al_data, pitts_data, houston_data, stlouis_data, vegas_data, milwaukee_data, .id = "id") %>%
  filter(date >= as.Date("2018-01-01")) %>%
  filter(date <= as.Date("2020-12-31"))

## cleaning data requires removing chicago from rest of database
clean_attempt_1 <- raw_merge_datebase_18_20 %>%
  mutate(crime = tolower(crime)) %>%
  mutate(id = as.double(id)) %>%
  inner_join(city_key, by = "id") %>%
  #remove chicago
  filter(key != "chicago") %>%
  select(x, y, date, crime, key) %>%
  ## add the standardized crime key here
  inner_join(clean_key, by = "crime") %>%
  rename(clean_crime = .clean_crime) %>%
  select(x, y, date, crime, key, clean_crime)

## chicago_cleaning
clean_attempt_2 <- raw_merge_datebase_18_20 %>%
  mutate(crime = tolower(crime)) %>%
  mutate(type = tolower(type),
         id = as.double(id)) %>%
  inner_join(city_key, by = "id") %>%
  ##parse only chicago
  filter(key == "chicago") %>%
  mutate(chicago_key = paste(crime, type, sep = "_")) %>%
  mutate(chicago_key = tolower(chicago_key)) %>%
  inner_join(chicago_key, by = "chicago_key") %>%
  select(x, y, date, crime.x, key, clean_crime)%>%
  rename(crime = crime.x)

## export database -- crime is now standardized
clean_merge <- bind_rows(clean_attempt_1, clean_attempt_2)
## find the nine crimes for analysis
nine_major <- clean_merge %>%
  select(x, y, date, key, clean_crime) %>%
  filter(clean_crime != "other") 

##write to file
write.csv(nine_major, "nine_major_18_20.csv")

### performae Census tract gecoding in either R or QGIS 
### St_louis and milwaukee will need sep geocoding unless 
### coordinates which change in the beginning





