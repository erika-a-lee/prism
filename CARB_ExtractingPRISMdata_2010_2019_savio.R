### This code calculates the mean annual precipitation experienced between 2010-2019
### within varying buffer sizes (5,10,15km) surrounding CARB monitor sites in Cocci endemic counties 

#It downloads daily rainfall and mean/min/max temperature data using the prism package in R
# and then does a raster extract

################ IMPORT PACKAGES ##############
install.packages("prism")
library(prism) #use to download and create raster stack of PRISM environmental data
library(lubridate) #epiweek()
library(tidyverse) 
library(raster) #crs()
library(rgdal) #reads in shapefile readOGR()
library(devtools)
install_github("hunzikp/velox", force = TRUE)
library(velox) #raster extract
#####################################################################################################

### 1. Extract total weekly precipitation from PRISM data

################ DOWNLOAD PPT RASTERS ##############
# create new folder to put downloads in
options(prism.path = "C:/Users/Erika/OneDrive/Documents/R/Cocci/PRISM_ppt")
# download daily ppt data 2010-2019
get_prism_dailys(type  ="ppt", minDate=as.Date("2018-12-24"), maxDate =as.Date("2018-12-24"), keepZip = FALSE)
ls_prism_data(name=TRUE)


################ CREATE TMEAN RASTER STACK ##############
# create stack of rasters for extraction
data_ppt <- as.data.frame(prism_archive_ls()) #creates a list of all the rasters in the prism.path
index_ppt <- 1:nrow(data_ppt)
#RS_ppt <- pd_stack(data_ppt[index_ppt,])#creates a raster stack of all rasters in 'data'
RS_test_ppt <- pd_stack(prism_archive_subset( "ppt","daily", minDate = as.Date("2010-01-01"),maxDate = as.Date("2010-01-31")))


### 2. Load shapefile and extract to shape
################ EXTRACT PPT ##############

# import shapefiles for each buffer size
all_buffers <- readOGR("CARB Monitors/CARB Buffers/all_buffers.shp")

# visually check by plotting polygons 
plot(all_buffers)
View(all_buffers@data)

# project both files into longlat 
all_buffers <- spTransform(all_buffers, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

RS_test_ppt <- projectRaster(RS_test_ppt, crs = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# check that extent formats match
ext <- extent(all_buffers)
extent(RS_test_ppt)

#check that crs matches 
crs(RS_test_ppt)
crs(all_buffers)

#create the velox raster
vx_ppt <- velox(RS_test_ppt, extent=ext, crs=crs(all_buffers))

#perform the extraction using velox package (FASTER)
vals_ppt <- vx_ppt$extract(all_buffers, fun= function(x) mean(x, na.rm = TRUE), small = TRUE)
vals_ppt <- data.frame(vals_ppt)


################ ORGANIZE DATAFRAME ##############

dates_ppt <- names(RS_test_ppt)

colnames(vals_ppt) <- dates_ppt

ID <- all_buffers@data$ID #make list of IDs (site code and buffer size) from buffer data

vals_ppt <- mutate(vals_ppt, buff_ID = ID)

data_long_ppt <- vals_ppt %>% gather(date, ppt, 1:(ncol(vals_ppt)-1)) %>%
  mutate(year=factor(substr(date,24,27)),
         month=factor(substr(date,28,29)),
         day=factor(substr(date,30,31))) %>% 
  dplyr::select(-date) %>%
  mutate(date = as.Date(paste(year, month, day, sep="-"))) %>%
  mutate(epiweek = epiweek(date), epiyear = epiyear(date))

data_mean_ppt <- data_long %>% 
  group_by(buff_ID, epiyear, epiweek) %>%
  summarise(ppt_total = sum(ppt)) #taking the sum of ppt to get total ppt per week 

#####################################################################################################

### 2.Extract average mean weekly temperature from PRISM data 

################ DOWNLOAD TMEAN RASTERS ##############

# create new folder to put downloads in
options(prism.path = "C:/Users/Erika/OneDrive/Documents/R/Cocci/PRISM")
# download daily tmean
get_prism_dailys(type  ="tmean", minDate=as.Date("2010-01-01"), maxDate =as.Date("2019-12-31"), keepZip = FALSE)
# check that data is saved
ls_prism_data(name=TRUE)

################ CREATE TMEAN RASTER STACK ##############

# create stack of rasters for extraction
data_tmean <- as.data.frame(prism_archive_ls()) #creates a list of all the rasters in the prism.path
index_tmean <- 1:nrow(data_tmean)
RS_tmean <- pd_stack(data_tmean[index_tmean,])#creates a raster stack of all rasters in 'data'
RS_test_tmean <- pd_stack(prism_archive_subset( "tmean","daily", minDate = as.Date("2010-01-01"),maxDate = as.Date("2010-01-31")))


################ EXTRACT TMEAN ##############

# project raster files into longlat 

RS_test_tmean <- projectRaster(RS_test_tmean, crs = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# check that extent formats match
extent(all_buffers)
extent(RS_test_tmean)

#check that crs matches 
crs(RS_test_tmean)
crs(all_buffers)


#create the velox raster
vx_tmean <- velox(RS_test_tmean, extent=ext, crs=crs(all_buffers)) #make a velox raster

#perform the extraction using velox package (FASTER)
vals_tmean <- vx_tmean$extract(all_buffers, fun= function(x) mean(x, na.rm = TRUE), small = TRUE) #extract tmean data within the specified buffers 
vals_tmean <- data.frame(vals_tmean) #make into dataframe


################ ORGANIZE DATAFRAME ##############

dates_tmean <- names(RS_test_tmean) #make list of names that have date information

colnames(vals_tmean) <- dates_tmean #change column names to associated dates

vals_tmean <- mutate(vals_tmean, buff_ID = ID)#add column with ID (site code and buffer size)

# convert vals_tmean dataframe from a matrix to long-form 
data_long_tmean <- vals_tmean %>% 
  gather(date, tmean, 1:(ncol(vals_tmean)-1)) %>%
  mutate(year=factor(substr(date,24,27)), #add column for 'year' by extracting the 24-27 character in the column named 'date'
         month=factor(substr(date,28,29)), #add column for 'month' by extracting the 28-29 character in the column named 'date'
         day=factor(substr(date,30,31))) %>%  #add column for 'day' by extracting the 30-31 character in the column named 'date'
  dplyr::select(-date) %>% #take out 'date' column
  mutate(date = as.Date(paste(year, month, day, sep="-"))) %>% #add column with date as.Date YY-MM-DD
  mutate(epiweek = epiweek(date), epiyear = epiyear(date)) #add column with week in epiweek() format (starts on Sundays)

data_mean_tmean <- data_long %>% 
  group_by(buff_ID, epiyear, epiweek) %>% #keep each of these columns in the order specified
  summarise(tmean_avg = mean(tmean)) #taking the mean of 'tmean' to get mean temp per week 

#####################################################################################################

### 3.Extract average min weekly temperature from PRISM data 

################ DOWNLOAD TMIN RASTERS ##############

# create new folder to put downloads in
options(prism.path = "C:/Users/Erika/OneDrive/Documents/R/Cocci/PRISM")
# download daily tmin
get_prism_dailys(type  ="tmin", minDate=as.Date("2010-01-01"), maxDate =as.Date("2019-12-31"), keepZip = FALSE)
# check that data is saved
ls_prism_data(name=TRUE)

################ CREATE TMEAN RASTER STACK ##############

# create stack of rasters for extraction
data_tmin <- as.data.frame(prism_archive_ls()) #creates a list of all the rasters in the prism.path
index_tmin <- 1:nrow(data_tmin)
RS_tmin <- pd_stack(data_tmin[index_tmin,])#creates a raster stack of all rasters in 'data'
RS_test_tmin <- pd_stack(prism_archive_subset( "tmean","daily", minDate = as.Date("2010-01-01"),maxDate = as.Date("2010-01-31")))


################ EXTRACT TMEAN ##############

# project raster files into longlat 

RS_test_tmin <- projectRaster(RS_test_tmin, crs = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# check that extent formats match
extent(all_buffers)
extent(RS_test_tmin)

#check that crs matches 
crs(RS_test_tmin)
crs(all_buffers)


#create the velox raster
vx_tmin <- velox(RS_test_tmin, extent=ext, crs=crs(all_buffers)) #make a velox raster

#perform the extraction using velox package (FASTER)
vals_tmin <- vx_tmin$extract(all_buffers, fun= function(x) mean(x, na.rm = TRUE), small = TRUE) #extract tmin data within the specified buffers 
vals_tmin <- data.frame(vals_tmin) #make into dataframe


################ ORGANIZE DATAFRAME ##############

dates_tmin <- names(RS_test_tmin) #make list of names that have date information

colnames(vals_tmin) <- dates_tmin #change column names to associated dates

vals_tmin <- mutate(vals_tmin, buff_ID = ID)#add column with ID (site code and buffer size)

# convert vals_tmin dataframe from a matrix to long-form 
data_long_tmin <- vals_tmin %>% 
  gather(date, tmin, 1:(ncol(vals_tmin)-1)) %>%
  mutate(year=factor(substr(date,24,27)), #add column for 'year' by extracting the 24-27 character in the column named 'date'
         month=factor(substr(date,28,29)), #add column for 'month' by extracting the 28-29 character in the column named 'date'
         day=factor(substr(date,30,31))) %>%  #add column for 'day' by extracting the 30-31 character in the column named 'date'
  dplyr::select(-date) %>% #take out 'date' column
  mutate(date = as.Date(paste(year, month, day, sep="-"))) %>% #add column with date as.Date YY-MM-DD
  mutate(epiweek = epiweek(date), epiyear = epiyear(date)) #add column with week in epiweek() format (starts on Sundays)

data_mean_tmin <- data_long %>% 
  group_by(buff_ID, epiyear, epiweek) %>% #keep each of these columns in the order specified
  summarise(tmin_avg = mean(tmin)) #taking the mean of 'tmin' to get mean temp per week 


#####################################################################################################

### 4.Extract average max weekly temperature from PRISM data 

################ DOWNLOAD TMAX RASTERS ##############

# create new folder to put downloads in
options(prism.path = "C:/Users/Erika/OneDrive/Documents/R/Cocci/PRISM")
# download daily tmax
get_prism_dailys(type  ="tmax", minDate=as.Date("2010-01-01"), maxDate =as.Date("2019-12-31"), keepZip = FALSE)
# check that data is saved
ls_prism_data(name=TRUE)

################ CREATE TMEAN RASTER STACK ##############

# create stack of rasters for extraction
data_tmax <- as.data.frame(prism_archive_ls()) #creates a list of all the rasters in the prism.path
index_tmax <- 1:nrow(data_tmax)
RS_tmax <- pd_stack(data_tmax[index_tmax,])#creates a raster stack of all rasters in 'data'
RS_test_tmax <- pd_stack(prism_archive_subset( "tmean","daily", minDate = as.Date("2010-01-01"),maxDate = as.Date("2010-01-31")))


################ EXTRACT TMEAN ##############

# project raster files into longlat 

RS_test_tmax <- projectRaster(RS_test_tmax, crs = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# check that extent formats match
extent(all_buffers)
extent(RS_test_tmax)

#check that crs matches 
crs(RS_test_tmax)
crs(all_buffers)


#create the velox raster
vx_tmax <- velox(RS_test_tmax, extent=ext, crs=crs(all_buffers)) #make a velox raster

#perform the extraction using velox package (FASTER)
vals_tmax <- vx_tmax$extract(all_buffers, fun= function(x) mean(x, na.rm = TRUE), small = TRUE) #extract tmax data within the specified buffers 
vals_tmax <- data.frame(vals_tmax) #make into dataframe


################ ORGANIZE DATAFRAME ##############

dates_tmean <- names(RS_test_tmean) #make list of names that have date information

colnames(vals_tmean) <- dates_tmean #change column names to associated dates

vals_tmean <- mutate(vals_tmean, buff_ID = ID)#add column with ID (site code and buffer size)

# convert vals_tmean dataframe from a matrix to long-form 
data_long_tmax <- vals_tmax %>% 
  gather(date, tmax, 1:(ncol(vals_tmax)-1)) %>%
  mutate(year=factor(substr(date,24,27)), #add column for 'year' by extracting the 24-27 character in the column named 'date'
         month=factor(substr(date,28,29)), #add column for 'month' by extracting the 28-29 character in the column named 'date'
         day=factor(substr(date,30,31))) %>%  #add column for 'day' by extracting the 30-31 character in the column named 'date'
  dplyr::select(-date) %>% #take out 'date' column
  mutate(date = as.Date(paste(year, month, day, sep="-"))) %>% #add column with date as.Date YY-MM-DD
  mutate(epiweek = epiweek(date), epiyear = epiyear(date)) #add column with week in epiweek() format (starts on Sundays)

data_mean_tmax <- data_long %>% 
  group_by(buff_ID, epiyear, epiweek) %>% #keep each of these columns in the order specified
  summarise(tmax_avg = mean(tmax)) #taking the mean of 'tmax' to get mean temp per week 
