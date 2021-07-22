### This code calculates the mean annual precipitation experienced between 1981-2018
### according to county grouping that are split

#It downloads annual rainfall data using the prism package in R
# and then does a raster extract



### 1. Download prism data
install.packages("prism")
library(prism)
#create new folder to put downloads in
options(prism.path = "C:/Users/Erika/OneDrive/Documents/R/Cocci/PRISM_ppt")
#download daily ppt data 2010-2019
get_prism_dailys(type  ="ppt", minDate=as.Date("2018-12-24"), maxDate =as.Date("2018-12-24"), keepZip = FALSE)
ls_prism_data(name=TRUE)

#create new folder to put downloads in
options(prism.path = "C:/Users/Erika/OneDrive/Documents/R/Cocci/PRISM")
#download daily tmean
get_prism_dailys(type  ="tmean", minDate=as.Date("2010-01-01"), maxDate =as.Date("2019-12-31"), keepZip = FALSE)
#check that data is saved
ls_prism_data(name=TRUE)

# Create stack of rasters for extraction
data <- as.data.frame(prism_archive_ls())
index <- 1:nrow(data)
RS <- pd_stack(data[index,])
RS_test <- pd_stack(prism_archive_subset( "ppt","daily", minDate = as.Date("2010-01-01"),maxDate = as.Date("2010-01-31")))


### 2. Load shapefile and extract to shape
################ EXTRACT PPT ##############

#load and plot shapefile 
#install.packages("devtools")
#install.packages("Rtools")

library(raster)
library(sp)
library(rgeos)
library(sf)
library(rgdal)
# install.packages("velox")
library(devtools)
install_github("hunzikp/velox", force = TRUE)
library(velox)
library(lubridate)

#COMBINE INTO 1 dataframe - 42 shapes and check names for IDs
buff_5 <- readOGR("CARB Monitors/CARB Buffers/5km/CARB_buffer_5.shp")
buff_10 <- readOGR("CARB Monitors/CARB Buffers/10km/CARB_buffer_10.shp")
buff_15 <- readOGR("CARB Monitors/CARB Buffers/15km/CARB_buffer_15.shp")
plot(buff_5)
plot(buff_10)
plot(buff_15)

all_buffers <- rbind(buff_5, buff_10,buff_15)
View(all_buffers@data)
plot(all_buffers)

# project both files and make sure crs match


all_buffers <- spTransform(all_buffers, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
all_buffers@data
ext <- extent(all_buffers)


#set CRS for SPDF
proj4string(RS) <- crs(all_buffers)
crs(RS)
crs(all_buffers)


#create the velox raster
vx <- velox(RS_test, extent=ext, crs=crs(all_buffers))

#perform the extraction using velox package (FASTER)
vals <- vx$extract(all_buffers, fun= function(x) mean(x, na.rm = TRUE), small = TRUE)
vals <- data.frame(vals)

dates <- names(RS_test)

colnames(vals) <- dates

ID <- buff_5@data$ID

vals <- mutate(vals, buff_ID = ID)

data_long <- vals %>% gather(date, ppt, 1:(ncol(vals)-1)) %>%
                      mutate(year=factor(substr(date,24,27)),
                      month=factor(substr(date,28,29)),
                      day=factor(substr(date,30,31))) %>% 
                      dplyr::select(-date) %>%
                      mutate(date = as.Date(paste(year, month, day, sep="-"))) %>%
                      mutate(epiweek = epiweek(date), epiyear = epiyear(date))

data_mean_ppt <- data_long %>% 
  group_by(buff_ID, epiyear, epiweek) %>%
  summarise(ppt_total = sum(ppt)) #taking the sum of ppt to get total ppt per week (for temp we will want mean - avg temp of each week, and Tmin and Tmax)


#OR perferm extraction using raster package (SLOWER)
#buff5_ppt_mean <- data.frame(buff5_sp$ID, raster::extract(RS_test, buff5_sp, fun=mean, na.rm=TRUE, df=TRUE))

#data_long_raster <- buff5_ppt_mean %>% gather(date, ppt, 1:ncol(buff5_ppt_mean), -buff5_sp.ID)

