

############################ Installing Packages ###############################
library(imputeTS)
library(rnoaa)
library(dplyr)
library(readr)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(readxl)
library(forecast)
################################################################################
#
#
#
#
############################## Preparing Data ##################################

washingtondc <- read_csv("washingtondc.csv")
washingtondc <- dplyr::filter(washingtondc, year > 1979)
dc <- select(washingtondc, year, Jan_Temp, Feb_Temp)


kyoto <- read_csv("kyoto.csv")
kyoto <- dplyr::filter(kyoto, year > 1979)
k_j <- dplyr::select(kyoto, year, Jan_Temp, Feb_Temp)

liestal <- read_csv("liestal.csv")
liestal <- dplyr::filter(liestal, year > 1979)
l_n <- dplyr::select(liestal, year, Jan_Temp, Feb_Temp)

gdd <- read_csv("gdd_model_dataset.csv")
gdd <- dplyr::filter(gdd, location == "liestal", year >1979)

big_dataset <- inner_join(l_n, gdd, by = "year")
big_dataset <- big_dataset[-c(4)]

cherry <- read_csv("JINALLDATA.csv")
cherry_dc <- dplyr::filter(cherry, location == "washingtondc")
cherry_kyoto <- dplyr::filter(cherry, location == "kyoto")
cherry_liestal <- dplyr::filter(cherry, location == "liestal")


############################## Kyoto Temp Data #################################

station_data <- ghcnd_stations()
lat_lon_df <- data.frame(id = "kyoto_japan",
                         latitude = 35.01198,
                         longitude = 135.6761)
nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                          station_data = station_data, radius = 10)
kyoto_weather <- data.frame(id = "kyoto_japan", latitude = 35.01198, longitude = 135.6761)

# Get Stations within 50 kilometers
meteo_nearby_stations(lat_lon_df = kyoto_weather, station_data = station_data,
                      radius = 50, var = c("PRCP", "TMAX"),
                      year_min = 1951, year_max = 2022)

kyotot_tavg <- data.frame(ghcnd_search(stationid = "JA000047759", var = "tavg",  
               date_min = "1999-02-01", date_max = "1999-02-28"))
kyotot_tavg <- data.frame(na_interpolation(kyotot_tavg))
mean(kyotot_tavg$tavg.tavg)

kyoto_temp_data <- read_csv("kyoto_temp.csv")
kyoto_temp_data <- na_interpolation(kyoto_temp_data)

################################################################################
                                                                               #
kyoto <- cbind(kyoto, kyoto_temp_data)                                         #
kyoto <- kyoto[-c(8)]                                                          #
                                                                               #
################################################################################
#
#
#
################################ DC Temp Data ##################################

lat_lon_df <- data.frame(id = "dc_usa",
                         latitude = 38.8841,
                         longitude = -77.03863)

nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                          station_data = station_data, radius = 10)

dc_weather <- data.frame(id = "dc_usa", latitude = 38.8841, longitude = -77.03863)

# Get Stations within 50 kilometers
meteo_nearby_stations(lat_lon_df = dc_weather, station_data = station_data,
                      radius = 50, var = c("TAVG", "TMAX"),
                      year_min = 1951, year_max = 2022)

ghcnd_search(stationid = "USC00186350", var = "tavg",  date_min = "1960-01-01", 
             date_max = "2022-01-31")

dc_temp_data <- read_csv("dc_temp.csv")

################################################################################
                                                                               #
washingtondc <- cbind(washingtondc, dc_temp_data)                              #
washingtondc <- washingtondc[-c(8)]                                            #
                                                                               #
################################################################################
#
#
#
############################# Liestal Temp Data ################################

lat_lon_df <- data.frame(id = "liestal_switzerland",
                         latitude = 47.4814,
                         longitude = 7.730519)

nearby_stations <-  meteo_nearby_stations(lat_lon_df = lat_lon_df,
                                          station_data = station_data, radius = 10)

liestal_weather <- data.frame(id = "liestal_switzerland", latitude = 47.4814, 
                         longitude = 7.730519)

# Get Stations within 50 kilometers
meteo_nearby_stations(lat_lon_df = liestal_weather, station_data = station_data,
                      radius = 50, var = c("PRCP", "TMAX"),
                      year_min = 1951, year_max = 2022)
liestal_tavg <- data.frame(ghcnd_search(stationid = "FRM00007299", var = "tavg",
                 date_min = "1999-2-01" , date_max = "1999-02-28"))
liestal_tavg <- data.frame(na_interpolation(liestal_tavg))
mean(liestal_tavg$tavg.tavg)

liestal_temp_data <- read_csv("liestal_temp.csv")

################################################################################
                                                                               #
liestal <- cbind(liestal, liestal_temp_data)                                   #
liestal <- liestal[-c(8)]                                                      #
                                                                               #
############ Concatenate the 3 data sets into 1 set called "cherry" ############
                                                                               #
cherry <- rbind(kyoto,washingtondc,liestal)                                    # 
                                                                               #
################################################################################
#
#
#
#
############################# Exploring the Data ###############################

# Feb_Temp vs bloom_doy with regression line

## Washington, DC
par(mfrow = c(1,2)) 
plot(washingtondc$Jan_Temp, washingtondc$bloom_doy, 
     main= "Jan_Temp vs. bloom_doy",
     xlab = "Average January Temperature (Celsius)", ylab = "Peak Bloom Day of the Year",
     sub = "Figure 1: Washington, DC", pch = 19, frame = FALSE)
abline(lm(bloom_doy ~ Jan_Temp, data = washingtondc), col = "blue")

plot(washingtondc$Feb_Temp, washingtondc$bloom_doy, 
     main= "Feb_Temp vs. bloom_doy", sub = "Figure 2: Washington, DC",
     xlab = "Average February Temperature (Celsius)", ylab = "Peak Bloom Day of the Year",
     pch = 19, frame = FALSE)
abline(lm(bloom_doy ~ Feb_Temp, data = washingtondc), col = "blue")

## Liestal
par(mfrow = c(1,2)) 
plot(liestal$Jan_Temp, liestal$bloom_doy, 
     main= "Jan_Temp vs. bloom_doy", sub = "Figure 5: Liestel, Switzerland",
     xlab = "Average January Temperature (Celsius)", ylab = "Peak Bloom Day of the Year",
     pch = 19, frame = FALSE)
abline(lm(bloom_doy ~ Jan_Temp, data = liestal), col = "orange")

plot(liestal$Feb_Temp, liestal$bloom_doy, 
     main= "Feb_Temp vs. bloom_doy", sub = "Figure 6: Liestal, Switzerland",
     xlab = "Average February Temperature (Celsius)", ylab = "Peak Bloom Day of the Year",
     pch = 19, frame = FALSE)
abline(lm(bloom_doy ~ Feb_Temp, data = liestal), col = "orange")

## Kyoto
par(mfrow = c(1,2)) 
plot(kyoto$Jan_Temp, kyoto$bloom_doy, 
     main= "Jan_Temp vs. bloom_doy", sub = "Figure 3: Kyoto, Japan",
     xlab = "Average January Temperature (Celsius)", ylab = "Peak Bloom Day of the Year",
     pch = 19, frame = FALSE)
abline(lm(bloom_doy ~ Jan_Temp, data = kyoto), col = "red")

plot(kyoto$Feb_Temp, kyoto$bloom_doy, 
     main= "Feb_Temp vs. bloom_doy", sub = "Figure 4: Kyoto, Japan",
     xlab = "Average February Temperature (Celsius)", ylab = "Peak Bloom Day of the Year",
     pch = 19, frame = FALSE)
abline(lm(bloom_doy ~ Feb_Temp, data = kyoto), col = "red")


# Frequencies by type of location
freq(cherry)

# Q-Q plot
qqnorm(cherry$bloom_doy, pch = 1, plot.it = TRUE, frame = FALSE)
qqline(cherry$bloom_doy, col = "red", lwd = 2)

# Box plots
ggplot(cherry, aes(location, bloom_doy)) + geom_boxplot()

################################################################################
################## Simple Linear Regression (Testing Covariates)################
l_r <- lm(bloom_doy ~ agdd, data=cherry)
summary(l_r)

kyotowind = lm(bloom_doy ~ aprilwind, data = cherry_kyoto)
summary(kyotowind)

liestalprec = lm(bloom_doy ~ febpre + marpre + aprpre, data = cherry_liestal)
summary(liestalprec)

######################### Multiple Linear Regression ###########################
m_r <- lm(bloom_doy ~ Jan_Temp + Feb_Temp, data=washingtondc)
summary(m_l_r) 


m_l_r1 <- lm(bloom_doy ~ agdd + Jan_Temp + Feb_Temp, data=cherry_dc)
summary(m_l_r1)

m_l_r2 <- lm(bloom_doy ~ agdd + Jan_Temp + Feb_Temp, data=cherry_kyoto)
summary(m_l_r2)

m_l_r3 <- lm(bloom_doy ~ agdd + Jan_Temp + Feb_Temp, data=cherry_liestal)
summary(m_l_r3)

model = lm(bloom_doy ~ Jan_Temp + Feb_Temp, data=cherry)
summary(model) 

# Show results of the model 
                    
################################################################################
#
#
#
#
############################### Time Series Plots ##############################

washingtondc_ts <- dplyr::select(washingtondc, Feb_Temp)
washingtondc_ts <- ts(washingtondc_ts, start=1921, end=2021, frequency=1) # Convert into ts format
autoplot(washingtondc_ts, color = "dodgerblue4") +
  ggtitle("Peak Bloom Date for Cherry Trees in Washington, D.C., 1921-2021") + 
  ylab("Bloom Date (doy)") + xlab("Year")
  theme_classic()
dc_ts <- dplyr::select(washingtondc_ts, )

kyoto_ts <- dplyr::select(kyoto, Feb_Temp)
  kyoto_ts <- ts(kyoto_ts, start=812, end=2021, frequency=1) # Convert into ts format
  autoplot(kyoto_ts, color = "red") +
    ggtitle("Peak Bloom Date for Cherry Trees in Kyoto, 812-2021") + 
    ylab("Bloom Date (doy)") + xlab("Year")
  theme_classic()
  
liestal_ts <- dplyr::select(liestal, Jan_Temp)
liestal_ts <- ts(liestal_ts, start=1894, end=2021, frequency=1) # Convert into ts format
  autoplot(dc_ts, color = "green") +
    ggtitle("Peak Bloom Date for Cherry Trees in Liestal, 1894-2021") + 
    ylab("Bloom Date (doy)") + xlab("Year")
  theme_classic()
