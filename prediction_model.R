# GMU Cherry Blossom Prediction Competition
# Justin Kim
# Isabel Melendez
# Nima Valibeigi

# Load required libraries
library(ggplot2)
library(tidyverse)
library(rnoaa)
library(imputeTS)
library(forecast)
library(tseries)
library(MTS)
library(vars)
library(reshape2)

# Create base dataset to work with
cherry <- read.csv("data/washingtondc.csv") %>% 
  bind_rows(read.csv("data/liestal.csv")) %>% 
  bind_rows(read.csv("data/kyoto.csv")) %>% 
  filter(year >= 1950)

# Converting base dataset to tibble data frame
cherry <- as_tibble(cherry)

# Create function to create Growing Degree Days and aggregate GDD to create agdd column
get_gdd <- function (stationid, date_min, date_max) {
  tmax.ds <-  ghcnd_search(stationid = stationid, var = c("tmax"), 
               date_min = date_min, date_max = date_max)[[1]][2:3]
  tmin.ds <-  ghcnd_search(stationid = stationid, var = c("tmin"), 
               date_min = date_min, date_max = date_max)[[1]][2:3]
  tmaxntmin <- inner_join(tmax.ds, tmin.ds, by = "date")
  gdd.base <- tmaxntmin %>% mutate_at(c("tmin","tmax"), function(x) {ifelse(x < 0, 0, x )}) 
  gdd.base.imputed <- na_interpolation(gdd.base)           
  gdd.final <- gdd.base.imputed %>% mutate(gdd = (tmax + tmin)/2 - 0)
 return(sum(gdd.final$gdd))
}

# Create function to run for each site based on their station id and bloom_date
add_agdd <- function(x, output){
  if (x[1] == "washingtondc"){
    loc = "USC00186350"
  } else if (x[1] == "liestal"){
    loc = "GME00127786"
  } else if (x[1] == "kyoto"){
    loc = "JA000047759"
  } else if (x[1] == "vancouver"){
    loc = "CA001108395"
  } else {
    loc = ""
  }
  year_first = paste0(x[5], "-01-01")

  a <-  get_gdd(loc, year_first, x[6])
  return(a)
}


# Use created function to fetch tmin and tmax data and create agdd column to cherry dataset
single <- apply(cherry, 1, add_agdd)
cherry.agdd <- cbind(cherry, agdd = single)

# Remove rows where there was no data available from cherry dataset
cherry.agdd <- cherry.agdd %>% filter(agdd != 0.00)

# Save cherry dataset with agdd 
write.csv(cherry.agdd, "gdd_model_dataset.csv")

# Create dataset per site for modeling
cherry.dc <- cherry.agdd %>% filter(location == "washingtondc")
cherry.liestal <- cherry.agdd %>% filter(location == "liestal")
cherry.kyoto <- cherry.agdd %>% filter(location == "kyoto")

# To view data distribution for each site between bloom day of year and agdd 
ggplot(cherry.dc, aes(x=bloom_doy, y=agdd)) + geom_point()
ggplot(cherry.liestal, aes(x=bloom_doy, y=agdd)) + geom_point()
ggplot(cherry.kyoto, aes(x=bloom_doy, y=agdd)) + geom_point()

# Importing average temperature dataset
avg.temp.dc <- read.csv("dc_temp.csv") %>% rename(year = Year)
avg.temp.kyoto <- read.csv("kyoto_temp.csv") %>% rename(year = Year)
avg.temp.liestal <- read.csv("liestal_temp.csv") %>% rename(year = Year)

# Combining avg temp and agdd to one dataset for each site
agdd.avg.temp.dc <- inner_join(cherry.dc, avg.temp.dc, by = "year")
agdd.avg.temp.kyoto <- inner_join(cherry.kyoto, avg.temp.kyoto, by = "year")
agdd.avg.temp.liestal <- inner_join(cherry.liestal, avg.temp.liestal, by = "year")

# base.dataset <- agdd.avg.temp.dc %>% bind_rows(agdd.avg.temp.kyoto) %>% bind_rows(agdd.avg.temp.liestal)
# write.csv(base.dataset, "dataset_with_all_vars.csv")

# Linear regression using the combined dataset for each site
dc.lm <- lm(bloom_doy ~ agdd + Jan_Temp + Feb_Temp, data=agdd.avg.temp.dc)
kyoto.lm <- lm(bloom_doy ~ agdd + Jan_Temp + Feb_Temp, data=agdd.avg.temp.kyoto)
liestal.lm <- lm(bloom_doy ~ agdd + Jan_Temp + Feb_Temp, data=agdd.avg.temp.liestal)

summary(lm(bloom_doy ~ agdd + Jan_Temp + Feb_Temp, data=agdd.avg.temp.dc))
summary(lm(bloom_doy ~ agdd + Jan_Temp + Feb_Temp, data=agdd.avg.temp.kyoto))
summary(lm(bloom_doy ~ agdd + Jan_Temp + Feb_Temp, data=agdd.avg.temp.liestal))

# To forecast for agdd, Jan_Temp, and Feb_Temp, create dataset for those 3 vars 
ts.agdd.avg.temp.dc <- agdd.avg.temp.dc %>%  select(agdd, Jan_Temp, Feb_Temp)
ts.agdd.avg.temp.kyoto <- agdd.avg.temp.kyoto %>% select(agdd, Jan_Temp, Feb_Temp)
ts.agdd.avg.temp.liestal <- agdd.avg.temp.liestal %>% select(agdd, Jan_Temp, Feb_Temp)

# Change above dataset to time series format
ts.agdd.avg.temp.dc <- ts(ts.agdd.avg.temp.dc, frequency = 1)
ts.agdd.avg.temp.kyoto <- ts(ts.agdd.avg.temp.kyoto, frequency = 1)
ts.agdd.avg.temp.liestal <- ts(ts.agdd.avg.temp.liestal, frequency = 1)

# Test stationary for each dataset
apply(ts.agdd.avg.temp.dc, 2, adf.test)
apply(ts.agdd.avg.temp.kyoto, 2, adf.test)
apply(ts.agdd.avg.temp.liestal, 2, adf.test)

# Apply differencing to make data stationary 
ts.agdd.avg.temp.kyoto.diff <- diffM(ts.agdd.avg.temp.kyoto)
ts.agdd.avg.temp.liestal.diff <- diffM(ts.agdd.avg.temp.liestal)

# Test stationary for each dataset 
apply(ts.agdd.avg.temp.dc, 2, adf.test)
apply(ts.agdd.avg.temp.kyoto.diff, 2, adf.test)
apply(ts.agdd.avg.temp.liestal.diff, 2, adf.test)

# Create vector autoregression model for multivariate time series
VARselect(ts.agdd.avg.temp.dc, type = "none")
VARselect(ts.agdd.avg.temp.kyoto.diff, type = "none" )
VARselect(ts.agdd.avg.temp.liestal.diff, type = "none")

dc.var <- vars::VAR(ts.agdd.avg.temp.dc, type = "none")
kyoto.var <- vars::VAR(ts.agdd.avg.temp.kyoto.diff, type = "none" )
liestal.var <- vars::VAR(ts.agdd.avg.temp.liestal.diff, type = "none")

summary(dc.var)
summary(kyoto.var)
summary(liestal.var)

# Forecast next 10 years based on VAR model
dc.predict <- predict(dc.var)
kyoto.predict <- predict(kyoto.var)
listal.preict <- predict(liestal.var)

# Extract forecasted values 
dc.predict.agdd <- dc.predict$fcst[1]$agdd[,1]
dc.predict.jt <- dc.predict$fcst[2]$Jan_Temp[,1]
dc.predict.ft <- dc.predict$fcst[3]$Feb_Temp[,1]

kyoto.predict.agdd <- kyoto.predict$fcst[1]$agdd[,1]
kyoto.predict.jt <- kyoto.predict$fcst[2]$Jan_Temp[,1]
kyoto.predict.ft <- kyoto.predict$fcst[3]$Feb_Temp[,1]

listal.predict.agdd <- listal.preict$fcst[1]$agdd[,1]
listal.predict.jt <- listal.preict$fcst[2]$Jan_Temp[,1]
listal.predict.ft <- listal.preict$fcst[3]$Feb_Temp[,1]

# Invert differencing for kyoto and liestal datasets 
kyoto.predict.agdd <- cumsum(kyoto.predict.agdd) + 6438.50
kyoto.predict.jt <- cumsum(kyoto.predict.jt) + 44.65
kyoto.predict.ft <- cumsum(kyoto.predict.ft) + 75.17

listal.predict.agdd <- cumsum(listal.predict.agdd) + 4417.0
listal.predict.jt <- cumsum(listal.predict.jt) + 19.98
listal.predict.ft <- cumsum(listal.predict.ft) + 54.29

# Create dataset with forecasted values for prediction
dc.predict.next.10 <- data.frame(agdd = dc.predict.agdd, Jan_Temp = dc.predict.jt, Feb_Temp = dc.predict.ft )
kyoto.predict.next.10 <- data.frame(agdd = kyoto.predict.agdd, Jan_Temp = kyoto.predict.jt, Feb_Temp = kyoto.predict.ft)
liestal.predict.next.10 <- data.frame(agdd = listal.predict.agdd, Jan_Temp = listal.predict.jt, Feb_Temp = listal.predict.ft)

# Predict bloom dates for each site in days from the 1st of year for next 10 years 
dc.predict.bloom_doy <- predict(dc.lm, dc.predict.next.10)
kyoto.predict.bloom_doy <- predict(kyoto.lm, kyoto.predict.next.10)
liestal.predict.bloom_doy <- predict(liestal.lm, liestal.predict.next.10)

# Vancouver dataset with estimated bloom dates from VCBF website
vancouver <- read.csv("data/vancouver.csv")
vancouver.dc <- inner_join(cherry.dc, vancouver, by = "year")
t.test(vancouver.dc$bloom_doy.x, vancouver.dc$bloom_doy.y)

vancouver.kyoto <- inner_join(cherry.kyoto, vancouver, by = "year")
t.test(vancouver.kyoto$bloom_doy.x, vancouver$bloom_doy.y)

vancouver.liestal <- inner_join(cherry.liestal, vancouver, by = "year")
t.test(vancouver.liestal$bloom_doy.x, vancouver.liestal$bloom_doy.y)

# Compare agdd between washingtondc and vancouver 
vancouver.dc.agdd <- inner_join(vancouver.agdd, cherry.dc, by = "year")
t.test(vancouver.dc.agdd$agdd.x, vancouver.dc.agdd$agdd.y)

# Compare January and February average temps between washingtondc and vancouver 
vancouver.avg.temps <- ghcnd_search(stationid = "CA001108395", var = "tavg",  date_min = "1980-01-01", date_max = "2022-01-31")
vancouver.avg.temps <- na_interpolation(vancouver.avg.temps$tavg) %>% dplyr::select(tavg, date)
vancouver.avg.temps$month <- months(vancouver.avg.temps$date)
vancouver.avg.temps$year <- format(vancouver.avg.temps$date, format="%Y")
vancouver.avg.temps <- aggregate(tavg ~ month+year, vancouver.avg.temps, mean)
vancouver.avg.temps.jan <- vancouver.avg.temps %>% dplyr::filter(month == "January") %>% dplyr::select(year, tavg)
vancouver.avg.temps.feb <- vancouver.avg.temps %>% dplyr::filter(month == "February") %>% dplyr::select(year, tavg)
vancouver.avg.temps.jan <- vancouver.avg.temps.jan %>% transform(year = as.integer(year))
vancouver.avg.temps.feb <- vancouver.avg.temps.feb %>% transform(year = as.integer(year))

dc.temps.only <- agdd.avg.temp.dc %>% dplyr::select(location, year, Jan_Temp, Feb_Temp)
dc.vancouver.avg.temps <- inner_join(dc.temps.only, vancouver.avg.temps.jan, by = "year" ) %>% rename(vancouver_Jan_Temp = tavg)
dc.vancouver.avg.temps <- inner_join(dc.vancouver.avg.temps, vancouver.avg.temps.feb, by = "year" ) %>% rename(vancouver_Feb_Temp = tavg)

# Test average temperature for January and February for dc and vancouver
t.test(dc.vancouver.avg.temps$Jan_Temp, dc.vancouver.avg.temps$vancouver_Jan_Temp)
t.test(dc.vancouver.avg.temps$Feb_Temp, dc.vancouver.avg.temps$vancouver_Feb_Temp)

# For month of January, 41.41452 - 36.43500 = 4.97952

# Adding .5 celsius to dc's model to predict for vancouver 
agdd.avg.temp.dc.vancouver <- agdd.avg.temp.dc %>% mutate(Jan_Temp_Vancouver = Jan_Temp + 5) 

vancouver.lm <- lm(bloom_doy ~ agdd + Jan_Temp_Vancouver + Feb_Temp, data=agdd.avg.temp.dc.vancouver)
ts.agdd.avg.temp.vancouver <- agdd.avg.temp.dc.vancouver %>% dplyr::select(agdd, Jan_Temp_Vancouver, Feb_Temp)
ts.agdd.avg.temp.vancouver <- ts(ts.agdd.avg.temp.vancouver, frequency = 1)
apply(ts.agdd.avg.temp.vancouver, 2, adf.test)
vancouver.var <- vars::VAR(ts.agdd.avg.temp.vancouver, type = "none")
vancouver.predict <- predict(vancouver.var)
vancouver.predict.agdd <- vancouver.predict$fcst[1]$agdd[,1]
vancouver.predict.jt <- vancouver.predict$fcst[2]$Jan_Temp[,1]
vancouver.predict.ft <- vancouver.predict$fcst[3]$Feb_Temp[,1]
vancouver.predict.next.10 <- data.frame(agdd = vancouver.predict.agdd, Jan_Temp_Vancouver = vancouver.predict.jt, Feb_Temp = vancouver.predict.ft )
vancouver.predict.bloom_doy <- predict(vancouver.lm, vancouver.predict.next.10)
