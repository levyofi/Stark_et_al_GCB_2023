library(raster)

#get the location of the station
lat = 31.3485
lon = 35.3845
site <- SpatialPoints(matrix(c(lon,lat), ncol=2), proj4string = CRS("+proj=longlat +datum=WGS84"))

# get historical data
# load the raster
historical_file_bio1 = raster("/home/ofir/Downloads/wc2.1_30s_bio_1.tif")
historical_file_bio10 = raster("/home/ofir/Downloads/wc2.1_30s_bio_10.tif")
historical_file_bio11 = raster("/home/ofir/Downloads/wc2.1_30s_bio_11.tif")
hist_bio1 = extract(historical_file_bio1, y = site, buffer=NULL)
hist_bio10 = extract(historical_file_bio10, y = site, buffer=NULL)
hist_bio11 = extract(historical_file_bio11, y = site, buffer=NULL)

future_files = dir(path="/home/ofir/Downloads/", pattern="wc2.+2100\\.tif", full.names =T)

climate_change = data.frame(file = character(0), bio1_annual_mean = numeric(0), bio10_warmest_quarter_mean = numeric(0), bio11_coldest_quarter_mean = numeric(0))

for (file in future_files){
  rs = stack(file)
  future_bio1 = extract(rs$wc2_1, y = site, buffer=NULL)
  future_bio10 = extract(rs$wc2_10, y = site, buffer=NULL)
  future_bio11 = extract(rs$wc2_11, y = site, buffer=NULL)
  climate_change = rbind(climate_change, data.frame(file = file,
                                                    bio1_annual_mean =  future_bio1 - hist_bio1,
                                                    bio10_warmest_quarter_mean = future_bio10 - hist_bio10,
                                                    bio11_coldest_quarter_mean = future_bio11 - hist_bio11))
  print(paste(file, future_bio1, future_bio10, future_bio11))
}

#remove NaNs
climate_change = na.omit(climate_change)
#remove unreasonable results 
climate_change = climate_change[-2,]
mean_summer_change = mean(climate_change$bio10_warmest_quarter_mean) # [1] 6.477
mean_winter_change = mean(climate_change$bio11_coldest_quarter_mean) # [1] 4.916

#get models names
models = stringr::str_remove_all(climate_change$file, "/home/ofir/Downloads//wc2.1_30s_bioc_")
models = stringr::str_remove_all(models, "_ssp585_2081-2100.tif")
models
