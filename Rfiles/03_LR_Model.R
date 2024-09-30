library(httr)
library(jsonlite)
library(tidyverse) 
library(ggplot2)   
library(folium)    
library(leaflet)   
library(rgdal)     
library(sp)        
library(rgeos)     
library(cluster)   


file_path <- paste0("C:\\Users\\Akhil_kamuni\\OneDrive\\Desktop\\MP_2\\MP_2\\datasets\\combined_eq_california_timeseries.csv")

df_eq <- read.csv(file_path)
df_eq$time <- as.Date(df_eq$time)  # Assuming "time" is in date format
df_eq_ts <- ts(df_eq, start = 1, frequency = 1)  # Assumi
head(df_eq_ts, 3)

str(df_eq)

options(repr.plot.width=7, repr.plot.height=4)

hist(log(df_eq$mag), main = "Histogram of EQ Magnitude", xlab = "EQ Magnitude", ylab = "Frequency", col = "lightblue")

options(repr.plot.width=7, repr.plot.height=4)

hist(log(df_eq$depth), main = "Histogram of EQ Depth", xlab = "EQ Depth", ylab = "Frequency", col = "lightblue")

options(repr.plot.width=7, repr.plot.height=4)

hist(log(df_eq$latitude), main = "Histogram of EQ Latitude", xlab = "EQ Latitude", ylab = "Frequency", col = "lightblue")

df_eq_large <- subset(df_eq, mag > 6)

df_eq_large$time <- as.POSIXct(df_eq_large$time)

df_eq_large$time_diff_day <- c(0, diff(df_eq_large$time))

head(df_eq_large, 5)


str(df_eq_large)

num_rows <- nrow(df_eq_large)

print(num_rows)

summary(df_eq_large$time_diff_day)

quantile(df_eq_large$time_diff_day, probs = c(0.25, 0.5, 0.75))

options(repr.plot.width=7, repr.plot.height=4)

df_eq_large$time_diff_sec <- as.numeric(df_eq_large$time_diff_day, units = "secs")

hist(df_eq_large$time_diff_sec, main = "Histogram of EQ Time Difference (Seconds)", xlab = "EQ Time Difference (Seconds)", ylab = "Frequency", col = "lightblue")

options(repr.plot.width=9, repr.plot.height=5)

plot(df_eq_large$mag, type = "o", col = "red", pch = 16, xlab = "Index", ylab = "Magnitude", main = "Magnitude")

options(repr.plot.width=7, repr.plot.height=4)

plot(df_eq_large$time_diff_sec, df_eq_large$mag, pch = 16, col = "blue", xlab = "EQ Time Difference (Seconds)", ylab = "EQ Magnitude")

options(repr.plot.width=10, repr.plot.height=7)

plot(df_eq_large$longitude, df_eq_large$latitude, pch = 16, col = "blue", cex = df_eq_large$mag/5, 
     xlab = "EQ Longitude", ylab = "EQ Latitude", main = "Earthquake Locations (Size based on Magnitude)")

legend("topleft", legend = "Magnitude", pch = 16, col = "blue", pt.cex = df_eq_large$mag/5)

par(mar = c(5, 5, 4, 2) + 0.1)

title(main = "Earthquake Locations (Size based on Magnitude)", xlab = "EQ Longitude", ylab = "EQ Latitude")

date_list <- as.Date(list(df_eq_large$time))

typeof(date_list[[1]])

library(leaflet)
library(htmltools)

m <- leaflet() %>%
  setView(lng = -120, lat = 38, zoom = 5) %>%
  addTiles()  # Add default OpenStreetMap tiles

markerCluster <- leaflet::markerClusterOptions()

for (i in 1:nrow(df_eq_large)) {
  m <- addCircleMarkers(map = m,
                        lng = df_eq_large[i, "longitude"],
                        lat = df_eq_large[i, "latitude"],
                        radius = df_eq_large[i, "mag"] * 0.75,  # Adjust radius based on magnitude
                        color = "red",
                        fillOpacity = 0.5,
                        clusterOptions = markerCluster)
}

m