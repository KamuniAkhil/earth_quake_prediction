library(httr)
library(jsonlite)
library(tidyverse)
library(leaflet)
library(geosphere)
library(sf)
library(tidyverse)
library(ggplot2)
library(caret)
library(cluster)

file_path <- "C:\\Users\\Akhil_kamuni\\OneDrive\\Desktop\\MP_2\\MP_2\\datasets\\combined_eq_california_clean.csv"
df_eq <- read.csv(file_path)
df_eq$time <- as.POSIXct(df_eq$time) 
head(df_eq, 3)


str(df_eq)

options(repr.plot.width = 7, repr.plot.height = 4)

hist(log(df_eq$mag), main = "EQ magnitude", xlab = "EQ magnitude", ylab = "Frequency", col = "skyblue", border = "white")


options(repr.plot.width = 7, repr.plot.height = 4)

# Create the histogram plot for EQ depth
hist(log(df_eq$depth), main = "EQ Depth", xlab = "EQ Depth", ylab = "Frequency", col = "skyblue", border = "white")



library(ggplot2)

# Set plot options
options(repr.plot.width=7, repr.plot.height=4)

# Plot histogram
ggplot(df_eq, aes(x = longitude)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "white", log ="y") +
  labs(x = "EQ longitude", y = "Frequency", fontsize = 13) +
  theme_minimal() +
  theme(text = element_text(size = 13))



# Set the figure size
options(repr.plot.width = 7, repr.plot.height = 4)

# Create the histogram plot for EQ latitude
hist(log(df_eq$latitude), main = "EQ Latitude", xlab = "EQ Latitude", ylab = "Frequency", col = "skyblue", border = "white")


# Subset the data frame
df_eq_large <- subset(df_eq, mag > 6)

# Convert the "time" column to POSIXct format (assuming it contains timestamps)
df_eq_large$time <- as.POSIXct(df_eq_large$time)

# Calculate the time difference in days using the difftime function
time_diff <- c(NA, as.numeric(diff(df_eq_large$time), units = "days"))

# Add the time difference as a new column
df_eq_large$time_diff_day <- time_diff

# Print the head of the data frame to check the results
#head(df_eq_large)


head(df_eq_large, 5)  # Display the first 5 rows of the filtered data frame


str(df_eq_large)


nrow(df_eq_large)


summary(df_eq_large$time_diff_day)


# Set the figure size
options(repr.plot.width = 7, repr.plot.height = 4)

# Convert time_diff_day to seconds
df_eq_large$time_diff_sec <- as.numeric(df_eq_large$time_diff_day * 86400)  # 86400 seconds in a day

# Create the histogram plot for EQ time difference in seconds
hist(df_eq_large$time_diff_sec, main = "EQ Time Difference (Seconds)", xlab = "EQ Time Difference (Seconds)", ylab = "Frequency", col = "skyblue", border = "white")


# Set the figure format
options(repr.plot.width = 9, repr.plot.height = 5)

# Create a plot for the "mag" column in df_eq_large
plot(df_eq_large$mag, type = "o", col = "red", xlab = "Index", ylab = "Magnitude", main = "Magnitude")

# Add a legend
legend("topright", legend = "Magnitude", col = "red", lty = 1, cex = 1, bty = "n")


# Set the figure size
options(repr.plot.width = 7, repr.plot.height = 4)

# Create a scatter plot
plot(df_eq_large$time_diff_sec, df_eq_large$mag, 
     xlab = "EQ Time Difference (Seconds)", ylab = "Magnitude", 
     main = "Scatter Plot of EQ Time Difference vs Magnitude", 
     col = "blue", pch = 16)

# Add labels
text(df_eq_large$time_diff_sec, df_eq_large$mag, labels = df_eq_large$time_diff_day, pos = 3)


# Set the figure size
options(repr.plot.width = 7, repr.plot.height = 4)

# Create a scatter plot
plot(df_eq_large$time_diff_sec, df_eq_large$mag, 
     xlab = "EQ Time Difference (Seconds)", ylab = "Magnitude", 
     main = "Scatter Plot of EQ Time Difference vs Magnitude", 
     col = "blue", pch = 16)

# Add labels
text(df_eq_large$time_diff_sec, df_eq_large$mag, labels = df_eq_large$time_diff_day, pos = 3)


# Assuming 'df_eq_large' is a data frame with a datetime index
date <- as.Date(as.character(df_eq_large$time[1]))  # Extracting the first datetime value and converting it to date

class(date)  # Check the class of 'date'

library(maps)
library(mapdata)
plot.new()

map("world", col = "lightgray", xlim = c(-130, -114), ylim = c(32, 42), fill = TRUE, bg = "white")
map.axes()

# Plot EQ locations
points(df_eq_large$longitude, df_eq_large$latitude, col = "red", pch = 0.19, cex = df_eq_large$mag/5, bg = "black", lwd = 1)

# Add labels
legend("topright", legend = "Magnitude", pch = 19, col = "red", pt.cex = df_eq_large$mag/5, bg = "black", cex = 1, bty = "n")

library(leaflet)
library(htmlwidgets)
library(ggmap)             # Load ggmap package
library(tmaptools)             # Load tmaptools package
library(ggmap)



# Assuming df_eq has a timestamp column named 'time'
df_eq_lp <- subset(df_eq, time >= as.POSIXct("1989-09-19") & time <= as.POSIXct("1989-10-19"))

head(df_eq_lp)


library(mapdata)

# Set up the plot with specified projection parameters
map("world", col = "lightgray", xlim = c(-130, -114), ylim = c(32, 42), fill = TRUE, bg = "white")
map.axes()

# Extract longitude and latitude values from df_eq_lp
lon <- df_eq_lp$longitude
lat <- df_eq_lp$latitude

# Calculate marker size based on magnitude
size <- df_eq_lp$mag / 5

# Plot the earthquake locations as circles
points(lon, lat, col = "red", pch = 19, cex = size, bg = "black", lwd = 1)



library(ggplot2)

# Plot the "mag" column against time
ggplot(df_eq_lp, aes(x = time, y = mag)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Magnitude", x = "Time", y = "Magnitude") +
  theme_minimal()


library(ggplot2)

# Plot the "depth" column against time
ggplot(df_eq_lp, aes(x = time, y = depth)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Depth", x = "Time", y = "Depth") +
  theme_minimal()


library(ggplot2)


ggplot(df_eq_lp, aes(x = time, y = longitude)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Longitude", x = "Time", y = "Longitude") +
  theme_minimal()


library(ggplot2)

ggplot(df_eq_lp, aes(x = time, y = latitude)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Latitude", x = "Time", y = "Latitude") +
  theme_minimal()



