library(data.table)
library(dplyr)
path <- 'C:\\Users\\Akhil_kamuni\\OneDrive\\Desktop\\MP_2\\MP_2\\datasets\\imported_usgs_api'
my_files <- list.files(path, pattern = "raw.csv", full.names = TRUE)\

df_combined <- rbindlist(lapply(my_files, fread))

rownames(df_combined) <- NULL

dim(df_combined)

head(df_combined, 5)

clean_df <- function(df) {

  df_clean <- df[, c('type', 'time', 'coordinates', 'mag', 'place', 'status', 'tsunami', 'sig', 'net', 
                     'nst', 'dmin', 'rms', 'gap', 'magType'), drop = FALSE]
  
  df_clean$coordinates <- gsub("\\[|\\]", "", df_clean$coordinates)
  
  coordinates_split <- strsplit(df_clean$coordinates, ",")
  coordinates_numeric <- sapply(coordinates_split, as.numeric)
  
  long <- coordinates_numeric[1, ]
  lat <- coordinates_numeric[2, ]
  dep <- coordinates_numeric[3, ]
  
  
  df_clean$longitude <- long
  df_clean$latitude <- lat
  df_clean$depth <- dep
  
  # Fixing time
  df_clean$time <- as.POSIXct(as.numeric(df_clean$time)/1000, origin="1970-01-01")
  
  # Dropping useless coordinate column
  df_clean$coordinates <- NULL
  
  return(df_clean)
}

df_combined_clean <- clean_df(df_combined)

head(df_combined_clean)

df_eq <- read.csv("C:\\Users\\Akhil_kamuni\\OneDrive\\Desktop\\MP_2\\MP_2\\datasets\\combined_eq_california_clean.csv")

df_eq <- df_eq[ ,-1]

df_eq$time <- as.POSIXct(df_eq$time)

df_eq$name_mag <- paste("M:", df_eq$mag, "/", sep=" ")
df_eq$name_date <- paste(as.Date(df_eq$time), "/", sep=" ")
df_eq$name <- paste(df_eq$name_mag, df_eq$name_date, df_eq$place, sep="")
d#f_eq <- df_eq %>% select(-name_mag, -name_date)

df_eq <- df_eq[order(df_eq$time), ]

df_eq <- arrange(df_eq, time)

df_eq$depth[is.na(df_eq$depth)] <- mean(df_eq$depth, na.rm = TRUE)
#dep_normalized <- (df_eq$depth - min(df_eq$depth)) / (max(df_eq$depth) - min(df_eq$depth))
#df_eq$depth <- dep_normalized
print(head(df_eq, 5))
summary(df_eq$mag)




  
