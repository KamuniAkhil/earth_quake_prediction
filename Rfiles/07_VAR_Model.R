library("data.table")
library("stats")
library("forecast")
library("dplyr")
library("vars")

file_path <- "C:\\Users\\Akhil_kamuni\\OneDrive\\Desktop\\MP_2\\MP_2\\datasets\\combined_eq_california_timeseries.csv"
df_eq <- read.csv(file_path)

df_eq <- df_eq[, c("time", "mag", "sig", "longitude", "latitude", "depth")]

df_eq$time <- as.POSIXct(df_eq$time)
df_eq$timestamps <- df_eq$time

head(df_eq)

str(df_eq)

duplicated_rows <- df_eq[duplicated(df_eq$timestamps), ]
df_eq <- df_eq[!duplicated(df_eq$timestamps), ]

# Calculating difference between consecutive timestamps
timestamps_diff <- diff(df_eq$timestamps)
summary(timestamps_diff)
str(df_eq)
library(zoo)

# Number 1: Time intervals between consecutive earthquakes
df_eq$time_diff <- c(NA, diff(df_eq$timestamps))
df_eq$time_diff_float <- as.numeric(df_eq$time_diff, units = "secs")

# Number 2: Rolling mean of magnitudes from the last 10 earthquakes
df_eq$mag_roll_10 <- rollapplyr(df_eq$mag, width = 10, FUN = mean, fill = NA, 
                                align = "right")

# Drop rows with NA values
df_eq <- na.omit(df_eq)
summary(df_eq)

filtered_rows <- df_eq[df_eq$time_diff_float > 86400, ]

num_filtered_rows <- nrow(filtered_rows)

print(num_filtered_rows)

# Filter rows based on the condition time_diff_float > 86400 (which means time difference greater than 86400 seconds)
filtered_rows <- subset(df_eq, time_diff_float > 86400)

print(filtered_rows)

min_date <- min(df_eq$time)
min_date <- as.Date(min_date)
print(min_date)

max_date <- max(df_eq$time)
max_date <- as.Date(max_date)
print(max_date)

max_date <- max(df_eq$time)
max_date <- as.Date(max_date)
print(typeof(max_date))


start_date <- min(df_eq$time)

# Calculate the number of days between min_date and max_date
number_of_days <- as.integer(difftime(max(df_eq$time), min(df_eq$time), units = "days"))
print(paste("number_of_days:", number_of_days))

date_list <- seq(start_date, by = "days", length.out = number_of_days)

print(date_list[2])
print(class(date_list[2]))
start_date <- min(df_eq$time)

number_of_days <- as.integer(difftime(max(df_eq$time), min(df_eq$time), units = "days"))
print(paste("number_of_days:", number_of_days))

date_list <- seq(start_date, by = "days", length.out = number_of_days)

print(date_list[2])
print(class(date_list[2]))
df_eq

setDT(df_eq)
setkey(df_eq, time)


rownames(df_eq) <- df_eq$time

setkey(df_eq, time)

library(zoo)

# Aggregate data by day
df_eq[, date := as.IDate(time)]
df_daily <- df_eq[, .(
  mag_max = max(mag, na.rm = TRUE),
  event_count = .N,
  mag_mean = mean(mag, na.rm = TRUE),
  mag_sum = sum(mag, na.rm = TRUE),
  mag_scatter = sd(mag, na.rm = TRUE),
  longitude_mean = mean(longitude, na.rm = TRUE),
  longitude_std = sd(longitude, na.rm = TRUE),
  latitude_mean = mean(latitude, na.rm = TRUE),
  latitude_std = sd(latitude, na.rm = TRUE),
  depth_mean = mean(depth, na.rm = TRUE),
  depth_std = sd(depth, na.rm = TRUE),
  time_diff_float_mean = mean(time_diff_float, na.rm = TRUE),
  time_diff_float_std = sd(time_diff_float, na.rm = TRUE)
), by = date]

# Calculate the rolling mean for the mag_mean column with a window of 10 days
df_daily[, mag_roll_10 := rollapply(mag_mean, width = 10, FUN = mean, fill = NA, align = "right")]

df_daily


library(data.table)
library(ggplot2)
library(naniar)
df_daily <- as.data.frame(df_daily)
ggplot_missing <- vis_miss(df_daily) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Heatmap of Missing Values")
print(ggplot_missing)

ggsave("heatmap_missing_values.png", plot = ggplot_missing, width = 8, height = 5)


setDT(df_daily)

df_daily_clean <- df_daily[date > as.IDate("1972-01-01")]

df_daily_clean[, mag_roll_10 := NULL]

print(df_daily_clean)

df_daily_clean <- as.data.frame(df_daily_clean)

ggplot_missing <- vis_miss(df_daily_clean) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Heatmap of Missing Values")
print(ggplot_missing)

ggsave("heatmap_missing_values.png", plot = ggplot_missing, width = 8, height = 5)

library(data.table)

setDT(df_daily_clean)

str(df_daily_clean)

summary(df_daily_clean)


library(data.table)
library(zoo)

df_daily_clean <- as.data.frame(df_daily_clean)

df_daily_clean$date <- as.POSIXct(df_daily_clean$date)

df_daily_clean <- df_daily_clean[order(df_daily_clean$date), ]

columns_to_interpolate <- c("mag_max", "event_count", "mag_mean", "mag_sum", "mag_scatter", 
                            "longitude_mean", "longitude_std", "latitude_mean", 
                            "latitude_std", "depth_mean", "depth_std", 
                            "time_diff_float_mean", "time_diff_float_std")

# Perform linear interpolation on specified columns
for (col in columns_to_interpolate) {
  df_daily_clean[[col]] <- na.approx(df_daily_clean[[col]], rule = 2)  # rule = 2 handles leading/trailing NAs
}

df_daily_clean$date <- as.Date(df_daily_clean$date)

setDT(df_daily_clean)

if (any(is.na(df_daily_clean))) {
  print("Warning: There are still NA values in df_daily_clean after interpolation.")
}

print(df_daily_clean)

df_daily_clean <- df_daily_clean[complete.cases(df_daily_clean), ]

print(df_daily_clean)

df_daily_clean <- as.data.frame(df_daily_clean)


str(df_daily_clean)

df_eq <- df_daily_clean
df_eq_shape <- dim(df_eq)

print(df_eq_shape)

str(df_eq)

label <- vector("integer", length = nrow(df_eq))
cnt <- 0

# Loop through each row and assign labels based on magnitude
for (i in 1:nrow(df_eq)) {
  if (df_eq$mag_max[i] > 5.5) {
    cnt <- cnt + 1
    label[i] <- as.integer(cnt)
  } else {
    label[i] <- 0
  }
}

df_eq$large_eq_label <- label

print(df_eq)

df_eq <- as.data.frame(df_eq)

summary_stats <- summary(df_eq)

summary_stats_transposed <- t(summary_stats)
print(summary_stats_transposed)

str(df_eq)

library(ggplot2)
library(gridExtra)

df_eq <- as.data.frame(df_eq)

plots <- lapply(names(df_eq), function(col) {
  ggplot(df_eq, aes_string(x = col)) +
    geom_histogram() + 
    labs(title = col) 
})

# Arrange plots in a grid layout
grid.arrange(grobs = plots, ncol = 3)  # Example: Arrange in 3 columns

str(df_eq)


library(corrplot)
df_numeric <- df_eq[, !names(df_eq) %in% "date"]
# Calculate the correlation matrix
df_corr <- cor(df_numeric)
print(df_corr)
corrplot(df_corr, method = "color", type = "upper", 
         col = colorRampPalette(c("blue", "white", "red"))(100),
         tl.col = "black", tl.srt = 45, diag = FALSE)

df_eq <- df_daily_clean

print(head(df_eq, 2))


interpret_dftest <- function(dftest) {
  dfoutput <- list(
    "Test Statistic" = dftest$statistic,
    "p-value" = dftest$p.value,
    "Lag Used" = dftest$parameter
  )
  
  return(dfoutput)
}
library(tseries)
# Perform Dickey-Fuller test
dftest <- adf.test(df_eq$mag_max)
result <- interpret_dftest(dftest)
print(result)


test_size <- 0.25
num_test_rows <- round(nrow(df_eq) * test_size)

test_indices <- sample(seq_len(nrow(df_eq)), size = num_test_rows, replace = FALSE)

# Create the test set using the sampled indices
test <- df_eq[test_indices, ]

# Create the train set by excluding the rows in the test set
train <- df_eq[-test_indices, ]

# Load the vars package
library(vars)
train_ts <- ts(train, frequency = 1)
# Instantiate a VAR model
model <- VAR(train_ts)
# Fit a VAR model with specified parameters
ts_model <- VAR(train_ts, p = 60, type = "const", ic = "aic")
ts_model.k_ar <- ts_model$p
print(ts_model.k_ar)
forecast_steps <- 3
forecast <- predict(ts_model, n.ahead = forecast_steps)
print(forecast)

#pdf("forecast_plot.pdf", width = 10, height = 6)
#plot(forecast)
#dev.off()

library(forecast)
forecast_horizon <- nrow(test)
forecast_result <- predict(ts_model, n.ahead = forecast_horizon)

forecast_values <- forecast_result$fcst

forecast_df <- data.frame(lapply(forecast_values, function(x) x[, "fcst"]))

names(forecast_df) <- names(test)

numeric_columns <- sapply(test, is.numeric)

test_numeric <- test[, numeric_columns]
forecast_numeric <- forecast_df[, numeric_columns]

names(forecast_numeric) <- names(test_numeric)

numeric_columns <- sapply(test, is.numeric)
test_numeric <- test[, numeric_columns]
forecast_numeric <- forecast_df[, numeric_columns]

names(forecast_numeric) <- names(test_numeric)

# Calculate and print MSE for each numeric column in the test data
for (i in seq_along(test_numeric)) {
  mse_value <- mean((test_numeric[[i]] - forecast_numeric[[i]])^2, na.rm = TRUE)
  cat(sprintf("The test MSE on the %s data is: %.4f\n", names(test_numeric)[i], mse_value))
}


library(ggplot2)
plot_data <- data.frame(
  Index = 1:length(test_numeric[, 1]),  
  True = test_numeric[, 1], 
  Predicted = forecast_numeric[, 1] 
)
ggplot(plot_data, aes(x = Index)) +
  geom_line(aes(y = True, color = "True"), size = 1) +
  geom_point(aes(y = True, color = "True"), size = 1, shape = 19) +
  geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
  geom_point(aes(y = Predicted, color = "Predicted"), size = 1, shape = 19) +
  scale_color_manual(values = c(True = "blue", Predicted = "red")) +
  labs(
    x = "Index",
    y = "Moment magnitude",
    title = "Maximum daily amplitude time-series",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 18, face = "bold")
  )

library(ggplot2)

df_eq_plot <- df_eq
df_eq_plot$time_days <- df_eq_plot$time_diff_float_mean / (24 * 60 * 60)  # Convert seconds to days
ggplot(df_eq_plot, aes(y = time_days)) +
  geom_histogram(binwidth = 1, color = 'black', fill = 'red', alpha = 0.7) +
  coord_flip() +  # Flip coordinates to make it horizontal
  labs(
    title = '50 years of California earthquakes',
    y = 'Time interval (days)',
    x = 'Frequency of earthquake occurrence'
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 24, face = "bold"),
    axis.title.y = element_text(size = 22),
    axis.title.x = element_text(size = 22),
    axis.text.y = element_text(size = 18),
    axis.text.x = element_text(size = 18)
  ) +
  scale_x_log10()  # Log scale on the x-axis


df_eq_model <- df_eq[, !(names(df_eq) %in% c("large_eq_label"))]

]train_size <- round(nrow(df_eq_model) * 0.9)
test_size <- nrow(df_eq_model) - train_size

train <- df_eq_model[1:train_size, ]
test <- df_eq_model[(train_size + 1):nrow(df_eq_model), ]

print(paste("Train shape:", nrow(train), "rows,", ncol(train), "columns"))
print(paste("Test shape:", nrow(test), "rows,", ncol(test), "columns"))

f_columns <- c('event_count', 'mag_mean', 'mag_sum', 'mag_scatter',
               'longitude_mean', 'longitude_std', 'latitude_mean', 'latitude_std',
               'depth_mean', 'depth_std', 'time_diff_float_mean',
               'time_diff_float_std')

train_scaled <- train
train_scaled[, f_columns] <- scale(train_scaled[, f_columns])

train_scaled$mag_max <- scale(train_scaled$mag_max)

print(head(train_scaled))

