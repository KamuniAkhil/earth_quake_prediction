library(httr)
library(jsonlite)
library(dplyr)
library(ggplot2)
library(leaflet)  
library(geosphere)  
library(maps) 
library(mapproj) 
library(caret) 
library(cluster) 
library(datasets) 
library(dbscan)  
library(ggplot2)  
library(Rtsne) 
library(randomForest)
library(caret)  
library(IRdisplay)  
library(repr) 
library(xts)
options(repr.plot.width = 6, repr.plot.height = 4)  # Adjust width and height as needed

file_path <- "C:\\Users\\Akhil_kamuni\\OneDrive\\Desktop\\MP_2\\MP_2\\datasets\\combined_eq_california_timeseries.csv"
df_eq <- read.csv(file_path)

df_eq$time <- as.POSIXct(df_eq$time, format = "%Y-%m-%d %H:%M:%S")
head(df_eq, 3)

df_eq_large <- subset(df_eq, mag > 6)
df_eq_large$time <- as.POSIXct(df_eq_large$time, format = "%Y-%m-%d %H:%M:%S")

df_eq_large$time_diff_day <- c(NA, diff(df_eq_large$time)) / (24 * 60 * 60)  # Assuming time difference in seconds
head(df_eq_large, 2)

df_eq$time <- as.Date(df_eq$time)
start_date <- as.Date("1989-09-19")
end_date <- as.Date("1989-10-19")
df_eq_sel_range <- subset(df_eq, time >= start_date & time <= end_date)
head(df_eq_sel_range, 2)


map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -120, lat = 38, zoom = 5)
map <- map %>%
  addCircleMarkers(data = df_eq_sel_range,
                   lng = ~longitude,
                   lat = ~latitude,
                   radius = ~mag/5,
                   color = "red",
                   opacity = 0.5)

map

df_eq_sel_range$time <- as.POSIXct(df_eq_sel_range$time)
ggplot(data = df_eq_sel_range, aes(x = time, y = longitude)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Longitude", x = "Time", y = "Longitude") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

df <- df_eq_sel_range[, c("longitude", "latitude", "depth")]
ss <- scale(df)

eps_param <- 0.33
min_sample <- 7

dbscan <- dbscan(ss, eps = eps_param, minPts = min_sample)
df$cluster <- dbscan$cluster
core_samples_mask <- dbscan$core$border
n_clusters <- length(unique(dbscan$cluster)) - (1 %in% dbscan$cluster)
n_noise <- sum(dbscan$cluster == 0)

cat("Total number of points:", nrow(df), "\n")
cat("Estimated number of clusters:", n_clusters, "\n")
cat("Estimated number of noise points:", n_noise, "\n")
#cat("Silhouette Coefficient:", silhouette(ss, dbscan$cluster), "\n")

cluster_counts <- table(df$cluster)
cat("Clusters:", cluster_counts, "\n")

top_cluster <- names(sort(table(df$cluster), decreasing = TRUE))[1]
cat("Top cluster is:", top_cluster, "\n")

if (top_cluster == "-1") {
  cluster_mask <- names(sort(table(df$cluster), decreasing = TRUE))[2]
} else {
  cluster_mask <- top_cluster
}

df_filtered <- df[df$cluster != -1, ]

plot <- ggplot(df_filtered, aes(x = longitude, y = latitude, color = factor(cluster))) +
  geom_point(size = 3) +
  scale_color_manual(values = c("gray", "blue", "green", "red", "orange","pink","black")) +
  labs(title = "Clusters", x = "Longitude", y = "Latitude") +
  theme_minimal()

plot



cluster_counts <- table(df$cluster)

sorted_clusters <- sort(cluster_counts, decreasing = TRUE)

top_cluster <- names(sorted_clusters)[1]
print(top_cluster)

print(head(core_samples_mask, 10))
cluster_counts <- table(df$cluster)
print(cluster_counts)



df_top_cluster <- df_eq_sel_range[df_eq_sel_range$cluster == cluster_mask, ]

df_top_cluster

map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -120, lat = 38, zoom = 5)

map <- map %>%
  addCircleMarkers(data = df_top_cluster,
                   lng = ~longitude,
                   lat = ~latitude,
                   radius = ~mag/0.05,
                   color = "red",
                   opacity = 0.5)

map

dbscan <- dbscan(ss, eps = eps_param, minPts = min_sample)

labels <- dbscan$cluster
df$cluster_labels <- factor(labels)

plot <- ggplot(df, aes(x = longitude, y = latitude, color = cluster_labels)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("gray", "blue", "green", "red", "orange")) +
  labs(title = "Clusters", x = "Longitude", y = "Latitude") +
  theme_minimal()

plot + guides(color = guide_legend(title = "Cluster"))


df_filtered <- df[df$cluster != -1, ]

unique_clusters <- unique(df_filtered$cluster)
print(unique_clusters)

eps_param <- 0.33
print(eps_param)



library(FNN)  # For k-nearest neighbors
nn <- get.knn(ss, k = min_sample)

distances <- nn$nn.dist
sorted_distances <- sort(distances[, min_sample], decreasing = FALSE)
indices <- seq_along(sorted_distances)
relative_change <- c(0, diff(sorted_distances) / sorted_distances[-length(sorted_distances)])
smoothed_change <- stats::filter(relative_change, rep(1/3, 3), sides=2)

# Find knee point
knee_index <- which.max(smoothed_change)

# Plot knee point
plot(indices, sorted_distances, type = "l", xlab = "Index", ylab = "Distances")
abline(v = knee_index, col = "red")
print(sorted_distances[knee_index])

