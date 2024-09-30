library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(e1071)
library(base)
library(png)
library(grid)
library(scales)
library(tidyverse)
library(factoextra)
library(fpc)

file_path <- "C:\\Users\\Akhil_kamuni\\OneDrive\\Desktop\\MP_2\\MP_2\\datasets\\combined_eq_california_clean.csv"
df_eq <- read.csv(file_path)

df_lp_eq <- df_eq %>% mutate(index = row_number())
df_lp_eq <- df_lp_eq %>% select(-index, -place, -status, -tsunami, -net, -nst, -type)
head(df_lp_eq)

summary(df_lp_eq)

img_path <- "C:\\Users\\Akhil_kamuni\\OneDrive\\Desktop\\MP_2\\MP_2\\assets\\Satellite_map_region.png"
img <- readPNG(img_path)

raster_img <- rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc"))

# Create the plot
ggplot() +
  annotation_custom(raster_img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void() +
  theme(
    plot.margin = margin(0, 0, 0, 0, "cm"),
    panel.spacing = unit(0, "null"),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

ggplot(df_lp_eq, aes(x = longitude, y = latitude, size = mag/0.05)) +
  geom_point(alpha = 0.4) +
  scale_color_manual(values = scales::hue_pal(h=c(0, length(unique(rownames(df_lp_eq)))))) +
  labs(x = "Longitude", y = "Latitude", color = "Index", size = "Magnitude / 0.05") +
  ggtitle("Earthquake Magnitude and Locations") +
  theme_minimal() +
  theme(legend.position = "right", plot.title = element_text(hjust = 0.5)) +
  guides(color = guide_legend(override.aes = list(size = 4)))

# Display the plot with the legend and a tight layout
ggsave("scatter_plot.png", width = 10, height = 7)


df_lp_eq_cut <- df_lp_eq
df_lp_eq_cut_cut <- df_lp_eq_cut[1:700, ]
df <- df_lp_eq_cut_cut %>%
  select(longitude, latitude, depth)
df <- df_lp_eq_cut_cut %>%
  select(longitude, latitude, depth)
X_scaled <- scale(df)
dbscan_result <- dbscan(X_scaled, eps = 0.2, MinPts = 5)

cluster_labels <- dbscan_result$cluster
print(cluster_labels)

df$cluster <- dbscan_result$cluster

df_2 <- df_lp_eq[1:566, ]
df_2$cluster <- df$cluster

top_clusters <- df_2 %>%
  group_by(cluster) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  slice(1:3) %>%
  pull(cluster)

# Selecting the cluster to plot
if (top_clusters[1] < 0) {
  cluster_mask <- top_clusters[2]
} else {
  cluster_mask <- top_clusters[1]
}

# Filtering the dataframe for the selected cluster
df_selected <- df[df$cluster == cluster_mask, ]

# Scatter plot
ggplot(df_selected, aes(x = longitude, y = latitude)) +
  geom_point(size = 10, color = "black", alpha = 0.4) +
  labs(x = "Longitude", y = "Latitude", title = "Top Cluster Example") +
  xlim(-126, -114) +
  ylim(32, 42) +
  theme_minimal()

# Scatter plot
ggplot(df_selected, aes(x = longitude, y = depth)) +
  geom_point(size = 10, color = "black", alpha = 0.4) +
  labs(x = "Longitude", y = "Depth", title = "Cluster Example (Depth)") +
  xlim(-126, -114) +
  theme_minimal()

