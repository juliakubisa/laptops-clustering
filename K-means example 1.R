### EXAMPLE 1 

#### DATA PREPARATION ##### 
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("NbClust")
# install.packages("ClusterR") 
# install.packages("cluster") 

# Loading package 
library(ClusterR) 
library(cluster)
library(dplyr)
library(NbClust)
library(ggplot2)
library(factoextra)
library(corrplot)
library(RColorBrewer)

# Read the data 
df <- read.csv("laptops.csv", header=TRUE)
summary(df)

# Price should be divided by 10
df$Price = df$Price/10

# Leave only numeric columns for kmeans 
df_numeric <- df %>% select(where(is.numeric))
df_numeric <- df_numeric[, !names(df_numeric) %in% c("secondary_storage_capacity")]  # delete this column because majority of rows = 0 

# Check for NA- -> No missing values 
sum(is.na(df))


# Scaling the dataset 
df_scaled <- scale(df_numeric[-1])


### DATA VISUALIZATION ### 
# Visualize the differences within price 
par(mfrow = c(1, 2))

ggplot(data=df, aes(x=Price)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of Price Values")

ggplot(data=df, aes(x=Rating)) +
  geom_histogram(fill="steelblue", color="black") +
  ggtitle("Histogram of Ratings")

# Visualize the Prices within brands 
ggplot(data=df, aes(x=Price, y=brand)) + 
  geom_boxplot(fill="steelblue")


# Correlation between variables 
correlation <- round(cor(df_numeric[-1]),2)
corrplot(correlation, order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"), tl.col="black", tl.cex = 0.5)


### IMPLEMENTING THE METHOD ### 
#  Elbow method - how many clusters we need within sum of square
fviz_nbclust(df_scaled, kmeans, method = "wss", linecolor = "red") + 
  labs(subtitle="Elbow Method") + 
  geom_vline(xintercept = 3, linetype = 2)


k_means_func <- function(k, i) {
  
  # K means 
  startTime <- Sys.time()
  km.out <- kmeans(df_scaled, centers=k, nstart=i)
  endTime <- Sys.time()
  computing_time <- (endTime-startTime)
  print(computing_time)
  
  # Visualize the clustering algorithm results.
  km.clusters<-km.out$cluster

  # Visualize the clusters 
  cluster_viz <- fviz_cluster(list(data=df_scaled, cluster = km.clusters))
  print(cluster_viz)
  
  # Show the mean values for clusters 
  cluster_values <- aggregate(df_numeric[-1], by=list(cluster=km.out$cluster),mean)
  print(cluster_values)
  
  # Final data
  df_with_clusters <- cbind(df, cluster = km.out$cluster)
  cluster_count <- df_with_clusters %>% count(cluster, sort = TRUE)
  print(cluster_count)
  
}


k_means_func(3, 10)
k_means_func(3, 50)
k_means_func(3, 100)
k_means_func(7, 10)
k_means_func(7, 50)


