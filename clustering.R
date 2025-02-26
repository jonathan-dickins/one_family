library(tidyverse)
library(fpc)
library(factoextra)
library(NbClust)
library(ggfortify)

#select variables for clustering
#select TotalPremium and Smoker to overlay
clustering <- data %>%
  abbreviate_long_colnames() %>%
  select(QuoteRef, `Person1 Age`,
         NOB, URB, X, LSB, EF, WGB, TotalPremium, Smoker) %>%
  mutate(Smoker = case_when(Smoker == "Y" ~ 1,
                            TRUE ~ 0)) %>%
  drop_na()

#scale numeric variables
clustering_scaled <- clustering %>%
  mutate(across(2:8, scale))

#elbow method for determining k
fviz_nbclust(clustering_scaled[, 2:8], 
             FUNcluster = kmeans, method = "wss")

#ensemble method for determining k
NbClust(data = clustering_scaled[, 2:8],
        diss = NULL, distance = "euclidean", min.nc = 2,
        max.nc = 7, method = "kmeans", index = "all")

#best result is 7 clusters
set.seed(124)
kmeans_result <- kmeans(clustering_scaled[, 2:8], 7)
clustering_scaled$cluster <- kmeans_result$cluster

#adding clusters back to full unscaled dataset
final <- clustering %>%
  left_join(clustering_scaled %>% select(QuoteRef, cluster), by = "QuoteRef")

#examining cluster means
cluster_means <- final %>%
  group_by(cluster) %>%
  summarise(age = round(mean(`Person1 Age`), 0),
            property_group = round(mean(NOB), 0),
            income_group = round(mean(URB), 0),
            months_current_address = round(mean(X), 0),
            other_addresses = round(mean(WGB), 0),
            mean_premium = round(mean(TotalPremium), 0),
            no_in_cluster = n())

#principal components analysis for plotting
pca <- prcomp(clustering_scaled[, 2:8])
pca_data <- as.data.frame(pca$x)
pca_data$Cluster <- as.factor(kmeans_result$cluster)
pca_scores <- pca$x
pca$rotation[, 1]
pca$rotation[, 2]

#plotting kmeans on PCA scores
kmeans_pca <- kmeans(pca_scores[, 1:2], 7)
autoplot(kmeans_pca, data = pca_scores[, 1:2],
         frame = T) +
  labs(title = "Kmeans clustering on PCA scores") +
  theme_minimal()

#examining distributions for PC1
ggplot(pca_data, aes(x = factor(Cluster), y = PC1, fill = factor(Cluster))) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  theme_minimal() +
  labs(title = "Distribution of PC1 Across Clusters",
       x = "Cluster",
       y = "PC1") 

