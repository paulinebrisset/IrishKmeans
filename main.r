# On considère les données Iris de Fisher
# Charger la bibliothèque "datasets", puis les données Iris
library(datasets)
data(iris)

# 1. Retirer la colonne classe des données
iris_data <- iris[, -5]  # Retirer la colonne 5, qui est "Species"
# 2. Calculer la matrice du carré de la distance euclidienne entre les individu
euclidean_distance_matrix <- dist(iris_data, method = "euclidean")^2

#3. Définir une fonction nommée « silPartition » qui calcule l’indice silhouette d’une partition donnée
silPartition <- function(data, clusters) {
  library(cluster)
  
  # Calculer l'indice silhouette
  silhouette_result <- silhouette(clusters, dist(data))
  silhouette_index <- mean(silhouette_result[, "sil_width"])
  
  # Retourner la valeur moyenne de l'indice silhouette
  return(silhouette_index)
}

# Étape 6: Partitionner l'ensemble des individus en 3 classes par le K-Means direct
k <- 3
kmeans_result <- kmeans(iris_data, centers = k)

# Étape 7: Appliquer la fonction "silPartition" à la partition K-Means
silhouette_index_kmeans <- silPartition(iris_data, kmeans_result$cluster)

# Étape 8: Définir une fonction "inertieIntraPartition" pour calculer l'inertie intra-classe
inertieIntraPartition <- function(data, clusters, centroids) {
  # Calculer les distances euclidiennes au carré de chaque point à son centre de cluster
  cluster_distances <- apply(data, 1, function(x) sum((x - centroids[clusters,])^2))
  
  # Somme des distances au carré à l'intérieur de chaque cluster
  inertie <- sum(cluster_distances)
  
  return(inertie)
}

# Étape 9: Appliquer la fonction "inertieIntraPartition" à la partition K-Means
kmeans_inertie <- inertieIntraPartition(iris_data, kmeans_result$cluster, kmeans_result$centers)

# Étape 10: Classification ascendante hiérarchique avec différents liens
# (a) Lien simple
single_linkage <- hclust(dist(iris_data), method = "single")

# (b) Lien moyen
average_linkage <- hclust(dist(iris_data), method = "average")

# (c) Lien complet
complete_linkage <- hclust(dist(iris_data), method = "complete")

# Étape 11: Définir une fonction "sautMax" pour calculer le niveau de coupure au saut maximum
sautMax <- function(dendrogram) {
  hauteurs <- dendrogram$height
  sauts <- diff(hauteurs)
  saut_max <- max(sauts)
  niveau_saut_max <- hauteurs[which.max(sauts) + 1]  # +1 pour obtenir le niveau correct
  return(niveau_saut_max)
}

# Étape 12: Calculer le saut maximum pour chaque lien
saut_max_single <- sautMax(single_linkage)
saut_max_average <- sautMax(average_linkage)
saut_max_complete <- sautMax(complete_linkage)

# Étape 13: Déterminer les partitions correspondant au saut maximum pour chaque lien
partition_single <- cutree(single_linkage, h = saut_max_single)
partition_average <- cutree(average_linkage, h = saut_max_average)
partition_complete <- cutree(complete_linkage, h = saut_max_complete)

# Étape 14: Calculer l'indice silhouette pour chaque partition
silhouette_index_single <- silPartition(iris_data, partition_single)
silhouette_index_average <- silPartition(iris_data, partition_average)
silhouette_index_complete <- silPartition(iris_data, partition_complete)

# Étape 15: Calculer l'inertie intra-classe pour chaque partition
inertie_single <- inertieIntraPartition(iris_data, partition_single, tapply(1:nrow(iris_data), partition_single, function(i) colMeans(iris_data[i,])))
inertie_average <- inertieIntraPartition(iris_data, partition_average, tapply(1:nrow(iris_data), partition_average, function(i) colMeans(iris_data[i,])))
inertie_complete <- inertieIntraPartition(iris_data, partition_complete, tapply(1:nrow(iris_data), partition_complete, function(i) colMeans(iris_data[i,])))

# Imprimer les résultats
cat("Silhouette index for Single Linkage partition:", silhouette_index_single, "\n")
cat("Silhouette index for Average Linkage partition:", silhouette_index_average, "\n")
cat("Silhouette index for Complete Linkage partition:", silhouette_index_complete, "\n")
cat("Intra-class inertia for K-Means partition:", kmeans_inertie, "\n")
cat("Intra-class inertia for Single Linkage partition:", inertie_single, "\n")
cat("Intra-class inertia for Average Linkage partition:", inertie_average, "\n")
cat("Intra-class inertia for Complete Linkage partition:", inertie_complete
