# Charger la bibliothèque "stats" pour K-means
library(stats)

# Charger la bibliothèque "datasets", puis les données Iris
library(datasets)
data(iris)

# 1. Retirer la colonne classe des données
iris_data <- iris[, -5]  # Retirer la colonne 5 "Species"

# 2. Calculer la matrice du carré de la distance euclidienne entre les individus
euclidean_distance_matrix <- dist(iris_data, method = "euclidean")^2

# 3. Définir "silPartition" qui calcule l’indice silhouette d’une partition
silPartition <- function(data, partition) {
  n <- nrow(data)
  silhouette_values <- numeric(n)
  cat("Nombre n :", n, "\n")
  for (i in 1:n) {
    a_i <- mean_distance_within_cluster(data, partition, i)
    b_i <- mean_distance_between_clusters(data, partition, i)
    silhouette_values[i] <- silhouette_coefficient(a_i, b_i)
    cat("Silhouette :", silhouette_values[i], "\n")
  }

  partition_silhouette <- mean(silhouette_values, na.rm = TRUE)
  return(partition_silhouette)
}

mean_distance_within_cluster <- function(data, clusters, point_index) {
  cluster_index <- clusters[point_index]
  cluster_points <- data[clusters == cluster_index, ]
  point <- data[point_index, ]
  distances <- sqrt(rowSums((cluster_points - point)^2))
  return(mean(distances))
}

mean_distance_between_clusters <- function(data, clusters, point_index) {
  cluster_index <- clusters[point_index]
  unique_clusters <- unique(clusters)
  unique_clusters <- unique_clusters[unique_clusters != cluster_index]
  min_distance <- Inf

  for (cluster in unique_clusters) {
    cluster_points <- data[clusters == cluster, ]
    point <- data[point_index, ]
    distances <- sqrt(rowSums((cluster_points - point)^2))
    distance <- mean(distances)

    if (distance < min_distance) {
      min_distance <- distance
    }
  }

  return(min_distance)
}

silhouette_coefficient <- function(a_i, b_i) {
  if (a_i < b_i) {
    return(1 - (a_i / b_i))
  } else if (a_i > b_i) {
    return(b_i / a_i - 1)
  } else {
    return(0)
  }
}

# 4. Partitionner l’ensemble des individus en 3 classes par le K-Means direct
k <- 3
kmeans_result <- kmeans(as.matrix(iris_data), centers = k)
cluster_assignments <- kmeans_result$cluster

#5. Appliquer à la partition obtenue à la question (4) la fonction « silPartition » définie à la question (3)
silhouette_index <- silPartition(as.matrix(iris_data), cluster_assignments)
cat("Indice silhouette de la partition K-means :", silhouette_index, "\n")

#6. Définir une fonction nommée « inertieIntraPartition » qui calcule l’inertie intra-classe d’une partition donnée
inertieIntraPartition <- function(data, partition) {
  n <- nrow(data)
  k <- max(partition)  # Nombre de clusters
  inertia <- 0

  for (cluster in 1:k) {
    cluster_indices <- which(partition == cluster)
    if (length(cluster_indices) > 0) {
      cluster_data <- data[cluster_indices, ]
      cluster_center <- colMeans(cluster_data)
      distances <- sqrt(rowSums((cluster_data - cluster_center)^2))
      inertia <- inertia + sum(distances^2)
    }
  }
  return(inertia)
}
inertie <- inertieIntraPartition(iris_data, cluster_assignments)
cat("Inertie intra-classe de la partition K-means :", inertie, "\n")
