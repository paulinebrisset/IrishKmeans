# On considère les données Iris de Fisher
# Charger la bibliothèque "datasets", puis les données Iris
library(datasets)
data(iris)

# 1. Retirer la colonne classe des données
iris_data <- iris[, -5]  # Retirer la colonne 5, qui est "Species"
# 2. Calculer la matrice du carré de la distance euclidienne entre les individu
euclidean_distance_matrix <- dist(iris_data, method = "euclidean")^2

#3. Définir "silPartition" qui calcule l’indice silhouette d’une partition
silPartition <- function(data, partition) {
  n <- length(data)
  silhouette_values <- numeric(n)

  for (i in 1:n) {
    a_i <- mean_distance_within_cluster(data, partition, i)
    b_i <- mean_distance_between_clusters(data, partition, i)

    silhouette_values[i] <- silhouette_coefficient(a_i, b_i)
  }

  partition_silhouette <- mean(silhouette_values)
  return(partition_silhouette)
}
mean_distance_within_cluster <- function(data, clusters, point_index) {
  cluster_index <- clusters[point_index]
  cluster_points <- data[clusters == cluster_index, , drop = FALSE]
  point <- data[point_index, , drop = FALSE]

  if (nrow(cluster_points) > 1) {
    distances <- sqrt(rowSums((cluster_points - point)^2))
    return(mean(distances))
  } else {
    return(0)
  }
}

mean_distance_between_clusters <- function(data, clusters, point_index) {
  cluster_index <- clusters[point_index]
  unique_clusters <- unique(clusters)
  unique_clusters <- unique_clusters[unique_clusters != cluster_index]
  min_distance <- Inf

  for (cluster in unique_clusters) {
    cluster_points <- data[clusters == cluster, , drop = FALSE]
    point <- data[point_index, , drop = FALSE]
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