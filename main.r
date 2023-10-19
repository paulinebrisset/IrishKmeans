# Bibliothèques "stats" pour K-means et dendextend pour le saut max
# install.packages("dendextend")
library(stats)
library(dendextend)

# bibliothèque "datasets" et données Iris
library(datasets)
data(iris)

# 1. Retirer la colonne classe des données
iris_data <- iris[, -5]  # colonne 5 "Species"

# 2. Calculer la matrice du carré de la distance euclidienne entre les individus
euclidean_distance_matrix <- dist(iris_data, "euclidean", FALSE, FALSE, p = 2)^2

# 3. Définir "silPartition" qui calcule l’indice silhouette d’une partition
#WSS
mean_distance_within_cluster <- function(matrix, clusters, point_index) {
  cluster_index <- clusters[point_index]
  cluster_points <- matrix[clusters == cluster_index, ]
  point <- matrix[point_index, ]
  distances <- sqrt(rowSums((cluster_points - point)^2))
  return(mean(distances))
}
#BSS
mean_distance_between_clusters <- function(matrix, clusters, point_index) {
  cluster_index <- clusters[point_index]
  unique_clusters <- unique(clusters)
  unique_clusters <- unique_clusters[unique_clusters != cluster_index]
  min_distance <- Inf

  for (cluster in unique_clusters) {
    cluster_points <- matrix[clusters == cluster, ]
    point <- matrix[point_index, ]
    distances <- sqrt(rowSums((cluster_points - point)^2))
    distance <- mean(distances)

    if (distance < min_distance) {
      min_distance <- distance
    }
  }

  return(min_distance)
}

silhouette_coefficient <- function(a, b) {
  if (a < b) {
    return(1 - (a / b))
  } else if (a > b) {
    return((b - a) / b)
  } else {
    cat("ON A UN PROBLEME")
    return(0)
  }
}
get_silhouette_values <- function(matrix, partition) {
  plot(matrix)
  n <- nrow(matrix)
  cat("N :", matrix, "\n")
  silhouette_values <- numeric(n)

  for (i in 1:n) {
    a <- mean_distance_within_cluster(matrix, partition, i)
    b <- mean_distance_between_clusters(matrix, partition, i)
    silhouette_values[i] <- silhouette_coefficient(a, b)
  }
  return(silhouette_values)
}

silPartition <- function(matrix, partition) {
  silhouette_values <- get_silhouette_values(matrix, partition)
  partition_silhouette <- mean(silhouette_values, na.rm = TRUE)
  return(partition_silhouette)
}

# 4. Partitionner l’ensemble des individus en 3 classes par le K-Means direct
k <- 3
kmeans_result <- kmeans(euclidean_distance_matrix, centers = k)
cluster_assignments <- kmeans_result$cluster
plot(cluster_assignments)

# 5. Appliquer à la partition obtenue la fonction « silPartition »
silhouette_index <- silPartition(as.matrix(euclidean_distance_matrix), cluster_assignments)
plot(silhouette_index)
cat("Indice silhouette de la partition K-means :", silhouette_index, "\n")

#6. Définir une fonction nommée « inertieIntraPartition » qui calcule l’inertie intra-classe d’une partition donnée
inertieIntraPartition <- function(euclidean_distance_matrix, partition) {
  n <- nrow(euclidean_distance_matrix)
  k <- max(partition)  # Nombre de clusters

  inertia <- 0

  for (cluster in 1:k) {
    cluster_indices <- which(partition == cluster)
    if (length(cluster_indices) > 0) {
      cat("cluster indices", cluster_indices, "\n")
      cluster_data <- euclidean_distance_matrix[cluster_indices, ]
      cluster_center <- colMeans(cluster_data)
      distances <- sqrt(rowSums((cluster_data - cluster_center)^2))
      inertia <- inertia + sum(distances^2)
    }
  }
  return(inertia)
}
#7. Appliquer à la partition la fonction « inertieIntraPartition ».
inertie <- inertieIntraPartition(as.matrix(euclidean_distance_matrix), cluster_assignments)
cat("Inertie intra-classe de la partition :", inertie, "\n")

#8. En utilisant la fonction « hclust », effectuer une classification ascendante hiérarchique de l’ensemble des individus par :
#(a) le lien simple ;
cah_simple <- hclust(euclidean_distance_matrix, method = "single")
# Afficher le dendrogramme du lien simple
plot(cah_simple, main = "Lien simple")

# (b) le lien moyen ;
cah_mean <- hclust(euclidean_distance_matrix, method = "average")
plot(cah_mean, main = "Lien moyen")

# (c) le lien complet.
cah_complete <- hclust(euclidean_distance_matrix, method = "complete")
plot(cah_complete, main = "Lien complet")

#9. Définir une fonction nommée « sautMax » qui calcule le niveau de coupure correspondant au saut maximum de l’indice de niveau d’une hiérarchie indicée obtenue par « hclust ».
  # https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html
  # partie dynamisTreeCut
  # et aussi https://cran.r-project.org/web/packages/dendextend/dendextend.pdf
sautMax <- function(hclust_obj) {
  dend <- as.dendrogram(hclust_obj)
  # heights <- attr(dend, "height")
  heights <- get_branches_heights(dend)
  max_jump <- max(heights)
  cat("max_jump :", max_jump, "\n")
  return(max_jump)
}

# 10. Dans chacun des cas de la question (8), déterminer la partition correspondant au saut maximum de l’indice de niveau
#(a) Lien simple
cut_height_simple <- sautMax(cah_simple)
cat("cut_height_simple :", cut_height_simple, "\n")
partition_simple <- cutree(as.dendrogram(cah_simple), k = NULL, h = cut_height_simple)
cat("Partition saut maximum (Lien Simple) :", partition_simple, "\n")
plot(cah_simple, main = paste("Simple (Cut Height =", cut_height_simple, ")")

#(b) Lien moyen
cut_height_mean <- sautMax(cah_mean)
partition_mean <- cutree(cah_mean, h = cut_height_mean)
cat("Partition saut max (Lien moyen) :", partition_mean, "\n")
plot(cah_mean, main = paste("Mean (Cut Height =", cut_height_mean, ")")

# (c) Lien complet
cut_height_complete <- sautMax(cah_complete)
partition_complete <- cutree(cah_complete, h = cut_height_complete)
plot(cah_complete, main = paste("Complete (Cut Height =", cut_height_complete, ")")

cat("Partition saut maximum (complet) :", partition_complete, "\n")