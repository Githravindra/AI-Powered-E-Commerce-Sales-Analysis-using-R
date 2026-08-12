# clustering.R
# Implements RFM customer segmentation and K-Means clustering with cluster profiling.

library(dplyr)

if (!exists("log_info")) {
  source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/utils/logging.R")
}

#' Perform RFM Customer Segmentation and K-Means Clustering
#' @param df data.frame. Cleaned transaction dataset.
#' @param k Integer. Number of clusters for K-Means (default 3).
#' @return A list containing RFM metrics, cluster labels, centroids, and performance metrics.
segment_customers <- function(df, k = 3) {
  log_info(paste("Performing Customer Segmentation using K-Means (k =", k, ")..."))
  
  if (is.null(df) || nrow(df) == 0) {
    log_warn("Empty dataset provided for customer clustering.")
    return(NULL)
  }
  
  # Source RFM calculation
  if (!exists("calculate_rfm_metrics")) {
    source("/Users/ravindralohar/R_Language/E-Commerce_sales01.CNG/project/modules/customer_analysis.R")
  }
  
  # Calculate raw RFM metrics
  rfm <- calculate_rfm_metrics(df)
  
  n_cust <- nrow(rfm)
  if (n_cust < k) {
    log_warn("Number of customers is less than the requested cluster count k. Reducing k to number of customers.")
    k <- n_cust
  }
  
  if (k <= 1) {
    # If only 1 customer or 1 cluster
    rfm$Cluster <- 1
    rfm$Segment <- "Valued Partners"
    return(list(
      data = rfm,
      centroids = rfm %>% summarize(Recency = mean(Recency), Frequency = mean(Frequency), Monetary = mean(Monetary)),
      k = 1
    ))
  }
  
  # Normalize RFM metrics using Min-Max scaling for K-Means
  min_max_scale <- function(x) {
    if (max(x) == min(x)) return(rep(0, length(x)))
    (x - min(x)) / (max(x) - min(x))
  }
  
  rfm_norm <- rfm %>%
    mutate(
      R_Norm = min_max_scale(Recency),
      F_Norm = min_max_scale(Frequency),
      M_Norm = min_max_scale(Monetary)
    )
  
  # K-Means Clustering
  # Use set.seed for reproducibility
  set.seed(123)
  km <- kmeans(rfm_norm[, c("R_Norm", "F_Norm", "M_Norm")], centers = k, nstart = 25)
  
  rfm$Cluster <- km$cluster
  
  # Profile clusters to dynamically assign logical segment names
  # Calculate centroids of raw variables
  centroids <- rfm %>%
    group_by(Cluster) %>%
    summarise(
      Size = n(),
      Mean_Recency = mean(Recency),
      Mean_Frequency = mean(Frequency),
      Mean_Monetary = mean(Monetary),
      .groups = 'drop'
    )
  
  # Determine segment labels based on centroids:
  # High frequency & monetary, low recency = Champions
  # Low frequency, high recency = At Risk
  # Others = Loyal/Potential
  
  # Calculate a scoring metric for each cluster: F_score + M_score - R_score
  # Scaled centroids
  centroids_norm <- as.data.frame(km$centers)
  centroids_norm$Cluster <- 1:k
  centroids_norm$Score <- centroids_norm$F_Norm + centroids_norm$M_Norm - centroids_norm$R_Norm
  
  # Rank clusters by score
  ranks <- centroids_norm %>%
    arrange(desc(Score)) %>%
    mutate(Rank = row_number())
  
  # Assign segment names
  # Rank 1: Champions, Rank K: At Risk, Others: Loyal
  segment_names <- character(k)
  for (i in 1:k) {
    cluster_idx <- ranks$Cluster[i]
    rank_val <- ranks$Rank[i]
    
    if (rank_val == 1) {
      segment_names[cluster_idx] <- "Champions"
    } else if (rank_val == k) {
      segment_names[cluster_idx] <- "At Risk"
    } else {
      segment_names[cluster_idx] <- "Loyal Customers"
    }
  }
  
  # Add segment labels to customer RFM data
  rfm$Segment <- segment_names[rfm$Cluster]
  
  # Map back sizes and names to centroids data frame
  centroids$Segment <- segment_names[centroids$Cluster]
  centroids <- centroids %>% arrange(desc(Mean_Monetary))
  
  log_info("K-Means clustering completed successfully.")
  
  list(
    data = rfm,
    centroids = centroids,
    k = k,
    withinss = km$tot.withinss
  )
}
