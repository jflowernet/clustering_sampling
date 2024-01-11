#function for comparing runtimes using different dataframe sizes and max number of clusters when finding optimal cluster size using NbClust
#can enter single integers and vectors of integers for sample_sizes and max_num_clusters. If sample_sizes == NULL, then all data are used
optimal_cluster_size_timing <- function(sample_sizes = NULL, max_num_clusters, data_for_clustering_df){
  
  results_df <- data.frame()
  sample_sizes <- sort(sample_sizes)
  max_num_clusters <- sort(max_num_clusters)
  
  if(is.null(sample_sizes)) sample_sizes <- nrow(data_for_clustering_df) 
  
  for (i in max_num_clusters){
    for(j in sample_sizes){
      print(paste0("Running max clusters = ", i, ", Sample size = ", j)) 
      
      df_sample <- data_for_clustering_df[sample.int(nrow(data_for_clustering_df), j),]
      
      start_time <- Sys.time()
      
      clust_result <- NbClust::NbClust(data = df_sample, method = "kmeans", max.nc = i,  index = "hartigan")
      
      time_diff <- difftime(Sys.time(), start_time)
      
      results_df <- rbind(results_df, c(j, sprintf("%.2f", 100*j/nrow(data_for_clustering_df)), i, as.numeric(clust_result$Best.nc[1]), sprintf("%.2f", time_diff), units(time_diff)))
    }
  }
  
  colnames(results_df) <- c("Sample size", "Sample size %", "Max number clusters", "Number of clusters", "Run time", "Run time units")
  results_df
}