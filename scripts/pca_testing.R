library(terra)
library(magrittr)
source("functions/clustering_data.R")

#get data
bio_oracle_data <- list.files("data/bio_oracle/", full.names = TRUE) |>
  terra::rast()

area_polygon <- mregions2::mrp_get("eez", cql_filter = "territory1 = 'Micronesia'") |>
  vect()

set.seed(1234)

data_for_clustering_df <- clustering_data(area_polygon = area_polygon, num_cells = 1e5)

clustered_data <- kmeans(data_for_clustering_df, centers = 3, nstart = 10)

pca_result <- prcomp(data_for_clustering_df, scale. = TRUE, center = TRUE)

plot(pca_result$x)
biplot(pca_result)

library(magrittr)
pca_df <- pca_result %>% 
  .[["x"]] %>% 
  as.data.frame()
pca_df$enviro_region <- clustered_data$cluster

plot(x = pca_df$PC1, y = pca_df$PC2, col = pca_df$enviro_region, xlab = "PC1", ylab = "PC2", pch = 4, cex = 0.6)
legend("bottomright", legend = unique(pca_df$enviro_region), col = unique(pca_df$enviro_region), pch = 4, cex = 1, title = "Enviro region")
