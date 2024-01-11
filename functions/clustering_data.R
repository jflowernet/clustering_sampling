#function for creating clustering data from Bio-Oracle data and specified EEZ and grid size
clustering_data <- function(area_polygon, num_cells = 1e4){
  
  num_cols <- round(sqrt(num_cells))

  num_rows <- round(sqrt(num_cells))
  
  area_polygon |>
    rast(nrow = num_rows, ncol = num_cols, vals = 0) %>%  
    resample(bio_oracle_data, ., method = "average") |> 
    as.data.frame()
}