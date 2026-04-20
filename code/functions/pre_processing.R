library(here)

here::i_am("code/functions/pre_processing.R")

input_nlcd_path <- function(product, year, version){
  
  # Force correct types
  product <- as.character(product)
  year    <- as.integer(year)
  version <- as.integer(version)
  
  # Access the product raster
  folder_name <- paste0("Annual_NLCD_", product, "_", year, "_CU_C1V", version)
  file_name <- paste0("Annual_NLCD_", product,"_", year, "_CU_C1V",version,".tif")
  full_file_name <- here::here("data", folder_name, file_name)
  
  return(full_file_name)
  # Example: input_nlcd_path(LndCov, 2023, 0)
  # ->"data/Annual_NLCD_LndCov_2023_CU_C1V0/Annual_NLCD_LndCov_2023_CU_C1V0.tif"
}


input_nlcd_path_server <- function(product, year, version){
  
  # Force correct types
  product <- as.character(product)
  year    <- as.integer(year)
  version <- as.integer(version)
  
  # Access the product raster
  folder_name <- paste0("Annual_NLCD_", product, "_", year, "_CU_C1V", version)
  file_name <- paste0("Annual_NLCD_", product,"_", year, "_CU_C1V",version,".tif")
  full_file_name <- paste("D:/Shared/Scratch/Data/lilachong",product,folder_name,file_name, sep = "/")
  
  return(full_file_name)
  # Example: input_nlcd_path_server(LndCov, 2023, 0)
  # ->"D:/Shared/Scratch/Data/lilachong/LndCov/Annual_NLCD_LndCov_2023_CU_C1V0/Annual_NLCD_LndCov_2023_CU_C1V0.tif"

}