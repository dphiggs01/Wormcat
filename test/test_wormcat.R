# Test Wormcat functions
library(wormcat)
rm(list= ls())

# Print the available Annotations files
annotation_files <- get_available_annotation_files()
print(annotation_files)

# Setup the call to Wormcat
setwd("~/examples")
file_to_process <- "sams-1_up.csv"
title <- "sams-1 up"
output_dir <- "wormcat_out"
rm_dir = FALSE
annotation_file <-"whole_genome_v2_nov-11-2021.csv"
input_type <- "Wormbase.ID"
zip_files=FALSE


delete_directory_if_exists <- function(directory_path) {
  if (dir.exists(directory_path)) {
    unlink(directory_path, recursive = TRUE)
    message(paste("Directory", directory_path, "deleted."))
  }
}

# delete the output directory if it exists
delete_directory_if_exists(output_dir)

# Call the Wormcat function
worm_cat_fun(file_to_process,
             title,
             output_dir,
             rm_dir,
             annotation_file,
             input_type,
             zip_files)

