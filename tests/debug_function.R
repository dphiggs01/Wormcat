#rm(list = c("get_available_annotation_files", "worm_cat_fun"))

# source("../R/worm_cat_controller.R")
# source("../R/worm_cat_add_categories.R")
# source("../R/worm_cat_fisher_test.R")
# source("../R/worm_cat_acceptable_pvalues.R")
# source("../R/worm_cat_bubble_plot.R")
#library(wormcat)

# Utility function to delete the directory if it exists
delete_directory_if_exists <- function(directory_path) {
  if (dir.exists(directory_path)) {
    unlink(directory_path, recursive = TRUE)
    message(paste("Directory", directory_path, "deleted."))
  }
}


file_to_process <- "/Users/dan/Downloads/sams-1_up.csv"
title <- "sams-1 up"
output_dir <- "~/wormcat_out"
rm_dir <- FALSE
annotation_file <- "whole_genome_v2_nov-11-2021.csv"
input_type <- "Wormbase.ID"
zip_files <- FALSE


delete_directory_if_exists(output_dir)

# Call the WormCat function
cat("Calling wormcat\n")
worm_cat_fun(file_to_process,
             title,
             output_dir,
             rm_dir,
             annotation_file,
             input_type,
             zip_files)


