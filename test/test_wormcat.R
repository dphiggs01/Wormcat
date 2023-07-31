#Test Worm CAT
library(wormcat)

rm(list= ls())
#setwd("/Users/dan/Code/R_Workspace/Wormcat/test")
setwd("~/projects/Code/R_Workspace/Wormcat/test")
file_to_process <- "sams-1_up.csv"
title <- "sams-1 up"
output_dir <- "wormcat_out"
rm_dir = FALSE
annotation_file <-"whole_genome_v2_nov-11-2021.csv"
input_type <- "Wormbase.ID"

delete_directory_if_exists <- function(directory_path) {
  if (dir.exists(directory_path)) {
    unlink(directory_path, recursive = TRUE)
    message(paste("Directory", directory_path, "deleted."))
  }
}

delete_directory_if_exists(output_dir)

worm_cat_fun(file_to_process,
             title,
             output_dir,
             rm_dir,
             annotation_file,
             input_type)


# Test get_files_in_extdata
#Test Worm CAT
library(wormcat)

annotation_files <- get_available_annotation_files()
print(annotation_files)


