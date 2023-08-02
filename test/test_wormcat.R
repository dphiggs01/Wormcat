# This is a simple script to test the Wormcat functions
# 1. get_available_annotation_files()
# 2. worm_cat_fun()

library(wormcat)

# IMPORTANT Set working directory
##############################################
# The location of Wormcat Development files
# i.e, where the code was checked out of gitHub
# for me the location is /Users/dan/Code/R_Workspace/Wormcat
project_dir <- "/Users/dan/Code/R_Workspace/Wormcat"

message(sprintf("The Wormcat project directory is: %s\n",project_dir))
setwd(sprintf("%s/test",project_dir))

# Remove any objects in the Global Environment
rm(list= ls())

# Print the available Annotations files
annotation_files <- get_available_annotation_files()
message("Available Annotation files are:")
message(annotation_files)
message("\n")

# Set Variables to call Wormcat
file_to_process <- "sams-1_up.csv"
title <- "sams-1 up"
output_dir <- "wormcat_out" # This will create a directory at the current location
rm_dir = FALSE
annotation_file <-"whole_genome_v2_nov-11-2021.csv"
input_type <- "Wormbase.ID"
zip_files=FALSE

message(sprintf("The file to process is %s",file_to_process))
message(sprintf("The report title prefix is %s",title))
message(sprintf("The output directory is %s",output_dir))
message(sprintf("The annotation file is %s",annotation_file))
message(sprintf("The input file contains %ss",input_type))
message("\n")

# Utility function to delete the directory if it exists
delete_directory_if_exists <- function(directory_path) {
  if (dir.exists(directory_path)) {
    unlink(directory_path, recursive = TRUE)
    message(paste("Directory", directory_path, "deleted."))
  }
}

# Delete the output directory if it exists
# A more advanced script would backup the directory or quit processing if
# the directory exists and contains files
delete_directory_if_exists(output_dir)

# Call the Wormcat function
worm_cat_fun(file_to_process,
             title,
             output_dir,
             rm_dir,
             annotation_file,
             input_type,
             zip_files)

message("\n")
message("worm_cat_fun run complete!")
