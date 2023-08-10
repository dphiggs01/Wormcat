library(testthat)
# source("../R/worm_cat_controller.R")
# source("../R/worm_cat_add_categories.R")
# source("../R/worm_cat_fisher_test.R")
# source("../R/worm_cat_acceptable_pvalues.R")
# source("../R/worm_cat_bubble_plot.R")


# Utility function to delete the directory if it exists
delete_directory_if_exists <- function(directory_path) {
    if (dir.exists(directory_path)) {
        unlink(directory_path, recursive = TRUE)
        message(paste("Directory", directory_path, "deleted."))
    }
}



test_that("get_available_annotation_files expect filenames are equal", {
    annotation_files <- get_available_annotation_files()
    #cat(annotation_files)
    expect_contains(annotation_files, "whole_genome_v2_nov-11-2021.csv")
})


test_that("worm_cat_fun smoke test", {
    testdata_dir <- system.file("testdata", package = "wormcat")
    file_to_process <- file.path(testdata_dir, "sams-1_up.csv")
    title <- "sams-1 up"
    output_dir <- "~/wormcat_out"
    rm_dir <- FALSE
    annotation_file <- "whole_genome_v2_nov-11-2021.csv"
    input_type <- "Wormbase.ID"
    zip_files <- FALSE


    tryCatch({
        # Delete the output directory if it exists
        # A more advanced script would backup the directory or quit processing if
        # the directory exists and contains files
        delete_directory_if_exists(output_dir)

        # Call the Wormcat function
        cat("Calling wormcat\n")
        worm_cat_fun(file_to_process,
                     title,
                     output_dir,
                     rm_dir,
                     annotation_file,
                     input_type,
                     zip_files)
        succeed()
    }, error = function(err) {
        message("Error:", conditionMessage(err))
        cat("Error:", conditionMessage(err), "/n")
        fail()
    })
})
