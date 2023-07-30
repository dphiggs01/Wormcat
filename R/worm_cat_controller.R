
#' Worm Cat Function
#'
#' This function takes a regulated gene set and the category annotations and runs a Fisher test.
#' @param file_to_process the file name to be processed (file should be in current working directory)
#' @param title the title for your bubble plots
#' @param output_dir the output directory (is a relative directory from the current working directory)
#' @param rm_dir Boolean If FALSE do not remove temp dir. If TRUE remove temp directory
#' @param annotation_file 'straight_mmm-DD-YYYY.csv' or 'physiology_mmm-DD-YYYY.csv' the default is straight
#' @param input_type 'Sequence ID' or 'Wormbase ID' the default is Sequence ID
#' @keywords worm cat
#' @export
#' @examples
#' worm_cat_fun()
worm_cat_fun <- function(file_to_process, title="rgs", output_dir=NULL, rm_dir=FALSE, annotation_file="physiology_jul-15-2018.csv", input_type="Sequence.ID") {

    # Get the current working directory
    working_dir <- getwd()

    # If output_dir is not given, create one using a timestamp
    if(is.null(output_dir)) {
      output_dir <- paste("worm-cat_", format(Sys.time(), "%b-%d-%Y-%H:%M:%S"), sep="")
    }

    dir.create(file.path(working_dir, output_dir))

    # Get full path to annotations file
    worm_cat_annotations <- system.file("extdata", annotation_file, package="wormcat")

    # Create the categories file and save it to CSV output
    .worm_cat_add_categories(file_to_process, output_dir, worm_cat_annotations, input_type)

    # Run a fisher test
    .worm_cat_fisher_test(output_dir, worm_cat_annotations)

    # For each of the three files created above:
    # 1. Parse the file only to include the entries with "acceptable pvalues."
    # 2. Create bubble plots for each of the three categories based on the acceptable pvlaues.
    for(i in 1:3) {

      rgs_fisher_cat_csv <- sprintf("./%s/rgs_fisher_cat%d.csv", output_dir, i)

      .worm_cat_acceptable_pvalues(rgs_fisher_cat_csv)

      rgs_fisher_cat_apv_csv <- sprintf("./%s/rgs_fisher_cat%d_apv.csv",output_dir, i)

      plot_title <- paste(title, sprintf("category%d",i), sep=":")
      .worm_cat_bubble_plot(rgs_fisher_cat_apv_csv, plot_title)
    }

    # Capture basic information about this run write output to run_data.txt
    run_data <- sprintf("./%s/run_data.txt", output_dir)
    runtime_l <- paste("runtime",format(Sys.time(), "%b-%d-%Y-%H:%M:%S"), sep=": ")
    annotation_file_l <- paste("annotation_version",annotation_file, sep=": ")
    input_type_l <- paste("input_type",input_type, sep=": ")

    cat(runtime_l, annotation_file_l, input_type_l, file=run_data,sep="\n",append=TRUE)

    # Create a zip file as the final output
    files2zip <- dir(output_dir, full.names = TRUE)
    zip(zipfile = output_dir, files = files2zip)

    if(rm_dir == TRUE){
      print("Cleaning up the working directory")
       unlink(output_dir, TRUE)
    }
}




