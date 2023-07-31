
#' Worm Cat Function
#'
#' This function takes a regulated gene set and a category annotations file and runs a Fisher test.
#' @param file_to_process the file name to be processed (file should be in current working directory)
#' @param title the title for your bubble plots
#' @param output_dir the output directory (is a relative directory from the current working directory)
#' @param rm_dir Boolean If FALSE do not remove temp dir. If TRUE remove temp directory
#' @param annotation_file provide an internal annotation file name or a path to an external file
#' @param input_type 'Sequence.ID' or 'Wormbase.ID' the default is Sequence ID
#' @keywords wormcat
#' @export
#' @examples
#' worm_cat_fun()
worm_cat_fun <- function(file_to_process, title="rgs", output_dir=NULL, rm_dir=FALSE, annotation_file="whole_genome_v2_nov-11-2021.csv", input_type="Sequence.ID", zip_files=TRUE) {

    # Get the current working directory
    working_dir <- getwd()

    # If output_dir is not given, create one using a timestamp
    if(is.null(output_dir)) {
      output_dir <- paste("worm-cat_", format(Sys.time(), "%b-%d-%Y-%H:%M:%S"), sep="")
    }

    dir.create(file.path(working_dir, output_dir))


    # If annotation_file contains a file system specific seperator assume an external annotation file is being used
    separator <- .get_system_path_separator()
    if (grepl(separator, annotation_file)) {
      worm_cat_annotations <- annotation_file
      message(paste("Using external Annotation file: ", worm_cat_annotations))
    }else{
      # Get full path to annotations file
      worm_cat_annotations <- system.file("extdata", annotation_file, package="wormcat")
    }

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
    if(zip_files) {
       files2zip <- dir(output_dir, full.names = TRUE)
       zip(zipfile = output_dir, files = files2zip)
    }else{
       message("Ignoring ZIP process")
    }
    if(rm_dir == TRUE){
       message("Cleaning up the working directory")
       unlink(output_dir, TRUE)
    }
}

.get_system_path_separator <- function() {
  if (Sys.info()["sysname"] == "Windows") {
    return("\\")
  } else {
    return("/")
  }
}


#' This function returns a list of available annotation files.
#' @keywords wormcat
#' @export
#' @examples
#' get_available_annotation_files()
get_available_annotation_files <- function() {

  # Get the path to the "extdata" directory within the wormcat package
  extdata_dir <- system.file("extdata", package = "wormcat")

  # List all files in the "extdata" directory
  files_in_extdata <- list.files(extdata_dir, full.names = TRUE)
  annotation_files <- lapply(files_in_extdata, basename)

  return(annotation_files)
}



