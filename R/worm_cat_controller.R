
#' Worm Cat Function
#'
#'  worm_cat_fun uses a concise list of nested categories defined in the annotation_file
#'  where each gene is first assigned to a category based on physiological function, and then to a molecular function or cellular location.
#'  worm_cat_fun output provides a scaled bubble chart that allows the visualization and direct comparison of complex datasets.
#'  The tool also provides csv files containing input gene annotations, P-values from Fisher’s exact tests, and Bonferroni multiple hypothesis testing corrections.
#' @param file_to_process The file name to the gene set to be processed.
#' @param title The title for the bubble plots.
#' @param output_dir The output directory (this is usually a relative directory from the current working directory).
#' @param rm_dir A flag to remove the processing directory, the Default is \code{TRUE}. This assumes the \code{zip_files} is also \code{TRUE} CAUTION: If \code{rm_dir=TRUE} and \code{zip_files=FALSE} no output will be created.
#' @param annotation_file An internal annotation file name or a path to an external annotation file.
#' @param input_type The type of input data. \code{'Sequence.ID'} or \code{'Wormbase.ID'} the default is \code{'Sequence.ID'}.
#' @param zip_files A flag to create a zip file of the final content. The default is \code{TRUE}.
#' @export
#' @examples
#' worm_cat_fun(file_to_process="/home/rstudio/examples/sams-1_up.csv",output_dir="./output",annotation_file="whole_genome_v2_nov-11-2021.csv",input_type="Wormbase.ID")
#'
worm_cat_fun <- function(file_to_process, title="rgs", output_dir=NULL, rm_dir=FALSE, annotation_file="whole_genome_v2_nov-11-2021.csv", input_type="Sequence.ID", zip_files=TRUE) {
    # Check the input file for validity
    if (!file.exists(file_to_process)) {
       print(sprintf("The file %s cannot be found.",file_to_process))
       print("EXITING!")
       return()
    }

    # If output_dir is not given, create one using a timestamp
    if(is.null(output_dir)) {
      output_dir <- paste("worm-cat_", format(Sys.time(), "%b-%d-%Y-%H:%M:%S"), sep="")
    }

    # Check if the output_dir directory exists
    if (dir.exists(output_dir)) {
       # Check if the directory is empty
       if (length(list.files(directory_path)) == 0) {
          message(sprintf("The directory %s exists and is Empty.",directory_path))
       } else {
         print(sprintf("The directory %s is not Empty.",directory_path))
         print("EXITING!")
         return()
       }
    } else {
      # Create the output directory
      dir.create(output_dir, recursive = TRUE)
    }

    # If annotation_file contains a file system specific separator assume an external annotation file is being used
    separator <- .get_system_path_separator()
    if (grepl(separator, annotation_file)) {
      worm_cat_annotations <- annotation_file
    }else{
      # Get full path to annotations file
      worm_cat_annotations <- system.file("extdata", annotation_file, package="wormcat")
    }

    if (!file.exists(worm_cat_annotations)) {
      print(sprintf("The annotation file %s cannot be found.",worm_cat_annotations))
      print("EXITING!")
      return()
    }

    # Create the categories file and save it to CSV output
    .worm_cat_add_categories(file_to_process, output_dir, worm_cat_annotations, input_type)
    # Run a fisher test
    .worm_cat_fisher_test(output_dir, worm_cat_annotations)

    # For each of the three files created above:
    # 1. Parse the file only to include the entries with "acceptable p-values."
    # 2. Create bubble plots for each of the three categories based on the acceptable p-vlaues.
    for(i in 1:3) {
      rgs_fisher_cat_csv <- .create_file_nm(output_dir, "rgs_fisher_cat%d.csv", i)
      .worm_cat_acceptable_pvalues(rgs_fisher_cat_csv)

      rgs_fisher_cat_apv_csv <- .create_file_nm(output_dir, "rgs_fisher_cat%d_apv.csv", i)

      plot_title <- paste(title, sprintf("category%d",i), sep=":")
      .worm_cat_bubble_plot(rgs_fisher_cat_apv_csv, plot_title)
    }

    # Capture basic information about this run write output to run_data.txt
    run_data <- .create_file_nm(output_dir, "run_data.txt")
    runtime_l <- paste("runtime",format(Sys.time(), "%b-%d-%Y-%H:%M:%S"), sep=": ")
    annotation_file_l <- paste("annotation_version",annotation_file, sep=": ")
    input_type_l <- paste("input_type",input_type, sep=": ")

    cat(runtime_l, annotation_file_l, input_type_l, file=run_data, sep="\n", append=TRUE)

    # Create a zip file as the final output
    if(zip_files) {
       files2zip <- dir(output_dir, full.names = TRUE)
       zip(zipfile = output_dir, files = files2zip)
    }

    if(rm_dir == TRUE){
       message("Cleaning up the working directory")
       unlink(output_dir, TRUE)
    }
    print("Wormcat Execution completed!")
}

.get_system_path_separator <- function() {
  separator <- file.path("dummy", "file")
  # Extract the separator from the generated path
  separator <- substr(separator, nchar("dummy") + 1, nchar(separator) - nchar("file"))
  return(separator)
}

.create_file_nm <- function(output_dir, name_format, n=0){
  if(n==0){
    file_nm = name_format
  } else {
    file_nm <- sprintf(name_format, n)
  }
  file_path <- file.path(output_dir, file_nm)
  return(file_path)
}


#' Worm Cat Function
#'
#' This function returns a list of available curated annotation file names for use with worm_cat_fun.
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



