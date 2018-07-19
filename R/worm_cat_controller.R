#rm(list= ls())

#source("worm_cat_add_categories.R")
#source("worm_cat_fisher_test.R")
#source("worm_cat_acceptable_pvalues.R")
#source("worm_cat_bubble_plot.R")

#' Worm Cat Function
#'
#' This function takes a regulated gene set and the category annotations and runs a fisher test.
#' @param file_to_process the file to be processes
#' @param title the title for your bubble plots
#' @param output_dir the output directory
#' @param rm_dir Boolean If FALSE do not remove temp dir. If TRUE remove temp dir
#' @param annotation_type 'straight' or 'physiology' the default is straight
#' @param input_type 'Sequence ID' or 'Wormbase ID' the default is Sequence ID
#' @keywords worm cat
#' @export
#' @examples
#' worm_cat_fun()
worm_cat_fun <- function(file_to_process, title="rgs", output_dir=NULL, rm_dir=FALSE, annotation_type="straight", input_type="Sequence.ID"){
    mainDir <- getwd()

    if(is.null(output_dir)){
      output_dir <- paste("worm-cat_",format(Sys.time(), "%b-%d-%Y-%H:%M:%S"), sep="")
    }
    output_dirPath <- paste("./",output_dir, sep="")
    dir.create(file.path(mainDir, output_dir))

    if(annotation_type == "straight"){
      worm_cat_annotations <- "annotations_jul-15-2018.csv"
    }else{
      worm_cat_annotations <- "annotations_jul-18-2018.csv"
    }
    print(paste("worm_cat_annotations=",worm_cat_annotations, sep=""))
    worm_cat_annotations <- system.file("extdata", worm_cat_annotations, package="wormcat")

    .worm_cat_add_categories(file_to_process, output_dirPath, worm_cat_annotations, input_type)

    .worm_cat_fisher_test(output_dirPath, worm_cat_annotations)

    # For each of the three files made above:
    # 1. Parse the file to only include the entries with "acceptable pvalues"
    # 2. Create bubble plots for each of the three categories based on the accepteble pvlaues
    for(i in 1:3) {

      cat_file_to_process <- sprintf("./%s/rgs_fisher_cat%d.csv", output_dir, i)
      print(sprintf("Processed %s",cat_file_to_process))
      .worm_cat_acceptable_pvalues(cat_file_to_process)

      cat_pvalue_file_to_process <- sprintf("./%s/rgs_fisher_cat%d_apv.csv",output_dir, i)

      plot_titles <- c(paste(title, "category1", sep=":"),
                       paste(title, "category2", sep=":"),
                       paste(title, "category3", sep=":"))
      .worm_cat_bubble_plot(cat_pvalue_file_to_process, plot_titles[i])
    }


    files2zip <- dir(output_dirPath, full.names = TRUE)
    zip(zipfile = output_dir, files = files2zip)
    if(rm_dir == TRUE){
      print("cleaning up")
       unlink(output_dir, TRUE)
    }
}




