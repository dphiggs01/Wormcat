#rm(list= ls())

#source("worm_cat_add_categories.R")
#source("worm_cat_fisher_test.R")
#source("worm_cat_acceptable_pvalues.R")
#source("worm_cat_bubble_plot.R")

#' Worm Cat Function
#'
#' This function take a regulated gene set and the category annotations and runs a fisher test.
#' @param file_to_process the file to be processes
#' @param title the title for your bubble plots
#' @param rm_dir Boolean If FALSE do not remove temp dir. If TRUE remove temp dir
#' @keywords worm cat
#' @export
#' @examples
#' worm_cat_fun()
worm_cat_fun <- function(file_to_process, title="rgs", rm_dir=FALSE){
    mainDir <- getwd()
    subDir <- paste("worm-cat_",format(Sys.time(), "%b-%d-%Y-%H:%M:%S"), sep="")
    subDirPath <- paste("./",subDir, sep="")
    dir.create(file.path(mainDir, subDir))

    #worm_cat_annotations <- "annotations_jul-09-2018.csv"
    worm_cat_annotations <- system.file("extdata", "annotations_jul-09-2018.csv", package="wormcat")

    .worm_cat_add_categories(file_to_process, subDirPath, worm_cat_annotations)

    .worm_cat_fisher_test(subDirPath, worm_cat_annotations)

    # For each of the three files made above:
    # 1. Parse the file to only include the entries with "acceptable pvalues"
    # 2. Create bubble plots for each of the three categories based on the accepteble pvlaues
    for(i in 1:3) {

      cat_file_to_process <- sprintf("./%s/rgs_fisher_cat%d.csv", subDir, i)
      print(sprintf("Processed %s",cat_file_to_process))
      .worm_cat_acceptable_pvalues(cat_file_to_process)

      cat_pvalue_file_to_process <- sprintf("./%s/rgs_fisher_cat%d_apv.csv",subDir, i)

      plot_titles <- c(paste(title, "category1", sep=":"),
                       paste(title, "category2", sep=":"),
                       paste(title, "category3", sep=":"))
      .worm_cat_bubble_plot(cat_pvalue_file_to_process, plot_titles[i])
    }

    files2zip <- dir(subDirPath, full.names = TRUE)
    zip(zipfile = subDir, files = files2zip)
    if(rm_dir == TRUE){
      print("cleaning up")
       unlink(subDir, TRUE)
    }
}




