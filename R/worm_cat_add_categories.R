# Merge the annotation file with the file to process

.worm_cat_add_categories <- function(file_to_process, out_dir, worm_cat_annotations, input_type){

    # Read in the file to process
    file_to_process  <- read.csv(file_to_process, header=TRUE, sep =",")

    # Read in the annotation file
    annotations  <- read.csv(worm_cat_annotations, header=TRUE, sep =",")

    # Remove duplicates from file_to_process
    file_to_process_df <- data.frame(unique(file_to_process))

    # Remove duplicates from annotations (there really should not be any)
    annotations_df <- annotations[!duplicated(annotations[,input_type]), ]

    # Remove other columns from annotations (there really should not be any)
    annotations_df <- annotations_df[c(1,2,3,4,5,6)]

    # Combine the file_to_process with the annotation file
    rgs_merge <-  merge(file_to_process_df, annotations_df, by = input_type, all.x = TRUE)

    # Save merged data as CSV file
    write.csv(rgs_merge, file = paste(out_dir,"/rgs_and_categories.csv", sep=""))

}

