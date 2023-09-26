# Merge the annotation file with the file to process

.worm_cat_add_categories <- function(file_to_process, out_dir, worm_cat_annotations, input_type) {
    #log_file <- file("/Users/dan/delme/log.txt", open = "a")
    #message_to_log <- sprintf("Calling .worm_cat_add_categories input_type[%s]",input_type)
    #cat(message_to_log, "\n", file = log_file)
    #close(log_file)

    # Read in the file to process
    file_to_process  <- utils::read.csv(file_to_process, header = TRUE, sep = ",")

    # Read in the annotation file
    annotations  <- utils::read.csv(worm_cat_annotations, header = TRUE, sep = ",")

    # Keep only the first column
    first_column <- file_to_process[, 1]
    file_to_process_df <- data.frame(FirstColumn = first_column)
    colnames(file_to_process_df)[1] <- input_type

    # Remove duplicates from file_to_process
    file_to_process_df <- data.frame(unique(file_to_process_df))

    # Remove duplicates from annotations (there really should not be any)
    # But we have found some custom Annotation files have duplicates
    annotations_df <- annotations[!duplicated(annotations[, input_type]), ]

    # Remove other columns from annotations (there really should not be any)
    # But we have found some custom Annotation files have additional columns
    annotations_df <- annotations_df[c(1, 2, 3, 4, 5, 6)]

    # Combine the file_to_process with the annotation file
    rgs_merge <- merge(file_to_process_df, annotations_df, by = input_type, all.x = TRUE)

    # Save merged data as CSV file
    rgs_and_categories <- file.path(out_dir, "rgs_and_categories.csv")
    utils::write.csv(rgs_merge, file = rgs_and_categories)
}
