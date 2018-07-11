## apply category 3 to regulated gene set then split categories

.worm_cat_add_categories <- function(csv_fnm, out_dir, worm_cat_annotations){

  RGS  <- read.csv(csv_fnm, header=TRUE, sep =",")

  annotations  <- read.csv(worm_cat_annotations, header=TRUE, sep =",")

  #Remove dupliplicates from RGS
  RGS_df <- data.frame(unique(RGS))

  #Remove dupliplicates from annotations
  #annotations_df <- data.frame(unique(annotations$Sequence.ID))
  annotations_df <- annotations[!duplicated(annotations$Sequence.ID), ]

  # remove other columns
  annotations_clean <- annotations_df[c(1,2,3,4,5,7)]

  # merge.x
  RGS_merge <-  merge(RGS_df, annotations_clean, by = "Sequence.ID", all.x = TRUE)

  # create Cat1, Cat2, Cat3 files
  Cat <- RGS_merge[c(3,4,5)]

  # Save csv
  write.csv(Cat, file = paste(out_dir,"/rgs_and_categories.csv", sep=""))
  write.csv(RGS, file = paste(out_dir,"/input_file.csv", sep=""))

}

