# Goal: generate p values for enrichment of annotation terms in RNA seq data
# Step 1: Count categories in annotation files *AC*, will stay static
# Step 2: Count categories in regulated gene sets *RGS*, make this easy to switch in alternate data sets, *RGS_a, b, c, etc.*
# Step 3: Generate data frame: AC and RGS
# Step 4: Build contingency table for each category in RGS vs AC, use a for loop to build the contingency tables.
# Step 5: Use Fisher.test to generate P value for enrichment of specific categories in RGS

library("data.table")
library("plyr")

.worm_cat_fisher_test <- function(output_dir, annotations_csv){
  # Read in annotations file
  annotations <- read.csv(annotations_csv,header = TRUE, sep = ",")

  # Read in rgs mapped to annotations
  rgs_and_categories_csv <- paste(output_dir,"rgs_and_categories.csv", sep="/")
  regulated_gene_set <- read.csv(rgs_and_categories_csv,header = TRUE, sep = ",")

  # Step 1 Count categories in annotation files *AC*, will stay static

  total_annotations_count <- data.frame(nrow(annotations))

  annotated_cat1 <- data.frame(table(annotations$Category.1))
  annotated_cat2 <- data.frame(table(annotations$Category.2))
  annotated_cat3 <- data.frame(table(annotations$Category.3))

  # Step 2/3: Count categories in regulated gene sets *RGS*

  total_rgs_count <- data.frame(nrow(regulated_gene_set))

  rgs_annotated_cat1 <- data.frame(table(regulated_gene_set$Category.1))
  rgs_annotated_cat2 <- data.frame(table(regulated_gene_set$Category.2))
  rgs_annotated_cat3 <- data.frame(table(regulated_gene_set$Category.3))

  .merger_cats(rgs_annotated_cat1, annotated_cat1, total_annotations_count$nrow, total_rgs_count$nrow, .out_file_nm(output_dir,1))
  .merger_cats(rgs_annotated_cat2, annotated_cat2, total_annotations_count$nrow, total_rgs_count$nrow, .out_file_nm(output_dir,2))
  .merger_cats(rgs_annotated_cat3, annotated_cat3, total_annotations_count$nrow, total_rgs_count$nrow, .out_file_nm(output_dir,3))
}

###########################
# Step 4: Merge data frames
.merger_cats <- function(rgs_annotated_cat, annotated_cat, total_annotations_count, total_rgs_count, file_nm) {

  merged_cats <- merge(rgs_annotated_cat, annotated_cat, by = "Var1", all.x = TRUE)

  merged_cats <- plyr::rename(merged_cats, c("Var1" = "Category", "Freq.x" = "RGS", "Freq.y" = "AC" ))

  # Step 5: Build contingency table for each category in RGS vs AC

  # con <- file("test.log")
  # sink(con, append=TRUE)
  # sink(con, append=TRUE, type="message")

  df <- data.frame(Category=character(),
                   RGS=double(),
                   AC=double(),
                   PValue=double(),
                   stringsAsFactors=FALSE)

  fact_character <- levels(merged_cats$Category)[as.numeric(merged_cats$Category)]

  for(i in 1:nrow(merged_cats)) {
    if(is.na(merged_cats$RGS[i]) | is.na(merged_cats$AC[i])){
      pvalue <- NA
    }else{
      stat <- fisher.test(matrix(c(merged_cats$RGS[i], total_rgs_count,
                                   merged_cats$AC[i],  total_annotations_count),
                                 nrow=2, ncol=2),
                          alternative="greater")
      pvalue <- stat$p.value
    }

    df[nrow(df) + 1,] = list(Category=fact_character[i],
                             RGS=merged_cats$RGS[i],
                             AC=merged_cats$AC[i],pvalue)
  }

  sorted_df <- df[with(df, order(PValue)),]
  write.csv(sorted_df, file = file_nm)
}

.out_file_nm <- function(output_dir,n){
  sprintf("%s/rgs_fisher_cat%d.csv",output_dir, n)
}




