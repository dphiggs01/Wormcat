# Goal: generate p values for enrichment of annotation terms in RNA seq data
# Step 1: Count categories in annotation files *AC*, will stay static
# Step 2: Count categories in regulated gene sets *RGS*, make this easy to switch in alternate data sets, *RGS_a, b, c, etc.*
# Step 3: Generate data frame: AC and RGS
# Step 4: Build contengency table for each category in RGS vs AC, use a for loop to build the contengency tables.
# Step 5: Use Fisher.test to generate P value for enrichment of specific categories in RGS

library("data.table")
library("plyr")

.worm_cat_fisher_test <- function(output_dir, worm_cat_annotations){
  # read in  csv files, create data frames
  AC <- read.csv(worm_cat_annotations,header = TRUE, sep = ",")

  csv_file <- paste(output_dir,"/rgs_and_categories.csv", sep="")
  RGS <- read.csv(csv_file,header = TRUE, sep = ",")

  # Step 1 Count categories in annotation files *AC*, will stay static

  total_count <- data.frame(nrow(AC))

  total_nrow <- rename(total_count, c("nrow.AC." = "nrow"))

  #print(total_nrow)
  #print(total_nrow$nrow)
  #print(total_annotated)

  total_annotated_cat1 <- data.frame(table(AC$Category.1))

  total_annotated_cat2 <- data.frame(table(AC$Category.2))

  total_annotated_cat3 <- data.frame(table(AC$Category.3))

  # Step 2/3: Count categories in regulated gene sets *RGS*

  RGS_count <- data.frame(nrow(RGS))

  #print(RGS_count)

  RGS_nrow <- rename(RGS_count, c("nrow.RGS." = "nrow"))

  #print(RGS_nrow)
  #print(RGS_nrow$nrow)

  RGS_annotated_cat1 <- data.frame(table(RGS$Category.1))

  RGS_annotated_cat2 <- data.frame(table(RGS$Category.2))

  RGS_annotated_cat3 <- data.frame(table(RGS$Category.3))

  .merger_cats(RGS_annotated_cat1, total_annotated_cat1,total_nrow$nrow, RGS_nrow$nrow, .out_file_nm(output_dir,1))
  .merger_cats(RGS_annotated_cat2, total_annotated_cat2,total_nrow$nrow, RGS_nrow$nrow, .out_file_nm(output_dir,2))
  .merger_cats(RGS_annotated_cat3, total_annotated_cat3,total_nrow$nrow, RGS_nrow$nrow, .out_file_nm(output_dir,3))
}

###########################
# Step 4: Merge data frames
.merger_cats <- function(UP_annotated_cat, total_annotated_cat,total_all_cat, total_rgs_cat, file_nm) {

  cat_a <- merge(UP_annotated_cat, total_annotated_cat, by = "Var1", all.x = TRUE)

  #print(head(cat_a))

  cat_b <- rename(cat_a, c("Var1" = "Category", "Freq.x" = "RGS", "Freq.y" = "AC" ))

  # Step 5: Build contengency table for each category in RGS vs AC

  #"a =cat_b$Category",

  #"x = cat_b$RGS",

  #"y = cat_b$AC"

  # con <- file("test.log")
  #  con2 <- file("/Users/danhiggins/Code/R_Workspace/Fisher-Test/test1.log")
  #  sink(con, append=TRUE)
  #  sink(con, append=TRUE, type="message")

  df <- data.frame(Category=character(),
                   RGS=double(),
                   AC=double(),
                   PValue=double(),
                   stringsAsFactors=FALSE)

  fact_character <- levels(cat_b$Category)[as.numeric(cat_b$Category)]

  for(i in 1:nrow(cat_b)) {
    #a <- cat_b$Category[i]
    #print("xxxxxxxxxxx")
    #print(RGS_nrow$nrow)
    #print(total_nrow$nrow)
    #print(cat_b$Category[i])
    #print(total_rgs_cat)
    #print(total_all_cat)
    #print(fact_character[i])
    #print(cat_b$RGS[i])
    #print(cat_b$AC[i])
    if(is.na(cat_b$RGS[i]) | is.na(cat_b$AC[i])){
      pvalue <- NA
    }else{
      stat <- fisher.test(matrix(c(cat_b$RGS[i],total_rgs_cat,
                                   cat_b$AC[i],total_all_cat),nrow=2,ncol=2),alternative="greater")
      pvalue <- stat$p.value
    }
    #print(pvalue)
    #print(fact_character[i])
    #print("xxxxxxxxxxx")

    df[nrow(df) + 1,] = list(Category=fact_character[i],RGS=cat_b$RGS[i], AC=cat_b$AC[i],pvalue)
  }

  sorted_df <- df[with(df, order(PValue)),]
  write.csv(sorted_df, file = file_nm)
}

.out_file_nm <- function(output_dir,n){
  sprintf("%s/rgs_fisher_cat%d.csv",output_dir, n)
}




