#library(FSA)
library("stats")
.worm_cat_acceptable_pvalues <- function(rgs_fisher_cat_csv) {

    rgs_fisher_cat  <- read.csv(rgs_fisher_cat_csv, row.names = 1, header = TRUE, sep = ",")
    rgs_fisher_cat <- na.omit(rgs_fisher_cat)

    rgs_fisher_cat[order(rgs_fisher_cat$PValue), ]

    rgs_fisher_cat$Bonferroni <- p.adjust(rgs_fisher_cat$PValue, method = "bonferroni")
    ### Acceptable is to be 0.01 on Bonferroni
    # rgs_fisher_cat <- subset(rgs_fisher_cat, PValue < 0.05)
    rgs_fisher_cat <- subset(rgs_fisher_cat, Bonferroni < 0.01)

    csv_out_file_nm <- sprintf("%s_apv.csv", substr(rgs_fisher_cat_csv, 0, nchar(rgs_fisher_cat_csv) - 4))
    write.csv(rgs_fisher_cat, file = csv_out_file_nm, row.names = FALSE)
}
