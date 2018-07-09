

.worm_cat_acceptable_pvalues <- function(csv_file_name) {

  acceptable_pvalues  <- read.csv(csv_file_name, row.names=1, header=TRUE, sep =",")
  acceptable_pvalues <- na.omit(acceptable_pvalues)

  acceptable_pvalues[order(acceptable_pvalues$PValue),]

  acceptable_pvalues <- subset(acceptable_pvalues, PValue < 0.05)

  acceptable_pvalues <- rbind(acceptable_pvalues, data.frame(Category = "calibration high", RGS = 250, AC = 0, PValue = 1.00E-50))
  acceptable_pvalues <- rbind(acceptable_pvalues, data.frame(Category = "calibration low", RGS = 1, AC = 0, PValue = 1))

  csv_out_file_nm <-sprintf("%s_apv.csv",substr(csv_file_name, 0, nchar(csv_file_name)-4))
  write.csv(acceptable_pvalues, file = csv_out_file_nm,row.names=FALSE)
}



