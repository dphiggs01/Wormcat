library(ggplot2)
library(plotflow)
library(scales)
library(ggthemes)
library(pander)
library(svglite)

#RGS-Regulated Gene Sets

#3.	Run bubble chart
#Will need to select categories (only p values  < 0.05 or only metabolism)
#Will need to match file name/category name in script
#Will need to add calibrations for high and low (see arf_cat2UP.csv) so that colors and sizes stay consistent between graphs

.worm_cat_bubble_plot <- function(csv_file_name, plot_title) {

  dev.list()

  bubbles  <- read.csv(csv_file_name, header=TRUE, sep =",")

  bubbles <- na.omit(bubbles)

  bubbles <- rbind(bubbles, data.frame(Category = "calibration high", RGS = 250, AC = 0, PValue = 1.00E-50))
  bubbles <- rbind(bubbles, data.frame(Category = "calibration low", RGS = 1, AC = 0, PValue = 1))


  bubbles$bubbles_z <- round(0.001 * (bubbles$PValue - mean(bubbles$PValue))/sd(bubbles$PValue), 2)  # compute normalized value as a placeholder

  bubbles$p_value_type <- ifelse(bubbles$PValue < 1e-40, "Col1",
                                         ifelse(bubbles$PValue < 1e-20, "Col2",
                                               ifelse(bubbles$PValue < 1e-10, "Col3",
                                                       ifelse(bubbles$PValue < 1e-5, "Col4",
                                                              ifelse(bubbles$PValue < 1e-2, "Col5",
                                                                     ifelse(bubbles$PValue < 5e-2, "Col6","NS"))))))


  bubbles$RGS_size <- ifelse(bubbles$RGS > 150, "Size1",
                                               ifelse(bubbles$RGS > 100, "Size2",
                                                      ifelse(bubbles$RGS > 75, "Size3",
                                                             ifelse(bubbles$RGS > 50, "Size4",
                                                                    ifelse(bubbles$RGS > 25, "Size5",
                                                                           ifelse(bubbles$RGS > 10, "Size6",
                                                                                  ifelse(bubbles$RGS > 5, "Size7",
                                                                                      ifelse(bubbles$RGS > 2, "Size8","Size9"))))))))


  bubbles_plot <- bubbles[order(bubbles$PValue), ]  # sort

  # Plot
  myplot <- ggplot(reorder_by(Category, ~ -PValue, bubbles_plot), aes(x=`Category`, y=bubbles_z, label=bubbles_z, size = bubbles_plot$RGS_size)) +

    geom_point(stat='identity', aes(col=bubbles_plot$p_value_type))  +

    scale_color_manual(name="P value",
                       labels = c("10-40", "10-20", "10-10", "10-5", "0.001", "0.05", "NS"),
                       values = c("Col1"="red4", "Col2" = "orangered3", "Col3" = "orangered1",
                                  "Col4" = "orange", "Col5" = "gold", "Col6" = "yellow", "NS"="grey48")) +
    scale_size_manual(name="Count",
                      labels = c( "150", "100", "75", "50", "25", "10","5", "2", "Zero"),
                      values = c( "Size1" = 10, "Size2" = 9,
                                 "Size3" = 8, "Size4" = 7, "Size5" = 6,"Size6" = 5, "Size7" = 2.5, "Size8" = 1,"Size9"=0.1)) +


    labs(title=plot_title) +
    ylim(-1.0, 1.0) +
    coord_flip()


  myplot + theme(panel.grid = element_blank(), panel.background = element_blank(), legend.key = element_rect(fill = "white", colour = "white"))

  s_from <- 0
  s_to <- nchar(csv_file_name)-4
  file_out_name <- sprintf("%s.svg",substr(csv_file_name, s_from, s_to))

  #ggsave(file_out_name, width = 5.5, height = 5, useDingbats=F)
  ggsave(file_out_name, width = 5.5, height = 5)

  print(myplot)

  dev.off()
}



