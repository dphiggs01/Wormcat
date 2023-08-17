#' @import ggplot2
#' @import ggthemes
#' @import svglite
#'
library(ggplot2)
library(ggthemes)
#library(plotflow)


# RGS-Regulated Gene Sets

# 3.	Run bubble chart
# Will need to select categories (only p values  < 0.05 or only metabolism)
# Will need to match file name/category name in script
# Will need to add calibrations for high and low (see arf_cat2UP.csv) so that colors and sizes stay consistent between graphs

.worm_cat_bubble_plot <- function(csv_file_name, plot_title) {
    Category <- NULL
    bubbles_z <- NULL

    bubbles <- utils::read.csv(csv_file_name, header = TRUE, sep = ",")

    bubbles <- na.omit(bubbles)

    bubbles <- rbind(bubbles, data.frame("Category" = "calibration high", "RGS" = 250, "AC" = 0, "PValue" = 1.00E-50, "Bonferroni" = 1.00E-50))
    bubbles <- rbind(bubbles, data.frame("Category" = "calibration low", "RGS" = 1, "AC" = 0, "PValue" = 1, "Bonferroni" = 1))


    bubbles$bubbles_z <- round(0.001 * (bubbles$PValue - mean(bubbles$PValue)) / sd(bubbles$PValue), 2)  # compute normalized value as a placeholder

    bubbles$p_value_type <- ifelse(bubbles$Bonferroni < 1e-40, "Col1",
                                   ifelse(bubbles$Bonferroni < 1e-20, "Col2",
                                        ifelse(bubbles$Bonferroni < 1e-10, "Col3",
                                               ifelse(bubbles$Bonferroni < 1e-5, "Col4",
                                                      ifelse(bubbles$Bonferroni < 1e-2, "Col5",
                                                             ifelse(bubbles$Bonferroni < 5e-2, "Col6", "NS"))))))


    bubbles$RGS_size <- ifelse(bubbles$RGS > 150, "Size1",
                               ifelse(bubbles$RGS > 100, "Size2",
                                    ifelse(bubbles$RGS > 75, "Size3",
                                           ifelse(bubbles$RGS > 50, "Size4",
                                                  ifelse(bubbles$RGS > 25, "Size5",
                                                         ifelse(bubbles$RGS > 10, "Size6",
                                                                ifelse(bubbles$RGS > 5, "Size7",
                                                                       ifelse(bubbles$RGS > 2, "Size8", "Size9"))))))))


    bubbles_plot <- bubbles[order(bubbles$PValue), ]  # sort

    myplot <- ggplot(.reorder_by(Category, ~ -PValue, bubbles_plot),
                     aes(x = Category, y = bubbles_z, size = bubbles_plot$RGS_size)) +

                    geom_point(stat = "identity", aes(col = bubbles_plot$p_value_type))  +

                    scale_color_manual(name = "P value",
                                       labels = c("10-40", "10-20", "10-10", "10-5", "0.001", "0.05", "NS"),
                                       values = c("Col1" = "red4", "Col2" = "orangered3", "Col3" = "orangered1",
                                                  "Col4" = "orange", "Col5" = "gold", "Col6" = "yellow", "NS" = "grey48"),
                                       limits = c("Col1", "Col2", "Col3", "Col4", "Col5", "Col6", "NS")) +
                    scale_size_manual(name = "Count",
                                      labels = c("150", "100", "75", "50", "25", "10", "5", "2", "Zero"),
                                      values = c("Size1" = 10, "Size2" = 9,
                                                 "Size3" = 8, "Size4" = 7, "Size5" = 6, "Size6" = 5, "Size7" = 2.5, "Size8" = 1, "Size9" = 0.1),
                                      limits = c("Size1", "Size2",
                                                 "Size3", "Size4", "Size5", "Size6", "Size7", "Size8", "Size9")
                                      ) +


                    labs(title = plot_title) +
                    ylim(-1.0, 1.0) +
                    coord_flip() +
                    theme(panel.grid = element_blank(),
                          panel.background = element_blank(),
                          legend.key = element_rect(fill = "white", colour = "white"),
                          text = element_text(family = "Arial", size = 14))

    s_from <- 0
    s_to <- nchar(csv_file_name) - 4
    file_out_name <- sprintf("%s.svg", substr(csv_file_name, s_from, s_to))
    ggsave(filename = file_out_name,
           plot = myplot,
           device = "svg",
           width = 6, height = 5.5)
    #message(paste("Plot saved to:", file_out_name))

}



# https://github.com/trinker/plotflow
# Order a Factor by Numeric Variable(s)
#
# Create a new dataframe with a factor reordered (re-leveled) by numeric
# variable(s).
#
.reorder_by <- function(fact, by, data, FUN = NULL, df = TRUE){

  if(by[[1]] != "~") {
    stop("Argument 'by' must be a one-sided formula.")
  }

  x <- data

  form1 <- as.character(substitute(fact))
  form2 <- as.character(by[[2]])
  check1 <- c(length(form2) == 1 && form1 == form2)
  check2 <- c(length(form2) == 2 && form1 == form2[2] && form2[1] == "-")

  if(check1 | check2) {
    if (is.null(FUN)) {
      warning("FUN not provided: `length` assumed")
      FUN <- length
    }
    ord <- match.fun(ifelse(check2, "rev", "c"))
    lvls <- ord(sort(tapply(data[, form1], data[, form1], FUN = FUN)))
    data[, form1] <- factor(data[, form1], levels = names(lvls))
    return(data)
  }


  fact <- as.character(substitute(fact))
  # Make the formula into character and remove spaces
  formc <- as.character(by[2])
  formc <- gsub(" ", "", formc)
  # If the first character is not + or -, add +
  if(!is.element(substring(formc, 1, 1), c("+", "-")))
    formc <- paste("+", formc, sep = "")

  # Extract the variables from the formula
  vars <- unlist(strsplit(formc, "[\\+\\-]"))
  vars <- vars[vars != ""] # Remove any extra "" terms

  ## use for aggregating
  if (!is.null(FUN)) {
    x <- eval(parse(text=paste0("aggregate(cbind(", paste(vars, collapse = ", "), ") ~",
                                fact, ", data = data, FUN = \"", substitute(FUN), "\")")))
  }

  # Build a list of arguments to pass to "order" function
  calllist <- list()
  pos <- 1 # Position of + or -
  for(i in 1:length(vars)){
    varsign <- substring(formc, pos, pos)
    pos <- pos + 1 + nchar(vars[i])
    if(is.factor(x[, vars[i]])){
      if(varsign == "-") {
        calllist[[i]] <- -rank(x[, vars[i]])
      } else {
        calllist[[i]] <- rank(x[, vars[i]])
      }
    } else {
      if(varsign == "-") {
        calllist[[i]] <- -x[, vars[i]]
      } else {
        calllist[[i]] <- x[,vars[i]]
      }
    }
  }
  data[, fact] <- factor(data[, fact], levels = x[do.call("order", calllist), fact])
  if (df) {
    data
  } else {
    data[, fact]
  }
}



