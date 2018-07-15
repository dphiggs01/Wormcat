# worm_cat

### Installing wormcat package
    install.packages("devtools")
    library("devtools")
    devtools::install_github("dphiggs01/wormcat")
    library(wormcat)

### Redploy wormcat package
#### 1. Update the documenation
    library("devtools")
    setwd("./wormcat")
    document()

#### 2. Install package
    setwd("..")
    install("wormcat")
