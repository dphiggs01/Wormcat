# Analysis of genome-scale data with WormCat identifies novel enriched gene categories in studies from metabolic, tissue-specific, and lifespan-drug data

#### Authors: Amy Holdorf, Daniel Higgins, Anne Hart, Peter Boag, Gregory Pazour, Marian Walhout,and Amy Walker

[GENETICS February 1, 2020 vol. 214 no. 2 279-294;](https://www.genetics.org/content/214/2/279)


## Abstract
The emergence of large sets of gene regulation data has revealed the need for improved tools to 1) identify enriched functional gene categories and 2) visualize enrichment patterns across comparative datasets.  Gene ontogeny enrichment (GO) has several limitations for C. elegans analysis. First, around 30% of C. elegans genes are not represented in commonly used search engines. Second, it is difficult to compare multiple GO analyses. To allow visualization and categorization of C. elegans gene sets, we have developed a web-based tool, WormCat.  This tool uses a near complete annotation of C. elegans genes to determine category enrichment and define potential co-regulated or co-functioning gene sets. Then WormCat provides a scaled heat map for visualization along with enrichment statistics and annotation of each input gene. We have developed an annotation strategy based on a nested category approach where each gene is annotated at three levels.  Enrichment scores are generated at each level, allowing both broad (Cat1) and more detailed analysis (Cat2, Cat3).  Using WormCat on published RNA seq datasets from metabolic, tissue-specific or after treatment with lifespan-increasing drugs, we show that WormCat finds major categories appearing in GO searches and also identifies additional enriched categories that are informative for interpreting phenotypes or predicting biological function.  Thus, WormCat is a powerful tool that will allow a sophisticated analysis of gene enrichment in different types of C. elegans datasets.

## Overview Wormcat
Wormcat is also available as an online tool at [www.wormcat.com](http://www.wormcat.com); the online version greatly simplifies the use of Wormcat and is maintained by the [Walker Lab at UMASS Medical School](http://www.amywalkerlab.com/).

###### The diagram below shows the flow of the Wormcat process:
<img src="./images/WormCat-Flow.png" alt="Flow" width="700"/>

###### The diagrams below shows sample output from a Wormcat.com run:
<img src="./images/results_screen.png" alt="Results" width="700"/>


##### Starburst view of categorical data
<img src="./images/sunburst.png" alt="starburst" width="400"/>

## Installing wormcat package

If you choose to install Wormcat locally, please follow the below instructions:

* Wormcat requires **R 4.1.0** or greater and can be downloaded from [here](https://www.r-project.org/)

* **devtools** package is also required for installation of Wormcat. You can install devtools package from RStudio or the R commandline with the following command. `install.packages("devtools")`

* Next load the **devtools** library with `library("devtools")`

* Install the **plotflow** dependency with `install_github("trinker/plotflow")`

* Install **wormcat** with `install_github(“dphiggs01/wormcat")`  

* If you are using **wormcat-batch** you will also need **argparse** which can be installed with `install.packages(“argparse")`


Condensed instructions:

```bash
R -e "install.packages(c('devtools'))"
R -e "install.packages(c('plyr','data.table','pander','ggthemes','ggplot2','scales','argparse','svglite'))"
R -e "install.packages(c('BiocManager'))"

R -e "library(BiocManager); BiocManager::install(c('FSA'))"
R -e "library('devtools'); install_github('trinker/plotflow')"
R -e "library('devtools'); install_github('dphiggs01/wormcat')"
```


Note: for (Linux) Ubuntu source code install 

```bash
apt-get update -q
apt-get install -y --no-install-recommends libxml2-dev libssl-dev libcurl4-openssl-dev libfontconfig1-dev libcairo2-dev libicu-dev libz-dev unzip zip liblzma-dev libbz2-dev libglpk-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev libgit2-dev cmake
```



