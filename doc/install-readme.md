### To install R on AWS EC2 follow the below guidelines:

1. Create a free micro instance
    I'm using Ubuntu Server 16.04 LTS (HVM), SSD Volume Type
    expose ports 22 (SSH), 3389 (RDP, optional), and 8787 (RStudio Server).

    add an elastic ip (in case we need to dump this instance but dont want to change IP's)

2. Start the instance and login with ssh
   * `ssh -i "PRIVATE-key.pem" ubuntu@xxx.xxx.xxx.xxx` (Provide your own permissions file and IP)
   * `sudo apt-get update`
   * `sudo apt-get -y upgrade`

3. Install R
   * `sudo adduser rstudio`
   * `sudo echo "deb https://cloud.r-project.org/bin/linux/ubuntu xenial/" | sudo tee -a /etc/apt/sources.list`
   * `sudo echo "deb http://mirror.math.princeton.edu/pub/ubuntu/ xenial main bionic-backports restricted universe" | sudo tee -a /etc/apt/sources.list`
   * `sudo apt-get update`
   * `sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 51716619E084DAB9`
   * `sudo apt-get update`
   * `sudo apt-get upgrade`
   * `sudo apt-get install libfreetype6-dev`
   * `sudo apt-get install r-base r-base-dev`
   * `sudo apt-get install libssl-dev libcurl4-openssl-dev `
   * `sudo apt-get install libcairo2-dev`

4. Install R Packages
   * `R` --Start R
   * `options("repos" = c(CRAN = "http://cran.rstudio.com/"))`
   * `install.packages(c("argparse","data.table", "knitr", "curl","ggplot2","devtools","git2r","httr","devtools"))`
   * `old.packages()` -- list all packages where an update is available
   * `update.packages()` -- update all available packages

   * `library("devtools")`
   * `install_github("trinker/plotflow")`
   * `install_github("dphiggs01/wormcat")`


5. Install rstudio-server
   * `sudo apt-get install gdebi-core`
   * `wget https://download2.rstudio.org/rstudio-server-1.1.453-amd64.deb`
   * `sudo gdebi rstudio-server-1.1.453-amd64.deb`


6. Stop and Start rstudio-server
   * `sudo rstudio-server status`
   * `sudo rstudio-server stop`
   * `sudo rstudio-server start`

7. test connection
   * visit http://xxx.xxx.xxx.xxx:8787/
