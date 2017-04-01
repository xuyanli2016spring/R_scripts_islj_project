# Name: Yanli Xu
# Date: Feb 2nd, 2017
# This script is for processing the rxnProduct.csv from Glen: separate the rxnProduct_zone_1_2.csv to rxnProduct_zone_0.csv and rxnProduct_zone_1.csv
# How to use this file: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("rxn_processing.R"), the script will start working 


rxn_process <- function()
{
  
  # Ask user to input the working directory
  wd_str <- readline(prompt="Please input the working directory: ")
  setwd(unlist(strsplit(wd_str, "[ ]+")))
  
  # read the files
  files <- list.files(pattern = "rxnProduct_zone_1_2-[0-9]+.csv")
  #print(length(files))
  for(i in 1:length(files))
  {
    data <- read.csv(files[i], header = TRUE, sep = ",")
    # Zone 0
    data_0 <- data[, !grepl("^X1", names(data))]
    write.csv(data_0, file = paste("rxnProduct_zone_0-", sprintf("%04d", (i-1)), ".csv", sep = ""), row.names=FALSE)
    # Zone 1
    data_1 <- data[, !grepl("^X0", names(data))]
    write.csv(data_1, file = paste("rxnProduct_zone_1-", sprintf("%04d", (i-1)), ".csv", sep = ""), row.names=FALSE)

  }
}
rxn_process()




