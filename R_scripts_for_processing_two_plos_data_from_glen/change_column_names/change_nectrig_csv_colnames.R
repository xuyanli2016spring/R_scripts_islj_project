# Name: Yanli
# Date: Feb 1st, 2017
# This script is for changing the column names of "nectrig.csv"
# How to use this file: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("change_nectrig_csv_colnames.R"), the script will start working 


change_names <- function()
{
  
  # Ask user to input the working directory
  wd_str <- readline(prompt="Please input the working directory: ")
  setwd(unlist(strsplit(wd_str, "[ ]+")))

  # read the files
  files <- list.files(pattern = "nectrig-[0-9]+.csv")
  #print(length(files))
  for(i in 1:length(files))
  {
    data <- read.csv(files[i], header = TRUE, sep = ",")
    
    names(data)[1] <- "Time_of_NecTrig"
    print(head(data))
    file.remove(files[i])

    write.csv(data, file = files[i], row.names=FALSE)
    #print(i)
    
  }
  setwd("/Users/yanlixu/Desktop/R_scripts_yanli/change_column_names")
}
change_names()





# wd_str <- readline(prompt="Please input the working directory: ")
# setwd(unlist(strsplit(wd_str, "[ ]+")))














