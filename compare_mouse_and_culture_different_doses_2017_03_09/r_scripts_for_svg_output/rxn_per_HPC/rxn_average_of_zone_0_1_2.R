
# Name: Yanli Xu
# Date: Mar 13th, 2017
# Note: This script is to combine the three csv files from Glen'r rxn.r


combine_rxn <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Ask user to input the name
  exp_str <- readline("Enter a name to represent this experiment: ")
  exp_name <- unlist(strsplit(exp_str, "[, ]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern = "rxnProduct_zone", recursive = T)
  
  Time <- c()
  total_S <- c()
  total_nMD <- c()
  total_MitoDD <- c()
  total_oAPAP <- c()
  total_G <- c()
  total_marker <- c()
  tatal_repair <- c()
  tatal_N <- c()
  total_GSH_Depletion <- c()
  
  
  for (i in 1: length(files))
  {
    if(i == 1)
    {
      data <- read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ",")
      Time <- data[, 1]
      total_S <- data[, 2]
      total_nMD <- data[, 3]
      total_MitoDD <- data[, 4]
      total_oAPAP <- data[, 5]
      total_G <- data[, 6]
      total_marker <- data[, 7]
      tatal_repair <- data[, 8]
      tatal_N <- data[, 9]
      total_GSH_Depletion <- data[, 10]
    }
    else
    {
      data <- read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ",")
      total_S <- total_S + data[, 2]
      total_nMD <- total_nMD + data[, 3]
      total_MitoDD <- total_MitoDD + data[, 4]
      total_oAPAP <- total_oAPAP + data[, 5]
      total_G <- total_G + data[, 6]
      total_marker <- total_marker + data[, 7]
      tatal_repair <- tatal_repair + data[, 8]
      tatal_N <- tatal_N + data[, 9]
      total_GSH_Depletion <- total_GSH_Depletion + data[, 10]
    }
  }
  
  df <- data.frame(Time, total_S * (1/3), total_nMD * (1/3), total_MitoDD * (1/3), total_oAPAP * (1/3), total_G * (1/3), total_marker * (1/3), tatal_repair * (1/3), tatal_N * (1/3), total_GSH_Depletion * (1/3))
  names(df) <- c("Time", "S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N", "GSH_Depletion")
  write.csv(df, paste(exp_name, "rxn_per_HPC.csv", sep = "_"), row.names = F)
  

}
combine_rxn()