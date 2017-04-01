# Name: Yanli Xu
# Date: Feb 2nd, 2017
# Input n csv file: rxnProduct_zone_Y-XXXX.csv.gz files, here Y is the zone number, the value can be 0, 1, 2
# Output 3 csv files: rxn_Zone_Y.csv
# How to use the script: Go to the directory that includes the rxnProduct_zone_Y-XXXX.csv.gz files type command: Rscript Zone_rxn.R XXXX-XX-XX-XXXX-XXXX
########################################### Preparation
#! /usr/bin/Rscript 
argv <- commandArgs(TRUE)
if (length(argv) != 1) 
{
  print("Usage: rxn_processing_2.R <exp directories>")
  print("Please input the directory of the exp.")
  quit()
}
if (!dir.exists(argv))
{
  print("Usage: rxn_processing_2.R <exp directories>")
  print("Please make sure the input is a directory.")
  quit()
}
#################################################
avgPerZone <- function(dir, zone_number)
{
  # read all the files in a specific zone 
  rxn_files <- list.files(path = dir, pattern = paste("rxnProduct_zone_", zone_number, "-[0-9]+.csv", sep = ""))
  ###################################### Input n "rxnProduct_zone_X-XXXX.csv.gz" files and output n "Zone_X_temp_file_X.csv" files
  # Process every file by avaraging all the columns of the same solutes
  #exp_names <- c()
  for (i in 1:length(rxn_files))
  {
    # Read a file
    exp <- read.csv(file = paste(dir, rxn_files[i], sep= "/"), header = TRUE, sep = ",")
      
    # initiate the variables with all 0s
    Total_of_NecInhib <- rep(0, nrow(exp))
    Total_of_G <- rep(0, nrow(exp))
    Total_of_S <- rep(0, nrow(exp))
    Total_of_N <- rep(0, nrow(exp))
    Total_of_nMD <- rep(0, nrow(exp))
    Total_of_MitoDD <- rep(0, nrow(exp))
    Total_of_Repair <- rep(0, nrow(exp))
    Total_of_GSH_Depletion <- rep(0, nrow(exp))
    # The first column is time, so start from the second line, average all the columns which have the same variable names
    for(j in 2:length(exp)) {
      if( (j - 1) %% 8 == 1){
        Total_of_NecInhib <- Total_of_NecInhib + exp[, j]
      }else if((j - 1) %% 8 == 2) {
        Total_of_G <- Total_of_G + exp[, j] 
      }else if((j - 1) %% 8 == 3) {
        Total_of_S <- Total_of_S +  exp[, j] 
      }else if((j - 1) %% 8 == 4) {
        Total_of_N <-  Total_of_N +  exp[, j] 
      }else if((j - 1) %% 8 == 5) {
        Total_of_nMD <- Total_of_nMD + exp[, j]
      }else if((j - 1) %% 8 == 6) {
        Total_of_MitoDD <- Total_of_MitoDD + exp[, j]   
      } else if((j - 1) %% 8 == 7) {
        Total_of_Repair <- Total_of_Repair + exp[, j] 
      } else if((j - 1) %% 8 == 0)  {
        Total_of_GSH_Depletion <- Total_of_GSH_Depletion + exp[, j]  
      }
    }
    num_of_node <- (length(exp) - 1)/8
    df <- data.frame(exp[, 1], Total_of_NecInhib/num_of_node, Total_of_G/num_of_node, Total_of_S/num_of_node, Total_of_N/num_of_node, Total_of_nMD/num_of_node, Total_of_MitoDD/num_of_node, Total_of_Repair/num_of_node, Total_of_GSH_Depletion/num_of_node )
    write.csv(df, file = paste("Zone_", zone_number, "_temp_file_", i, ".csv", sep = ""),row.names=FALSE)
  }
  ############################################### Avereage the results of n "Zone_X_temp_file_X.csv" files and ouput one "rxn_Zone_X.csv" file
  # Read all the "Zone_X_temp_file_X.csv" files
  temp_zone_files <- list.files(pattern = paste("Zone_", zone_number, "_temp_file", sep = ""))
  exp_names <- c()
  for (i in 1:length(temp_zone_files))
  {
    exp_names[i] <- paste("zone_", zone_number, "_", i, sep = "")
    assign(exp_names[i], read.csv(temp_zone_files[i], header = TRUE, sep = ","))
    file.remove(temp_zone_files[i])
  }
  
  #############################################
  
  # initialize the data with the first file, since the final result need to average among all the mc trials,
  # therefore the initialization is the data from first file devided by the number of MC trials
  data <- eval(parse(text = exp_names[1]))
  
  Time <- data[1]
  NecInhib <- data[2]/length(exp_names)
  G <- data[3]/length(exp_names)
  S <- data[4]/length(exp_names)
  N <- data[5]/length(exp_names)
  nMD <- data[6]/length(exp_names)
  MitoDD <- data[7]/length(exp_names)
  Repair <- data[8]/length(exp_names)
  GSH_Depletion <- data[9]/length(exp_names)
  
  # Here i starts from 2 because the first file is already used to initialize the data
  for(i in 2:length(exp_names))
  {
    data <- eval(parse(text = exp_names[i]))
    
    NecInhib <- NecInhib + data[2]/length(exp_names)
    G <- G + data[3]/length(exp_names)
    S <- S + data[4]/length(exp_names)
    N <- N + data[5]/length(exp_names)
    nMD <- nMD + data[6]/length(exp_names)
    MitoDD <- MitoDD + data[7]/length(exp_names)
    Repair <- Repair + data[8]/length(exp_names)
    GSH_Depletion <- GSH_Depletion + data[9]/length(exp_names)

  }
  
  df <- data.frame(Time, NecInhib, G, S, N, nMD, MitoDD, Repair, GSH_Depletion)
  
  names(df) <- c("Time", paste("Z.", zone_number, ".NecInhib", sep = ""), paste("Z.", zone_number, ".G", sep = ""), paste("Z.", zone_number, ".S", sep = ""), paste("Z.", zone_number, ".N", sep = ""), paste("Z.", zone_number, ".nMD", sep = ""), paste("Z.", zone_number, ".MitoDD", sep = ""), paste("Z.", zone_number, ".Repair", sep = ""), paste("Z.", zone_number, ".GSH_Depletion", sep = ""))
  write.table(df, paste(dir, "_rxnProduct_zone-", zone_number, ".csv", sep = ""), row.names=FALSE, sep = ",")

  #############################################
}

for(i in 0:2)
{
  avgPerZone(argv, i)
}

print(paste(argv, " rxn data is finished!", sep = ""))
################################### End

