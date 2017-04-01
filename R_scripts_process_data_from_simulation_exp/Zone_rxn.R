# Name: Yanli Xu
# Date: Written on Nov 9th and updated on Feb 3rd, 2017
# Input n csv file: rxnProduct_zone_Y-XXXX.csv.gz files, here Y is the zone number, the value can be 0, 1, 2
# Output 3 csv files: rxn_Zone_Y.csv
# How to use the script: Go to the directory that includes the rxnProduct_zone_Y-XXXX.csv.gz files type command: Rscript Zone_rxn.R XXXX-XX-XX-XXXX-XXXX
########################################### Preparation
#! /usr/bin/Rscript 
argv <- commandArgs(TRUE)
if (length(argv) != 1) 
{
  print("Usage: Rscript Zone_rxn.R <exp directories>")
  print("Please input one directory.")
  quit()
}
if (!dir.exists(argv))
{
  print("Usage: Rscript Zone_rxn.R <exp directories>")
  print("Please make sure the input is a directory.")
  quit()
}
#################################################
avgPerZone <- function(dir, zone_number)
{
  rxn_files <- list.files(path = dir, pattern = paste("rxnProduct_zone_", zone_number, "-[0-9]+.csv.gz", sep = ""))
  ###################################### Input n "rxnProduct_zone_X-XXXX.csv.gz" files and output n "Zone_X_temp_file_X.csv" files
  for (i in 1:length(rxn_files))
  {
    # Read a file
#     exp_names[i] <- paste("rxn_", i, sep = "")
#     assign(exp_names[i], read.csv(file = paste(dir, rxn_files[i], sep= "/"), header = TRUE, sep = ","))
#     # change string to object
#     exp <- eval(parse(text = exp_names[i]))
    
    exp <- read.csv(file = paste(dir, rxn_files[i], sep= "/"), header = TRUE, sep = ",")
    
    # initiate the variables with all 0s
    Total_of_S <- rep(0, nrow(exp))
    Total_of_nMD <- rep(0, nrow(exp))
    Total_of_MitoDD <- rep(0, nrow(exp))
    Total_of_oAPAP <- rep(0, nrow(exp))
    Total_of_G <- rep(0, nrow(exp))
    Total_of_marker <- rep(0, nrow(exp))
    Total_of_Repair <- rep(0, nrow(exp))
    Total_of_N <- rep(0, nrow(exp))
    Total_of_GSH_Depletion <- rep(0, nrow(exp))
    # The first column is time, so start from the second line, average all the columns which have the same variable names
    for(j in 2:length(exp)) {
      if( (j - 1) %% 9 == 1){
        Total_of_S <- Total_of_S + exp[, j]
      }else if((j - 1) %% 9 == 2) {
        Total_of_nMD <- Total_of_nMD + exp[, j] 
      }else if((j - 1) %% 9 == 3) {
        Total_of_MitoDD <- Total_of_MitoDD +  exp[, j] 
      }else if((j - 1) %% 9 == 4) {
        Total_of_oAPAP <-  Total_of_oAPAP +  exp[, j] 
      }else if((j - 1) %% 9 == 5) {
        Total_of_G <- Total_of_G + exp[, j]
      }else if((j - 1) %% 9 == 6) {
        Total_of_marker <- Total_of_marker + exp[, j]   
      } else if((j - 1) %% 9 == 7) {
        Total_of_Repair <- Total_of_Repair + exp[, j] 
      } else if((j - 1) %% 9 == 8)  {
        Total_of_N <- Total_of_N + exp[, j]  
      }else if((j - 1) %% 9 == 0) {
        Total_of_GSH_Depletion <- Total_of_GSH_Depletion + exp[, j] }
    }
    num_of_node <- (length(exp) - 1)/9
    df <- data.frame(exp[, 1], Total_of_S/num_of_node, Total_of_nMD/num_of_node, Total_of_MitoDD/num_of_node, Total_of_oAPAP/num_of_node, Total_of_G/num_of_node, Total_of_marker/num_of_node, Total_of_Repair/num_of_node, Total_of_N/num_of_node, Total_of_GSH_Depletion/num_of_node )
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
  ###############################################
  # initialize the data with the first file, since the final result need to average among all the mc trials,
  # therefore the initialization is the data from first file devided by the number of MC trials
  data <- eval(parse(text = exp_names[1]))
  
  Time <- data[1]
  S <- data[2]/length(exp_names)
  nMD <- data[3]/length(exp_names)
  MitoDD <- data[4]/length(exp_names)
  oAPAP <- data[5]/length(exp_names)
  G <- data[6]/length(exp_names)
  Marker <- data[7]/length(exp_names)
  Repair <- data[8]/length(exp_names)
  N <- data[9]/length(exp_names)
  GSH_Depletion <- data[10]/length(exp_names)
  
  # Here i starts from 2 because the first file is already used to initialize the data
  for(i in 2:length(exp_names))
  {
    data <- eval(parse(text = exp_names[i]))
    
    S <- S + data[2]/length(exp_names)
    nMD <- nMD + data[3]/length(exp_names)
    MitoDD <- MitoDD + data[4]/length(exp_names)
    oAPAP <- oAPAP + data[5]/length(exp_names)
    G <- G + data[6]/length(exp_names)
    Marker <- Marker + data[7]/length(exp_names)
    Repair <- Repair + data[8]/length(exp_names)
    N <- N + data[9]/length(exp_names)
    GSH_Depletion <- GSH_Depletion + data[10]/length(exp_names)
    
  }
  # "Time", "S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N", "GSH_Depletion"
  df <- data.frame(Time, S, nMD, MitoDD, oAPAP, G, Marker, Repair, N, GSH_Depletion)
  names(df) <- c("Time", "S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N", "GSH_Depletion")
  write.table(df, paste(dir, "_rxn_Zone_", zone_number, ".csv", sep = ""), row.names=FALSE, sep = ",")
  
  
  
#   # rxn_df is a data.frame which is used to save the final rxn_Zone_X data
#   rxn_df <- data.frame(matrix(ncol = length(eval(parse(text = exp_names[1]))), nrow = nrow(eval(parse(text = exp_names[1])))))
#   # For every variable in the dataset, average the data from all the files
#   for(var_num in 1:length(eval(parse(text = exp_names[1]))))
#   {
#     # Get the data from the first file, if only one file, then total is only from the first file
#     # if there are 2 or more files, then total need to add all the data from the other files
#     total <- (eval(parse(text = exp_names[1])))[, var_num]
#     if(length(exp_names) >= 2)
#     {
#       for (mc_num in 2:length(exp_names))
#       {
#         total <- total + (eval(parse(text = exp_names[mc_num])))[, var_num]
#       }
#     }
#     rxn_df[, var_num] <- total/(length(exp_names))
#     
#   }
#   # Add names to the data frame and write to rxn_Zone_X.csv file
#   names_df <- c("Time", "S", "nMD", "MitoDD", "oAPAP", "G", "Marker", "Repair", "N", "GSH_Depletion")
#   for(i in 1:length(names_df))
#   {
#     names_df[i] = paste("Z.", zone_number, ".", names_df[i], sep = "")
#   }
#   names(rxn_df) <- names_df
#   write.csv(rxn_df, paste(dir, "_rxn_Zone_", zone_number, ".csv", sep = ""), row.names=FALSE)
}

for(i in 0:2)
{
  avgPerZone(argv, i)
}

print(paste(argv, " rxn data is finished!", sep = ""))
################################### End

