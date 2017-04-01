# Name: Yanli Xu
# Date: Nov 10th, 2016
# Input 6*n csv file: enzymes-XXXX-enzyme_type.csv files, there are 6 enzymes and n monte carlo trails
# Output 3 csv files: mean_enzyme_count_per_zone_XXXXXX.csv
# How to use the script: Go to the directory that includes the enzymes-XXXX-enzyme_type.csv files type command: Rscript enzymes_perZone.R XXXX-XX-XX-XXXX-XXXX
########################################### Preparation
#! /usr/bin/Rscript 
# for every monte carlo enzyme file, sum over all the enzyme in every zone, then average over all the monte carlo files and average over all the nodes
library("gdata") # This library is for function of startsWith()
argv <- commandArgs(TRUE)
if (length(argv) != 1) 
{
  print("Usage: Rscript enzymes_perZone.R <exp directories>")
  print("Please input one directory.")
  quit()
}
if (!dir.exists(argv))
{
  print("Usage: Rscript enzymes_perZone.R <exp directories>")
  print("Please make sure the input is a directory.")
  quit()
}
#################################################
enzymePerZone <- function(dir, enzyme_type)  # Start of the function
{
  enz_files <- list.files(path = dir, pattern = paste("enzymes-[0-9]+-", enzyme_type, ".csv", sep = ""))
  ###################################### Input n "enzymes-[0-9]+-XXXX.csv" files and output n "enzyme_temp_X.csv" files
  exp_names <- c()
  for (i in 1:length(enz_files))
  {
    # Read a file
    exp_names[i] <- paste("enz_", i, sep = "")
    assign(exp_names[i], read.csv(file = paste(dir, enz_files[i], sep= "/"), header = TRUE, sep = ","))
    # change string to object
    exp <- eval(parse(text = exp_names[i]))

    # initiate the variables with all 0s
    Total_of_time <- c()
    Total_of_Z_0 <- rep(0, nrow(exp))
    Total_of_Z_1 <- rep(0, nrow(exp))
    Total_of_Z_2 <- rep(0, nrow(exp))
    
    # The first column is time, so start from the second column, average all the columns which have the same variable names
    for(var_name in names(exp))
    {
      if(startsWith(var_name, "Time"))
      {
        Total_of_time <- exp[var_name]
      }
      else if (startsWith(var_name, "Z_0.SS"))
      {
        Total_of_Z_0 <- Total_of_Z_0 + exp[var_name]
      }
      else if (startsWith(var_name, "Z_1.SS"))
      {
        Total_of_Z_1 <- Total_of_Z_1 + exp[var_name]
      }
      else if (startsWith(var_name, "Z_2.SS"))
      {
        Total_of_Z_2 <- Total_of_Z_2 + exp[var_name]
      }
    }
    avg_zone_0 <- Total_of_Z_0/45
    avg_zone_1 <- Total_of_Z_1/20
    avg_zone_2 <- Total_of_Z_2/3
    # For every file, get the average enzyme per node for every monte carlo trial
    df <- data.frame(Total_of_time, avg_zone_0, avg_zone_1, avg_zone_2)
    names(df) <- c("Time", "Zone_1", "Zone_2", "Zone_3")
    write.csv(df, file = paste("enz_", enzyme_type, "_", i, "_temp.csv", sep = ""),row.names=FALSE)   
  }
 ########################## After getting the enzymes per node of every zone for all the MC trials, average the results accross all the MC trials
  # Read all the "enz_XXXX_X_temp.csv" files
  temp_enz_files <- list.files(pattern = paste("enz_", enzyme_type, "_[0-9]+", "_temp.csv", sep = ""))
  exp_names <- c()
  for (i in 1:length(temp_enz_files))
  {
    exp_names[i] <- paste("enz_", enzyme_type, "_", i, sep = "")
    assign(exp_names[i], read.csv(temp_enz_files[i], header = TRUE, sep = ","))
    file.remove(temp_enz_files[i])
  }
  # enz_df is a data.frame which is used to save the final mean_enzyme_count_per_zone_XXXXXXX.csv
  enz_df <- data.frame(matrix(ncol = length(eval(parse(text = exp_names[1]))), nrow = nrow(eval(parse(text = exp_names[1])))))
  # For every variable in the dataset, average the data from all the files
  for(var_num in 1:length(eval(parse(text = exp_names[1]))))
  {
    # Get the data from the first file, if only one file, then total is only from the first file
    # if there are 2 or more files, then total need to add all the data from the other files
    total <- (eval(parse(text = exp_names[1])))[, var_num]
    if(length(exp_names) >= 2)
    {
      for (mc_num in 2:length(exp_names))
      {
        total <- total + (eval(parse(text = exp_names[mc_num])))[, var_num]
      }
    }
    enz_df[, var_num] <- total/(length(exp_names))
    
  }
  # Add names to the data frame and write to mean_enzyme_count_per_zone_XXXXXXX.csv file
  names(enz_df) <- c("Time", "Zone_1", "Zone_2", "Zone_3")
  write.csv(enz_df, paste(dir, "_mean_enzyme_count_per_zone_", enzyme_type, ".csv", sep = ""), row.names=FALSE)
}# End of the function
enzyme_types <- c("damageRepair", "Ndamage", "nonspecific", "Phase1", "Phase2", "Phase3")
for (enzyme_type in enzyme_types)
{
  enzymePerZone(argv, enzyme_type) 
}
 
print(paste(argv, " enzymes data is finished!", sep = ""))

################################################################# End
