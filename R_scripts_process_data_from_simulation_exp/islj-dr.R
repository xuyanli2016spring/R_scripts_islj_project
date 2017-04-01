#! /usr/bin/Rscript

# Name: Yanli Xu
# Date: Nov 7th, 2016
# Input n csv file: run-XXXX.csv
# Output 1 csv file: profile_mean_per_node.csv
# How to use the script: Go to the directory that includes the run-XXXX.csv type commands: Rscript islj-dr.R XXXX-XX-XX-XXXX-XXXX

########################################### Preparation
argv <- commandArgs(TRUE)
if (length(argv) != 1) 
{
  print("Usage: Rscript islj-dr.R <exp directories>")
  print("Please input one directory.")
  quit()
}
if (!dir.exists(argv))
{
  print("Usage: Rscript islj-dr.R <exp directories>")
  print("Please make sure the input is a directory.")
  quit()
}

############################################ Input run-XXXX.csv files 
# read all the run-XXXX.csv files
run_files <- list.files(path = argv, pattern = "run-[0-9]+.csv")
run_exp_names <- c()
for (i in 1:length(run_files))
{
  run_exp_names[i] <- paste("run_", i, sep = "")
  assign(run_exp_names[i], read.csv(file = paste(argv, run_files[i], sep= "/"), header = TRUE, sep = ","))
}
#######################################################  Output profile_mean_per_node.csv file
# Create "profile_mean_per_node.csv" file
file_name <- paste(argv, "_profile_mean_per_node.csv", sep = "")
if (file.exists(file_name)) {a <- file.remove(file_name)}; b <- file.create(file_name)
# profile_df is a data.frame which is used to save the final profile mean per node data
profile_df <- data.frame(matrix(ncol = length(eval(parse(text = run_exp_names[1]))), nrow = nrow(eval(parse(text = run_exp_names[1])))))
# For every variable in the dataset, average the data from all the files
for(var_num in 1:length(eval(parse(text = run_exp_names[1]))))
{
  # Get the data from the first file, if only one file, then total is only from the first file
  # if there are 2 or more files, then total need to add all the data from the other files
  total <- (eval(parse(text = run_exp_names[1])))[, var_num]
  if(length(run_exp_names) >= 2)
  {
    for (mc_num in 2:length(run_exp_names))
    {
      total = total + (eval(parse(text = run_exp_names[mc_num])))[, var_num]
    }
  }
  mean_of_var <- total/(length(run_exp_names))
  profile_df[, var_num] <- mean_of_var
}
# Add names to the data frame and write to profile_mean_per_node.csv file
names(profile_df) <- names(eval(parse(text = run_exp_names[1])))
write.csv(profile_df, file_name, row.names = F)
print(paste(argv, " run data is finished!", sep = ""))


################################################################# End



