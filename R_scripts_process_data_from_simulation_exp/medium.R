#! /usr/bin/Rscript

# Name: Yanli Xu
# Date: Nov 5th, 2016
# Input n csv file: medium-XXXX.csv
# Output 1 csv file: medium.csv
# How to use the script: Go to the directory that includes the medium-XXXX.csv type commands: Rscript medium.R XXXX-XX-XX-XXXX-XXXX

########################################### Preparation
argv <- commandArgs(TRUE)
if (length(argv) != 1) 
{
  print("Usage: Rscript medium.R <exp directories>")
  print("Please input one directory.")
  quit()
}
if (!dir.exists(argv))
{
  print("Usage: Rscript medium.R <exp directories>")
  print("Please make sure the input is a directory.")
  quit()
}

############################################ Input medium-XXXX files 
# read all the medium-XXXX.csv files
medium_files <- list.files(path = argv, pattern = "medium-[0-9]+.csv")
medium_exp_names <- c()
for (i in 1:length(medium_files))
{
  medium_exp_names[i] <- paste("medium_", i, sep = "")
  assign(medium_exp_names[i], read.csv(file = paste(argv, medium_files[i], sep= "/"), header = TRUE, sep = ","))
}
#######################################################  Output medium.csv file
# Create "medium.csv" file
file_name <- paste(argv, "_medium.csv", sep = "")
if (file.exists(file_name)) {a <- file.remove(file_name)}; b <- file.create(file_name)
# emdium_df is a data.frame which is used to save the final emdium data
medium_df <- data.frame(matrix(ncol = length(eval(parse(text = medium_exp_names[1]))), nrow = nrow(eval(parse(text = medium_exp_names[1])))))
# For every variable in the dataset, average the data from all the files
for(var_num in 1:length(eval(parse(text = medium_exp_names[1]))))
{
  # Get the data from the first file, if only one file, then total is the onl from the first file
  # if there are 2 or more files, then total need to add all the data from the other files
  total <- (eval(parse(text = medium_exp_names[1])))[, var_num]
  if(length(medium_exp_names) >= 2)
  {
    for (mc_num in 2:length(medium_exp_names))
    {
      total = total + (eval(parse(text = medium_exp_names[mc_num])))[, var_num]
    }
  }
  mean_of_var <- total/(length(medium_exp_names))
  medium_df[, var_num] <- mean_of_var
}
# Add names to the data frame and write to emdium.csv file
names(medium_df) <- names(eval(parse(text = medium_exp_names[1])))
write.csv(medium_df, file_name, row.names = F)

print(paste(argv, " medium data is finished!", sep = ""))

################################################################# End


