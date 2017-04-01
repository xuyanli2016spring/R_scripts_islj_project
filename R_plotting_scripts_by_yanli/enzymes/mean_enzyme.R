# Name: Yanli Xu
# Date: Nov 21th, 2016
# Note: This script is used for plotting time vs rxn for multiple rxnProduct_zone_X files
# How to use this script: open terminal, cd to the directory of this script, enter command R will
# make the terminal work under R, then enter command source("mean_enzyme.R"), the script will start working 
# Input files: XXXX-XX-XX-XXXX-XXXX-mean_enzyme_count_per_zone-AAAA_BBBB.csv or XXXX-XX-XX-XXXX-XXXX-mean_enzyme_count_per_zone_BBBB.csv
# plot all the Time vs zone_enzyme_level(zone 1, zone 2 and zone 3)

# The following is an example of running this script
# > source("mean_enzyme.R")
# Please input the working directory: /Users/yanlixu/Desktop/exp_from_around_201611/Updated_data_from_2016_11_01/data_from_around_2016_11_01
# Please input the enzyme file name for plotting, you may choose one from these 12 names, damageRepair, Ndamage, nonspecific, Phase1, Phase2, Phase3, dxdt_damageRepair, dxdt_Ndamage, dxdt_nonspecific, dxdt_Phase1, dxdt_Phase2, dxdt_Phase3: dxdt_Phase3
# Enter 9 concentration values separated by comma: 5, 10, 15, 25, 35, 50, 75, 100, 125
# You have  9  plots, please input how many rows and columns do you want to display the plots: 3, 3
# Please input the cycleLimit of these experiments: 36000

time_vs_enzyme_level <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # Ask user the enzyme_file name
  enz_file_str <- readline("Please input the enzyme file name for plotting, you may choose one from these 12 names, damageRepair, Ndamage, nonspecific, Phase1, Phase2, Phase3, dxdt_damageRepair, dxdt_Ndamage, dxdt_nonspecific, dxdt_Phase1, dxdt_Phase2, dxdt_Phase3: ")
  enz_file <- unlist(strsplit(enz_file_str, "[ ,]+"))
  # Read the files
  files <- list.files(path = work_dir, pattern =  paste("[0-9]+_mean_enzyme_count_per_zone[_|-]", enz_file, ".csv", sep = ""), recursive = T)
  # Ask user to input the concentrations
  conc_str <- readline(paste("Enter ", length(files),  " concentration values separated by comma: ", sep = ""))
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  # Read the files
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("rxn_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  mf_input <- readline(paste("You have ", length(files), " plots, please input how many rows and columns do you want to display the plots: "))
  par(mfrow=as.numeric(unlist(strsplit(mf_input, "[, ]+"))))

    # Ask user the cycleLimit, this information can be used for the max value of the x-axis
    run_time_str <- readline("Please input the cycleLimit of these experiments: ")
    run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
    
    # predict the enzyme range for y-axis by using the last concentration file, zone_1 and zone_3
    exp <- eval(parse(text = exp_names[length(files)]))
    y_limit_1 <- exp[1, 2]
    y_limit_2 <- exp[1, 4]
  
  # For every file
  for (i in 1:length(conc))
  {
    exp <- eval(parse(text = exp_names[i]))
    
    plot(exp$Zone.3 ~ exp$Time, type = "l", xlab = "Time", ylab = "Enzyme", main = paste("mean_enz", enz_file, conc[i], sep="_"), ylim = c(min(y_limit_1, y_limit_2)*0.8, max(y_limit_1, y_limit_2)*1.2), xlim = c(0, run_time))
    lines(exp$Zone.2 ~ exp$Time, col = "blue")
    lines(exp$Zone.1 ~ exp$Time, col = "red")
    if(i == 1)
    {
      legend("topleft", c("Z3", "Z2", "Z1"),
             col=c("black", "blue", "red"), cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
    }
  }
}
time_vs_enzyme_level()