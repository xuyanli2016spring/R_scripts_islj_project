# Name: Yanli Xu
# Date: Feb 13th
# To compare nectrig events at a specific time
# create .csv files

# Example to use this script
# > source("compare_trig_events_between_exps_in_one_group_at_specific_time.R")
# Please input the working directory: /Users/yanlixu/Desktop/experiment_data_2017/exp_2017_01_15_to_2017_01_21/mouse
# Please input a name to distinguish this group, for example, infusion_liver: mouse
# Enter 6 names including doseRatios to differentiate the experiments: 0.025, 0.050, 0.1, 0.25, 0.5, 0.75
# Please input the time at which you want to calculte the trigger events: 10800
# Please input the number of monte carlo trials: 3
# dose trig_events  trig_rate
# 1    0.025         472 0.01145492
# 2    0.050         914 0.02218177
# 3    0.100        1960 0.04756704
# 4    0.250        3426 0.08314525
# 5    0.500        8140 0.19754884
# 6    0.750       10852 0.26336610

compare_trig_events <- function()
{
  #########
 
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  # ask user the group name
  exp_group_name <- unlist(strsplit(readline(paste("Please input a name to distinguish this group, for example, infusion_liver: ", sep = "")), "[,.]+"))
  
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_nectrig.csv", recursive = T)
  
  # Ask user to input the concentrations
  dose_str <- readline(paste("Enter ", length(files),  " doseRatios to differentiate the experiments: ", sep = ""))
  dose <- as.numeric(unlist(strsplit(dose_str, "[, ]+")))
  
  # ask user at what time the user wants to calculte the trigger events
  run_time_str <- readline("Please input the time at which you want to calculte the trigger events: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  # ask user the number of monte carlo trials
  mc_str <- readline("Please input the number of monte carlo trials: ")
  num_mc <- as.numeric(unlist(strsplit(mc_str, "[, ]+")))
  
    trig_events <- c()
    trig_rate <- c()
    for(i in 1:length(files))
    {
      data <- read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ",")
      new_data <- subset(data, Time_of_NecTrig <= run_time)
      trig_events[i] = max(new_data$Cumu_Nectrig)
      trig_rate[i] = (max(new_data$Cumu_Nectrig))/(num_mc*14000)
    }
    df <- data.frame(dose, trig_events, trig_rate)
    write.table(df, file = paste("nectrig_events_of", exp_group_name,"at_time", run_time,".csv" ,sep = "_"), col.names = T, sep = ",")
    
    x_lim_str <- readline("Please input min and max doses to represent bottom and upper x limits: ")
    x_limits <- as.numeric(unlist(strsplit(x_lim_str, "[, ]+")))
    y_lim_str <- readline("Please input the bottom and upper limits to represent the range of nectrig percentage: ")
    y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
    plot(trig_rate ~ dose, type = "l",  xlim = x_limits, ylim = y_limits, main = paste("dose vs response of ", exp_group_name, " at ", run_time, sep = ""), xlab = "Doses", ylab = "Nectrig_Percentage")
}
compare_trig_events()