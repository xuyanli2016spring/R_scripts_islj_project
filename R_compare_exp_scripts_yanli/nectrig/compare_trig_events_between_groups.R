compare_trig_events <- function()
{
  #########
  # Ask user to input the number of groups of experiments
  num_of_groups <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of groups of experiments you would like to compare, for example 2: "), "[, ]+")))

  # ask user to input the directory and name
  exp_dirs <- c()
  exp_group_names <- c()
  for(d_num in 1:num_of_groups)
  {
    exp_dirs[d_num] <- unlist(strsplit(readline(paste("Please input the directory of experiments of group ", d_num, ": ", sep = "")), "[, ]+"))
    exp_group_names[d_num] <- unlist(strsplit(readline(paste("Please input a name to distinguish this group, for example, infusion_liver: ", sep = "")), "[,.]+"))
  }
  
  # Ask user how many experiments in every group
  num_of_exp_in_one_group <- as.numeric(unlist(strsplit(readline(prompt="Please input the number of experiments in every group, for example 5: "), "[, ]+")))
  # ask user the concentration values
  exp_names_str <- readline(paste("Enter ", num_of_exp_in_one_group,  " names to distinguish the experiments: ", sep = ""))
  exp_names <- unlist(strsplit(exp_names_str, "[, ]+"))
  
  # ask user at what time the user wants to calculte the trigger events
  run_time_str <- readline("Please input the time at which you want to calculte the trigger events: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  # ask user the number of monte carlo trials
  mc_str <- readline("Please input the number of monte carlo trials: ")
  num_mc <- as.numeric(unlist(strsplit(mc_str, "[, ]+")))
  
#   #########
  for(group_num in 1:num_of_groups)
  {
    # create csv file
    #file.create(paste("nectrig_events_of_", exp_group_names[group_num],".csv" ,sep = ""))
    # Read the files
    files <- list.files(path = exp_dirs[group_num], pattern = "[0-9]+_nectrig.csv", recursive = T)
    trig_events <- c()
    trig_rate <- c()
    for(i in 1:length(exp_names))
    {
      data <- read.csv(paste(exp_dirs[group_num], files[i], sep= "/"), header = TRUE, sep = ",")
      new_data <- subset(data, Time_of_NecTrig <= run_time)
      trig_events[i] = max(new_data$Cumu_Nectrig)
      trig_rate[i] = (max(new_data$Cumu_Nectrig))/(num_mc*14000)
      #print(paste("The Cumu_Nectrig is ", max(new_data$Cumu_Nectrig)))
    }
    df <- data.frame(exp_names, trig_events, trig_rate)
    write.table(df, file = paste("nectrig_events_of", exp_group_names[group_num],run_time,".csv" ,sep = "_"), col.names = T, sep = ",")
    

    
  }# end of for loop
}
compare_trig_events()