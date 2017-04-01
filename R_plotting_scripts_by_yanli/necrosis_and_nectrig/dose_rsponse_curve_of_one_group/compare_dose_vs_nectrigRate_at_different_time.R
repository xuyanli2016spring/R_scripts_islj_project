# Name: Yanli Xu
# Date: Feb 13th, 2017
# To compare dose vs nectrig_rate at different time

nectrig_vs_dose <- function()
{
  # Ask user to input the working directory
  work_dir <- unlist(strsplit(readline(prompt="Please input the working directory: "), "[ ]+"))
  
  # Read the files
  files <- list.files(path = work_dir, pattern = "[0-9]+_nectrig.csv", recursive = T)
  
  # Ask user to input the names of the experiments
  conc_str <- readline(paste("Enter ", length(files),  " different doseRatios for all the experiments: ", sep = ""))
  conc <- as.numeric(unlist(strsplit(conc_str, "[, ]+")))
  
  exp_names <- c()
  for(i in 1:length(conc))
  {
    exp_names[i] <- paste("nectrig_", conc[i], sep="")
    assign(exp_names[i], read.csv(paste(work_dir, files[i], sep= "/"), header = TRUE, sep = ","))
  }
  
  par(mfrow = c(1, 1))
  
    # Ask user how many MC trials
    mc_str <- readline("Please input the number of monte carlo trials: ")
    num_mc <- as.numeric(unlist(strsplit(mc_str, "[, ]+")))
  
  x_lim_str <- readline("Please input min and max doseRatios to be bottom and upper x limits: ")
  x_limits <- as.numeric(unlist(strsplit(x_lim_str, "[, ]+")))
  y_lim_str <- readline("Please input the bottom and upper limits to represent the nectrig percentage, for expample (0, 0.5): ")
  y_limits <- as.numeric(unlist(strsplit(y_lim_str, "[, ]+")))
  
  plot(c(), c(), xlim = x_limits, ylim = y_limits, xlab = "Doses", ylab = "Nectrig_rate", main = "Doses vs Nectrig_rate")
  
  run_time_str <- readline("Please input the cycleLimit of these experiments: ")
  run_time <- as.numeric(unlist(strsplit(run_time_str, "[, ]+")))
  
  col_str <- readline(paste("Please input ", (run_time/1800), " different colors to represent different time, you may choose from these colors, black, blue, green, red, burlywood4, forestgreen, firebrick, blue4, purple, chocolate, cyan, gold, darkviolet, brown: ", sep = "")) 
  color <- unlist(strsplit(col_str, "[, ]+"))
  
  for(hrs in 1:(run_time/1800))
  {
    cumu_nectrig <- c()
    for(i in 1:length(conc))
    {
      exp <- eval(parse(text = exp_names[i]))
      exp_sub <- subset(exp[exp$Time_of_NecTrig <= (1800 * hrs), ])
      cumu_nectrig[i] <-  max(exp_sub$Cumu_Nectrig)  
    }
    nectrig_rate <- cumu_nectrig/(14000*num_mc)
    lines(nectrig_rate ~ conc, col = color[hrs])
  }
  legend_str <- readline("You already got the plot, please input x and y coordinates to put legends: ")
  legend_x <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[1]
  legend_y <- as.numeric(unlist(strsplit(legend_str, "[, ]+")))[2]
  
  legends <- c()
  for(i in 1:(run_time/1800))
  {
    legends[i] = paste(i*1800, "s", sep = "")
  }
  
  legend(legend_x, legend_y, legends,
         col=color, cex=1,text.font=1, lty = c(1, 1), bg='lightblue')
}
nectrig_vs_dose()