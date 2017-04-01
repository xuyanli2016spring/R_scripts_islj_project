# Name: Yanli Xu
# Date: Nov 3rd, 2016
# Input n csv file: necrotic-XXXX.csv
# Output 5 csv files: total_necrotic.csv, zone_1_necrotic.csv, zone_2_necrotic.csv, zone_3_necrotic.csv, zone_near_PV_necrotic.csv
# How to use the script: Go to the directory that includes the necrotic-XXXX.csv type commands: Rscript necrotic.R XXXX-XX-XX-XXXX-XXXX
########################################### Preparation
#! /usr/bin/Rscript 
argv <- commandArgs(TRUE)
if (length(argv) != 1) 
{
  print("Usage: Rscript necrotic.R <exp directories>")
  print("Please input one directory.")
  quit()
}
if (!dir.exists(argv))
{
  print("Usage: Rscript necrotic.R <exp directories>")
  print("Please make sure the input is a directory.")
  quit()
}
# "plyr" is for count function and rename function
library(plyr)
############################################ Input ncrotic-XXXX files and put them into one file
# read all the necrotic-XXXX.csv files
nec_files <- list.files(path = argv, pattern = "necrotic-[0-9]+.csv")
mc_trials_names <- c()
for (i in 1:length(nec_files))
{
  mc_trials_names[i] <- paste("mc_", i, sep = "")
  assign(mc_trials_names[i], read.csv(file = paste(argv, nec_files[i], sep= "/"), header = TRUE, sep = ","))
}
# combine all the MC trial files
for (i in 1:length(nec_files))
{
  nec_data <- eval(parse(text = mc_trials_names[i]))
  if(i == 1) # For the first file
  {
    if (file.exists("necrotic_new.csv")) {a <- file.remove("necrotic_new.csv")}; b <- file.create("necrotic_new.csv")
    write.table(nec_data, "necrotic_new.csv", append = T, row.names = F, col.names = F, sep = ",")
  }
  else # For the other files
  {
    write.table(nec_data, "necrotic_new.csv", append = T, row.names = F, col.names = F, sep = ",")
   }
}
nec_combined <- read.csv("necrotic_new.csv", header = T, sep = ",")
names(nec_combined) <- c("Time_of_Necrosis", "Cell_ID", "Dist_from_PV","Dist_from_CV")
a <- file.remove("necrotic_new.csv")
# Count function is from "plyr" library, for counting the frequncies
# make the Time_of_Necrosis in order 
nec_combined <- nec_combined[order(nec_combined$Time_of_Necrosis), ]
#Change the "freq" to num_of_necrotic_cells
nec_combined <- rename(count(nec_combined), c("freq" = "Num_Cells"))
#################################################### Output the total_necrotic.csv
# Create total_necrotic.csv file
file_name_total <- paste(argv, "_total_necrotic.csv", sep = "")
if (file.exists(file_name_total)) {a <- file.remove(file_name_total)}; b <- file.create(file_name_total)
# row_sub_nec is to save one row of data
row_sub_nec <- c()
# list out the unique time in the data set
unique_time <- unique(nec_combined$Time_of_Necrosis)
# For every unique time, combine all the rows with the same time and calculate the mean, meidan...
for(nec_time in unique_time)
{
  sub_nec <- subset(nec_combined, nec_combined$Time_of_Necrosis == nec_time)
  Time_of_sub_nec = nec_time
  Num_of_sub_nec = sum(sub_nec[, 5])
  Mean.Dist_from_PV_sub = mean(sub_nec[, 3])
  Median.Dist_from_PV_sub = median(sub_nec[, 3])
  Mean.Dist_from_CV_sub = mean(sub_nec[, 4])
  Median.Dist_from_CV_sub = median(sub_nec[, 4])
  if(nrow(sub_nec) == 1)
  {
    SD.Dist_from_PV_sub = 0
    SD.Dist_from_CV_sub = 0
  }
  else
  {
    SD.Dist_from_PV_sub = sd(sub_nec[, 3])
    SD.Dist_from_CV_sub = sd(sub_nec[, 4])
  }
  row_sub_nec <- c(Time_of_sub_nec, Num_of_sub_nec, Mean.Dist_from_PV_sub, Median.Dist_from_PV_sub, SD.Dist_from_PV_sub, Mean.Dist_from_CV_sub, Median.Dist_from_CV_sub, SD.Dist_from_CV_sub )
  write.table(rbind(row_sub_nec), file_name_total, append = T, row.names = F, col.names = F, sep = ",")  
}
necrosis_new <- read.csv(file_name_total, header = F, sep = ",")
# Add column names
colnames(necrosis_new) <- c("Time_of_Necrosis", "Num_Cells", "Mean.Dist_from_PV", "Median.Dist_from_PV", "SD.Dist_from_PV", "Mean.Dist_from_CV", "Median.Dist_from_CV", "SD.Dist_from_CV")
necrosis_new$cum_nectic <- cumsum(necrosis_new$Num_Cells)
# Calculate the average necrosis per monte carlo trial
necrosis_new$cum_nec_per_MC <- (necrosis_new$cum_nectic)/length(nec_files)
necrosis_new$Time_Norm <- (necrosis_new$Time_of_Necrosis)/max(necrosis_new$Time_of_Necrosis)
necrosis_new$Cumu_Necrotic_Norm <- (necrosis_new$cum_nectic)/max(necrosis_new$cum_nectic)
# output the final total_necrotic.csv file
write.csv(necrosis_new, file_name_total, row.names = F)
############################################################### output zone_3_necrotic.csv
# create perZone_necrotic.csv file
file_name_zone3 <- paste(argv, "_zone_3_necrotic.csv", sep = "")
if (file.exists(file_name_zone3)) {a <- file.remove(file_name_zone3)}; b <- file.create(file_name_zone3)
# subset the rows of zone 3
zone3_subset <- subset(nec_combined[nec_combined$Dist_from_CV >= 0 & nec_combined$Dist_from_CV <= 10, ])
unique_time_Z3 <- unique(zone3_subset$Time_of_Necrosis)
# For every unique time find all the other rows with the same time
for(time_nec in unique_time_Z3)
{
  sub_nec_Z3 <- subset(zone3_subset, zone3_subset$Time_of_Necrosis == time_nec)
  Time_Z3 = time_nec
  DistCV_Z3 = mean(sub_nec_Z3$Dist_from_CV)
  Necrotic_Z3 = sum(sub_nec_Z3$Num_Cells)
  row_sub_nec <- c(DistCV_Z3, Time_Z3, Necrotic_Z3 )
  write.table(rbind(row_sub_nec), file_name_zone3, append = T, row.names = F, col.names = F, sep = ",")  
}

Z3 <- read.csv(file_name_zone3, header = F, sep = ",")
colnames(Z3) <- c("DistCV_Z3", "Time_Z3", "Necrotic_Z3")
# add the cumsum column
Z3$Cumu_Necrotic_Z3 <- cumsum(Z3$Necrotic_Z3)
# Calculate average necrotic cells per monte carlo
Z3$Cumu_Necrotic_Z3_per_MC <- (Z3$Cumu_Necrotic_Z3)/length(nec_files)
write.csv(Z3, file_name_zone3, row.names = F)
#######################################################  output zone_2_necrotic.csv
file_name_zone2 <- paste(argv, "_zone_2_necrotic.csv", sep = "")
if (file.exists(file_name_zone2)) {a <- file.remove(file_name_zone2)}; b <- file.create(file_name_zone2)
zone2_subset <- subset(nec_combined[nec_combined$Dist_from_CV > 10 & nec_combined$Dist_from_CV <= 20, ])
unique_time_Z2 <- unique(zone2_subset$Time_of_Necrosis)
# For every unique time find all the other rows with the same time
for(time_nec in unique_time_Z2)
{
  sub_nec_Z2 <- subset(zone2_subset, zone2_subset$Time_of_Necrosis == time_nec)
  Time_Z2 = time_nec
  DistCV_Z2 = mean(sub_nec_Z2$Dist_from_CV)
  Necrotic_Z2 = sum(sub_nec_Z2$Num_Cells)
  row_sub_nec <- c(DistCV_Z2, Time_Z2, Necrotic_Z2 )
  write.table(rbind(row_sub_nec), file_name_zone2, append = T, row.names = F, col.names = F, sep = ",")  
}

Z2 <- read.csv(file_name_zone2, header = F, sep = ",")
colnames(Z2) <- c("DistCV_Z2", "Time_Z2", "Necrotic_Z2")
Z2$Cumu_Necrotic_Z2 <- cumsum(Z2$Necrotic_Z2)
# Calculate average necrotic cells per monte carlo
Z2$Cumu_Necrotic_Z2_per_MC <- (Z2$Cumu_Necrotic_Z2)/length(nec_files)
write.csv(Z2, file_name_zone2, row.names = F)
#######################################################  output zone_1_necrotic.csv
file_name_zone1 <- paste(argv, "_zone_1_necrotic.csv", sep = "")
zone1_subset <- subset(nec_combined[nec_combined$Dist_from_CV > 20 & nec_combined$Dist_from_CV <= 30, ])
if (file.exists(file_name_zone1)) {a <- file.remove(file_name_zone1)}; b <- file.create(file_name_zone1)
unique_time_Z1 <- unique(zone1_subset$Time_of_Necrosis)
# For every unique time find all the other rows with the same time
for(time_nec in unique_time_Z1)
{
  sub_nec_Z1 <- subset(zone1_subset, zone1_subset$Time_of_Necrosis == time_nec)
  Time_Z1 = time_nec
  DistCV_Z1 = mean(sub_nec_Z1$Dist_from_CV)
  Necrotic_Z1 = sum(sub_nec_Z1$Num_Cells)
  row_sub_nec <- c(DistCV_Z1, Time_Z1, Necrotic_Z1 )
  write.table(rbind(row_sub_nec), file_name_zone1, append = T, row.names = F, col.names = F, sep = ",")  
}

Z1 <- read.csv(file_name_zone1, header = F, sep = ",")
colnames(Z1) <- c("DistCV_Z1", "Time_Z1", "Necrotic_Z1")
Z1$Cumu_Necrotic_Z1 <- cumsum(Z1$Necrotic_Z1)
# Calculate average necrotic cells per monte carlo
Z1$Cumu_Necrotic_Z1_per_MC <- (Z1$Cumu_Necrotic_Z1)/length(nec_files)
write.csv(Z1, file_name_zone1, row.names = F)
############################################################ output zone_near_PV_necrotic.csv
file_name_zone_PV <- paste(argv, "_zone_near_PV_necrotic.csv", sep = "")
zone_PV_subset <- subset(nec_combined[nec_combined$Dist_from_CV > 30, ])
if (file.exists(file_name_zone_PV)) {a <- file.remove(file_name_zone_PV)}; b <- file.create(file_name_zone_PV)
unique_time_Zone_PV <- unique(zone_PV_subset$Time_of_Necrosis)
# For every unique time find all the other rows with the same time
for(time_nec in unique_time_Zone_PV)
{
  sub_nec_Zone_PV <- subset(zone_PV_subset, zone_PV_subset$Time_of_Necrosis == time_nec)
  Time_Zone_PV = time_nec
  DistCV_Zone_PV = mean(sub_nec_Zone_PV$Dist_from_CV)
  Necrotic_Zone_PV = sum(sub_nec_Zone_PV$Num_Cells)
  row_sub_nec <- c(DistCV_Zone_PV, Time_Zone_PV, Necrotic_Zone_PV )
  write.table(rbind(row_sub_nec), file_name_zone_PV, append = T, row.names = F, col.names = F, sep = ",")  
}

Zone_PV <- read.csv(file_name_zone_PV, header = F, sep = ",")
colnames(Zone_PV) <- c("DistCV_Zone_PV", "Time_Zone_PV", "Necrotic_Zone_PV")
Zone_PV$Cumu_Necrotic_Zone_PV <- cumsum(Zone_PV$Necrotic_Zone_PV)
# Calculate average necrotic cells per monte carlo
Zone_PV$Cumu_Necrotic_Zone_PV_per_MC <- (Zone_PV$Cumu_Necrotic_Zone_PV)/length(nec_files)
write.csv(Zone_PV, file_name_zone_PV, row.names = F)
print(paste(argv, " necrotic data is finished!", sep = ""))
###############################################################  End
