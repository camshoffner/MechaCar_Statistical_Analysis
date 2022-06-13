# install dplyer library
library(dplyr)

#Read csv data as dataframe
mpg_data <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)

# Linear Regression on mpg data
MC_lm <- lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data=mpg_data)
  
# Summary of linear regression model
summary(MC_lm)
  
# Reading in the Suspension Coil data
sc_data <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)

#Create Summary of PSI data
psi_summary <-sc_data %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance=var(PSI), SD = sd(PSI) )

#Create Summary for each lot
lot_summary <-sc_data %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance=var(PSI), SD = sd(PSI) )

# t-test population Deliverable 3
psi_ttestP <- t.test(sc_data$PSI, mu=mean(sc_data$PSI))

# t-test subsets 
#MF Lot 1
lot1_subset <- subset(sc_data, Manufacturing_Lot="Lot1")
psi_ttest1 <- t.test(lot1_subset$PSI, mu=mean(lot1_subset$PSI))

#MF Lot 2
lot2_subset <- subset(sc_data, Manufacturing_Lot="Lot2")
psi_ttest2 <- t.test(lot2_subset$PSI, mu=mean(lot2_subset$PSI))

#MF Lot 3
lot3_subset <- subset(sc_data, Manufacturing_Lot="Lot3")
psi_ttest3 <- t.test(lot3_subset$PSI, mu=mean(lot3_subset$PSI))
