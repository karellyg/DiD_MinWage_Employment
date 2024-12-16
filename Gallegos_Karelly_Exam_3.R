##################################################
# ECON 418-518 Exam 3
# Karelly Gallegos
# The University of Arizona
# karellygallegos@arizona.edu 
# 15 December 2024
###################################################

#####################
# Preliminaries
#####################

# Clear environment, console, and plot pane
rm(list = ls())
cat("\014")
graphics.off()

# Turn off scientific notation
options(scipen = 999)

# Load packages
pacman::p_load(data.table)

# Set the working directory
setwd("Desktop")

#Load the data and label it "data" as data table
data <- fread("ECON_418-518_Exam_3_Data.csv")

#Check that it laoded right
head(data)

#################
# part (ii)
#################

# Create indicator variables/cloumn for November
data[, Nov := ifelse(time_period == "Nov", 1, 0)] 

# Create indicator variables/cloumn for NJ
data[, NJ := ifelse(state == 1, 1, 0)]    

#Check that it loaded right
head(data)

# Compute mean total employment in New Jersey in November
mean_emp_nj_nov <- data[NJ == 1 & Nov == 1, mean(total_emp)]

#Check what the number is
mean_emp_nj_nov

# Compute mean total employment in New Jersey in February
mean_emp_nj_feb <- data[NJ == 1 & Nov == 0, mean(total_emp)]

#Check what the number is
mean_emp_nj_feb

# Compute mean total employment in Pennysylvania in November
mean_emp_pa_nov <- data[NJ == 0 & Nov == 1, mean(total_emp)]

#Check what the number is
mean_emp_pa_nov

# Compute mean total employment in Pennysylvania in February
mean_emp_pa_feb <- data[NJ == 0 & Nov == 0, mean(total_emp)]

#Check what the number is
mean_emp_pa_feb

#################
# part (iii)
#################

# Calculate DiD
DiD_estimate <- (mean_emp_nj_nov - mean_emp_nj_feb) - 
                (mean_emp_pa_nov - mean_emp_pa_feb)

#Check what the DiD is 
DiD_estimate

#################
# part (iv)
#################

# Estimate the DiD model using lm()
model <- lm(total_emp ~ Nov * NJ, data = data)
summary(model)

# Compute the 95% confidence interval for the ATT
confint(model)

#################
# part (vi)
#################

# Estimate the DiD model with restaurant fixed effects
model2 <- lm(total_emp ~ Nov * NJ + factor(restaurant_id), data = data)
summary(model2)

