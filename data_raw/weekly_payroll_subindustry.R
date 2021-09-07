# Download weekly payroll subindustry jobs 

#Load in required pacakges
library(readr) # Easily Install and Load the 'Tidyverse', CRAN v1.3.1  
library(readabs) # Download and Tidy Time Series Data from the Australian Bureau of Statistics, CRAN v0.4.9 
library(strayr) # Ready-to-use Australian common structures and classifications and tools for working with them, [github::runapp-aus/strayr] v0.1.2 

# download payroll jobs (by subindustry)
payroll_sub <- readabs::read_payrolls("subindustry_jobs") 

#save a csv file of the data to be read in when plotting

payroll_sub %>% write_csv("data/weekly_payroll_subindustry.csv")







