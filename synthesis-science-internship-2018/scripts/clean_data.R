##  Always load this script using the source() function at the start of all other scripts.
##  I haven't done much cleaning yet, but when we need to do any cleaning to the entire dataset,
##  do it here.

# Load Libraries
library(tidyverse)
library(googlesheets)

# Read data from computer
#raw_data <- read.csv("data_input/dataone_SR_raw_data.csv", head = TRUE)
#head(raw_data)

# Read data from google sheets
(my_sheets <- gs_ls()) #get list of sheets you can access, will possible prompt you with a google login and tidyverse permission page
my_sheets #just to see the list of sheets you can access
gs1 = gs_title('database_for_systematic_review', verbose = TRUE) #get spreadsheet of interest
raw_data = gs_read(gs1,ws = "Combined80papers") #get worksheet of interest from spreadsheet
raw_data <- raw_data[raw_data$input_unique_in_paper!="no",] #remove duplicate source data rows