##  Always load this script using the source() function at the start of all other scripts.
##  I haven't done much cleaning yet, but when we need to do any cleaning to the entire dataset,
##  do it here.

library(tidyverse)

raw_data <- read.csv("data_input/dataone_SR_raw_data.csv", head = TRUE)

head(raw_data)
