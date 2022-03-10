library(tidyverse)
library("readxl")
# xlsx files
my_data <- read_excel( file.choose("ZebrafishQuantData.xlsx"))
view(my_data)                      
