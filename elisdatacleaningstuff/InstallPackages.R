
# This was only run once at the creation of the project 

renv::init()
renv::install("tidyverse")
renv::install("fredr")
renv::install("lubridate")
renv::install("stringr")
renv::install("reshape2")


renv::snapshot()
