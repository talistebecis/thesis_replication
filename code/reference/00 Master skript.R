# clean memory
rm(list=ls())

# load libraries
pacman::p_load(data.table)
pacman::p_load(dplyr)
pacman::p_load(tidyr)
pacman::p_load(readxl)
pacman::p_load(stringr)
pacman::p_load(gets)
pacman::p_load(getspanel)
pacman::p_load(here)

# call skripts
# data assembly
# source("01 Dataset_creation.R")

# # analysis
# source("02 Analysis.R")