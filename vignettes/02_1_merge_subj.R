rm(list=ls())

library(dplyr)
library(stringr)
library(eeguana)
library(eegusta)

load("locations_eeguana.Rdata")


# debug(make_sample)
make_sample(file_dir = "./data_bdf",
                        files = "sub.._seg\\.Rdata",
                        stat_summ = "mean",
                        location = LOC)
