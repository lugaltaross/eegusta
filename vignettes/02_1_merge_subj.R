rm(list=ls())

library(dplyr)
library(stringr)
library(eeguana)
source('./functions/make_sample.R', echo=TRUE)

load("locations_eeguana.Rdata")
stat_summ="mean"

debug(make_sample)
make_sample(file_dir="./data_bdf",
                        files="sub.._seg\\.Rdata",
                        stat_summ="median", #MANCA, IMPLEMENTARE QUESTA
                        LOC=LOC)
