rm(list = ls())

library(dplyr)
library(reshape2)
library(stringi)
library(assertthat)

source("scripts/functions.R")

### DECLARE PARAMS ##############
nr_redundant_symbols = 2 # 2 symbols match co-players, how many do not match nobody?
black_n_white = FALSE # just for testing - if TRUE, number will be plotted next to the symbol to immitate color
seed = 950707
### DECLARE PARAMS ##############


# we can just LOAD IMAGE from previous run
  # load("outputs.RData")

# or we can RUN THE PROCEDURE
  # DELETES pics folder content
  do.call(file.remove, list(list.files("pics", full.names = TRUE)))
  # sets SEEDS
  set.seed(seed)
  # RUNS the algorithm, exports pictures to the pics folder
  runEverything(nr_redundant_symbols, black_n_white)
  # save image
  save.image("output.RData")