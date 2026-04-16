#main file
library(devtools)
library(here)
setwd(here())
load_all()
library(tidyverse)

f <- c( 5, 4, 3, 2, 1)
fabio_mittelwert(f)