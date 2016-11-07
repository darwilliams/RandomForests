library(tidyverse)
library(knitr)

vantest <- RES

one_m <- vantest$One_m$all
str(one_m)
kappa <- as.data.frame(one_m$Kappa)
Fmeas <- as.data.frame(one_m$Fmeas)
ConfMat <- as.data.frame(one_m$ConfMat)
write_csv(ConfMat,"D:/RandomForests/Results/vantestConfMat")
write_csv(kappa,"D:/RandomForests/Results/vantestKap")
write_csv(Fmeas,"D:/RandomForests/Results/vantestFmeas")