# Load libraries
library(MFPCA)
require(fda)
library(tidyr)
library(readxl)
library(cluster)
library(ggplot2)
library(dplyr)
library(fdm2id)
library(flexclust)
library(cvTools)
library(readr)
library(tidyverse)
library(clValid)
library(ggthemes)
library(gridExtra)
library(ggpubr)
library(grid)
library(factoextra)

#### Load and plot the data ####

AllCMJData = read_excel(paste("Z:/DEVELOPMENT/SPORT MA",
                           "/CMJ GRF Project/Master_CMJ_Landing_Data.xlsx",sep = ""))

numFiles = c(1:nrow(AllCMJData))

AllCMJData = add_column(AllCMJData, numFiles, .before = 1)

# need to fix row names
GRFX = subset(AllCMJData, Var == "LGRFx" | Var == "RGRFx")
GRFX<- data.frame(GRFX, row.names = 1)
GRFX = GRFX[, -c(1:16)] #remove all columns except GRFx time series dat
GRFX = data.matrix(GRFX)

GRFY = subset(AllCMJData, Var == "LGRFy" | Var == "RGRFy")
GRFY <- data.frame(GRFY, row.names = 1)
GRFY = GRFY[, -c(1:16)]
GRFY = data.matrix(GRFY)

GRFZ = subset(AllCMJData, Var == "LGRFz" | Var == "RGRFz")
GRFZ <- data.frame(GRFZ, row.names = 1)
GRFZ = GRFZ[, -c(1:16)]
GRFZ = data.matrix(GRFZ)

set.seed(1234)

myIOR <- multiFunData(funData(argvals = seq(from=1, to=101, by=1), X = GRFX), 
                      funData(argvals = seq(from=1, to=101, by=1), X = GRFY),
                      funData(argvals = seq(from=1, to=101, by=1), X = GRFZ))

# plot the data
par(mar = c(4,2.5,0.5,0.5) + 0.1, cex.axis = 1.5, cex.lab = 1.5)

plot(myIOR[[1]], col = "gray", xlab = "Percent Landing")
legend("topleft", legend = "", title = "GRFx", cex = 1.25, bty = "n")

plot(myIOR[[2]], col = "gray", xlab = "Percent Landing")
legend("topleft", legend = "", title = "GRFy", cex = 1.25, bty = "n")

plot(myIOR[[3]], col = "gray", xlab = "Percent Landing")
legend("topleft", legend = "", title = "GRFz", cex = 1.25, bty = "n")


#### MFPCA calculation ####
set.seed(1234)
MFPCAIOR <- MFPCA(myIOR, M = 5, uniExpansions = list(list(type = "splines1Dpen"),
                                                     list(type = "splines1Dpen"),
                                                     list(type = "splines1Dpen"))) # for 3 time series

