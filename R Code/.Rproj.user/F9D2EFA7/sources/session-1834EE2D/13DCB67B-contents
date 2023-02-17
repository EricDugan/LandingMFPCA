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
#GRFX<- data.frame(GRFX, row.names = 1)
GRFX = GRFX[, -c(1:17)] #remove all columns except GRFx time series dat
GRFX = data.matrix(GRFX)

GRFY = subset(AllCMJData, Var == "LGRFy" | Var == "RGRFy")
#GRFY <- data.frame(GRFY, row.names = 1)
GRFY = GRFY[, -c(1:17)]
GRFY = data.matrix(GRFY)

GRFZ = subset(AllCMJData, Var == "LGRFz" | Var == "RGRFz")
#GRFZ <- data.frame(GRFZ, row.names = 1)
GRFZ = GRFZ[, -c(1:17)]
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

#### Comparison to univariate FPCA (Fig. 1) ####

# calculate the univariate FPCA for the gait data
pca1 <- PACE(myIOR[[1]], npc = 5)
pca2 <- PACE(myIOR[[2]], npc = 5)
pca3 <- PACE(myIOR[[3]], npc = 5) # for 3 time series

# rescale multivariate functions to norm 1 one each interval
MFPCAIOR$functions[[1]] <- MFPCAIOR$functions[[1]]/funData::norm(MFPCAIOR$functions[[1]], squared = F)
MFPCAIOR$functions[[2]] <- MFPCAIOR$functions[[2]]/funData::norm(MFPCAIOR$functions[[2]], squared = F)
MFPCAIOR$functions[[3]] <- MFPCAIOR$functions[[3]]/funData::norm(MFPCAIOR$functions[[3]], squared = F) # for 3 time series

# flip univariate functions for comparison purposes

# for 3 time series
uFPCA <- multiFunData(flipFuns(MFPCAIOR$functions[[1]], pca1$functions),
                      flipFuns(MFPCAIOR$functions[[2]], pca2$functions),
                      flipFuns(MFPCAIOR$functions[[3]], pca2$functions))

# "flip" associate scores
pca1$scores[,2:3] <- -1 * pca1$scores[,2:3]


screeplot(MFPCAIOR, type = "barplot")
plot(MFPCAIOR, combined = TRUE)
scoreplot(MFPCAIOR)