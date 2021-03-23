###################################################################################
###################################################################################
## Paper: Structure of Proboscis and their associated sensilla in moth Families  ##
## Lepidoptera: Geometridae, Sphingidae and Erebidae                             ##
## Written by - Rajesh Lenka (rlenka85@gmail.com)                                ##
## Senior Project Fellow, Zoological Survey of India, Kolkata                    ##
## Date: 22_Jan_2021 | Last updated: 22_Jan_2021                                 ##   
## Description - This script helps plotting PCA in various ways and also helps in##
## calculating various calculations regarding PCA.                               ##  
###################################################################################
###################################################################################
proboscis <- read.csv('proboscis.csv', row.names = 1)
proboscis
proboscis_data <- proboscis[,1:6]
proboscis_data
proboscis.pca <- prcomp(na.omit(proboscis_data), center = T, scale =T)
install.packages("ggfortify")
library(ggfortify)
pca.plot <- autoplot(proboscis.pca, data = proboscis, colour='family')
pca.plot

#Alternate code for PCA of measurement of proboscis sensilla
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")
proboscis <- read.csv('proboscis.csv', row.names = 1)
proboscis
proboscis_data <- proboscis[,1:6]
proboscis_data
PCA(proboscis_data, scale.unit = TRUE, ncp = 5, graph = TRUE)
library("FactoMineR")
res.pca <- PCA(na.omit(proboscis_data), graph = FALSE)
print(res.pca)
library("factoextra")
#Calculate Eigen values
eig.val <- get_eigenvalue(res.pca)
eig.val
#Scree plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
#Calculate Variance
var <- get_pca_var(res.pca)
var
# Coordinates
head(var$coord)
# Cos2: quality on the factore map
head(var$cos2)
# Contributions to the principal components
head(var$contrib)
# Correlation circle
# Coordinates of variables
head(var$coord, 4)
# Plotting variable correlation plot
fviz_pca_var(res.pca, col.var = "black")
# Quality of the variables on the factor map
#Cos2 value
head(var$cos2, 6)
#Visualise Cos Value
install.packages("corrplot")
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
# Bar plot of variables cos2 using the function fviz_cos2()[in factoextra]
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)
# Contributions of variables to PCs
head(var$contrib, 6)
#corrplot the contribution of variables
library("corrplot")
corrplot(var$contrib, is.corr=FALSE)
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# The total contribution to PC1 and PC2 is obtained with the following R code
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)
# The most important (or, contributing) variables highlighted on the correlation plot
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Changing the transparency by contrib values
fviz_pca_var(res.pca, alpha.var = "contrib")
fviz_pca_ind(res.pca)
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
