# Packages
library(readxl)
library(FactoMineR) # For PCA 1
library(ggplot2)
library(gplots)
library(maps)
library(mapdata)
library(dplyr)
library(RDSTK)     # Recently removed from CRAN
library(ggfortify)
library(ggpubr)
library(agricolae)
library(car)        # Tests
library(factoextra) # Data visualization
library(NbClust)    # Determining the optimal number of clusters
library(fpc)        # Computing clustering validation statistics
library(ggforce)
library(cluster)


"Principal Component Analysis (PCA)"

# Import data
salsas <- read_excel("C:/Users/Ruben Padilla/Downloads/Trabajos/Bases de datos/copy_db_analysis.xlsx", sheet="salsas")
attach(salsas)

# Remove id column
data <- salsas[-1]

# Rename rows
data <- as.data.frame(data)
row.names(data) <- salsas$salsa

# Correlation
cor(data)


# PCA 1 ---------------------------------------
pca_salsas1 <- PCA(data, graph = F)
print(pca_salsas1)
summary(pca_salsas1)

# Plots
plot(pca_salsas1, choix = "ind")
plot(pca_salsas1, choix = "var")


# PCA 2 ---------------------------------------
pca_salsas2 <- prcomp(data, center = T, scale. = T)
print(pca_salsas2)
summary(pca_salsas2)

# Plots
plot(pca_salsas2, type = "l")
biplot(pca_salsas2, scale = 0)

# Save the values with the original data
c1 <- apply(pca_salsas2$rotation[,1]*data,1,sum)
c2 <- apply(pca_salsas2$rotation[,2]*data,1,sum)
salsas$c1 <- c1 
salsas$c2 <- c2 


# PCA 3 ---------------------------------------
pca_salsas3 <- princomp(data, cor=TRUE)
print(pca_salsas3)
names(pca_salsas3)
summary(pca_salsas3)


"CLUSTER"

# Import data
personas <- read_excel("C:/Users/Ruben Padilla/Downloads/Trabajos/Bases de datos/copy_db_analysis.xlsx", sheet="personas")
attach(personas)


# Hierarchical Cluster ----------------------------------
# Remove id column
data2 <- personas[-1]

# Normalization for zero mean and unit varianze
nor <- scale(data2)

# Euclidean distance matrix (EDM) for dissimilarity or similarity between pairs of observations
euclid <- dist(nor)

# Cluster
hierarchical <- hcut(euclid, k = 2, stand = TRUE)
hierarchical2 <- hclust(euclid, method = "ward.D")

# Dendogram
fviz_dend(hierarchical, rect = TRUE, cex = 0.5,
          k_colors = c("red","#2E9FDF"))
# Save
cluster.J <- cutree(hierarchical, k = 2)


# Non-Hierarchical Cluster ------------------------------
# Normalization
nor <- scale(data2)

# Euclidean distance matrix (EDM)
euclid <- dist(nor)

# Number of clusters
fviz_nbclust(nor, kmeans, method="wss")
fviz_nbclust(nor, kmeans, method="silhouette")
resnumclust <- NbClust(nor, distance="euclidean", method = "kmeans", index = "alllong")
fviz_nbclust(resnumclust)

# Kmeans
KM <- kmeans(nor, centers = 2, nstart = 25)
KM
KM$size

# Charts
fviz_cluster(KM, data = nor, ggtheme = theme_minimal())
fviz_cluster(KM, data = nor, ellipse.type = "euclid",repel = TRUE,star.plot = TRUE)
fviz_cluster(KM, data = nor, ellipse.type = "norm")
fviz_cluster(KM, data = nor, ellipse.type = "norm",palette = "Set2", ggtheme = theme_minimal())

# Save
cluster.KM <- KM$cluster

# Export of the dataset
datacluster <- cbind(personas, cluster.KM, cluster.J)
write.csv(datacluster,"datacluster.csv")


"ANOVA, MANOVA & FRIEDMAN"

# Import data
anova_data <- read_excel("C:/Users/Ruben Padilla/Downloads/Trabajos/Bases de datos/copy_db_analysis.xlsx",sheet="data1")
attach(anova_data)

# Plots
plotmeans(color~salsa)
plotmeans(sabor~salsa)
plotmeans(olor~salsa)
plotmeans(textura~salsa)

# Convert into factor for grouping
salsas <- factor(anova_data$salsa)
personas <- factor(anova_data$persona)

# Multivariate analysis of variance (MANOVA)
m1 <- manova(cbind(color,sabor,olor,textura)~salsas+personas, data = anova_data)
summary(m1)

# Anovas and tests for normality and homogeneity
summary.aov(m1)
m2 <- aov(color~salsas, data = anova_data)
m3 <- aov(sabor~salsas, data = anova_data)
m4 <- aov(olor~salsas, data = anova_data)
m5 <- aov(textura~salsas, data = anova_data)

shapiro.test(m2$residuals);shapiro.test(m3$residuals);shapiro.test(m4$residuals);shapiro.test(m5$residuals)
leveneTest(m2);leveneTest(m3);leveneTest(m4);leveneTest(m5)

# Friedman for significant differences between the means
friedman.test(y = sabor, groups = salsa, blocks = persona)
pairwise.wilcox.test(sabor, salsa, p.adj = "bonferroni", paried = TRUE)
x <- friedman(evaluation = color, trt = salsa, judge = persona)


"PROBIT (Only for the sauce number 3)"

# Import data
compra_data <- read_excel("C:/Users/Ruben Padilla/Downloads/Trabajos/Bases de datos/copy_db_analysis.xlsx",sheet="probit")

# Model
mprobit <- glm(compra ~ edad+factor(genero)+factor(feria)+factor(ingreso)+factor(prov),
               data = compra_data, family = binomial(link = "probit"))
summary(mprobit)
