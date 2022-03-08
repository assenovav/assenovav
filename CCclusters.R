library(data.table)
library(tidyr)
library(ggplot2)
library(ggfortify)
library(GGally)
library(dbscan)
library(broom)
library(cluster)
library(e1071)
library(factoextra)
library(mclust)

options(scipen = 999)

creditcards <- fread("D:\\UNI - master\\Master Thesis\\CC GENERAL.csv")

# The distribution of monetary variables, such as GDP, annual salary or credit card balance, usually has a heavy right tale
# Thus, taking the logarithm is necessary to make the variable distribution more or less symmetrical
# Density based (parametric) clustering
creditcards[, `:=`(BALANCE = fifelse(BALANCE == 0, -4, log10(BALANCE)), 
                   PURCHASES = fifelse(PURCHASES == 0, -4, log10(PURCHASES)),
                   ONEOFF_PURCHASES = fifelse(ONEOFF_PURCHASES == 0, -4, log10(ONEOFF_PURCHASES)),
                   INSTALLMENTS_PURCHASES = fifelse(INSTALLMENTS_PURCHASES == 0, -4, log10(INSTALLMENTS_PURCHASES)),
                   CASH_ADVANCE = fifelse(CASH_ADVANCE == 0, -4, log10(CASH_ADVANCE)),
                   PAYMENTS = fifelse(PAYMENTS == 0, -4, log10(PAYMENTS)),
                   MINIMUM_PAYMENTS = fcase(MINIMUM_PAYMENTS == 0, -4, 
                                            is.na(MINIMUM_PAYMENTS), -4, 
                                            MINIMUM_PAYMENTS > 0, log10(MINIMUM_PAYMENTS)))]
creditcards <- na.omit(creditcards)

# Wide-to-long transformation
molten.creditcards <- melt(creditcards, id.vars = "CUST_ID")
mycolours <- c(rgb(1, 0, 0, alpha = 0.4), rgb(0, 1, 0, alpha = 0.4), rgb(0, 0, 1, alpha = 0.4), rgb(1, 1, 0, alpha = 0.4),
               rgb(1, 0, 1, alpha = 0.4), rgb(0, 1, 1, alpha = 0.4), rgb(0, 0, 0, alpha = 0.4))
ggplot(data = molten.creditcards[variable %in% c("BALANCE", "PURCHASES", "ONEOFF_PURCHASES", "INSTALLMENTS_PURCHASES",
                                                 "CASH_ADVANCE", "PAYMENTS", "MINIMUM_PAYMENTS")], 
       aes(x = variable, y = value, fill = variable)) + 
  geom_violin(draw_quantiles = 0.5) + 
  scale_fill_manual(values = mycolours) +
  ylim(-1, 5) + ylab("log10(value)") + theme(axis.text.x = element_blank())

# Principal component analysis
creditcardsPCA <- prcomp(creditcards[, 2:18], scale. = TRUE)
plot(creditcardsPCA, main = "PCA of Credit Card Data")

# Variance explained
cumsum(creditcardsPCA$sdev ^ 2) / sum(creditcardsPCA$sdev ^ 2)

# GMM <- Mclust(prcomp(creditcards[, 2:13], scale. = TRUE)$x[, 1:5], G = 2:5)
# summary(GMM)

# Density-based (nonparametric) clustering
par(mfrow = c(2, 2))
DenseClust100 <- hdbscan(creditcardsPCA$x[, 1:4], minPts = floor(nrow(creditcards) / 100))
plot(DenseClust100, gradient = c("red", "yellow", "green"), show_flat = TRUE)

DenseClust300 <- hdbscan(creditcardsPCA$x[, 1:4], minPts = floor(nrow(creditcards) / 300))
plot(DenseClust300, gradient = c("red", "yellow", "green"), show_flat = TRUE)

DenseClust1000 <- hdbscan(creditcardsPCA$x[, 1:4], minPts = floor(nrow(creditcards) / 1000))
plot(DenseClust1000, gradient = c("red", "yellow", "green"), show_flat = TRUE)

# Plotting clusters
PlotDC <- data.table(creditcardsPCA$x[, 1:4], 
                        Cluster = factor(DenseClust100$cluster), Membership = DenseClust100$membership_prob)
mycolours2 <- c(rgb(0.8, 0, 0), rgb(0, 0.8, 0), rgb(0, 0, 0.8), rgb(0.8, 0.8, 0),
                rgb(0.8, 0, 0.8), rgb(0, 0.8, 0.8), rgb(0, 0, 0), rgb(1, 0.5, 0.5), 
                rgb(0.5, 1, 0.5), rgb(0.5, 0.5, 1))
ggplot(data = PlotDC[Membership > 0.01], aes(x = PC1, y = PC2, col = Cluster, alpha = Membership)) + 
  geom_point() + scale_colour_manual(values = mycolours2) + scale_fill_manual(values = mycolours2)

ggpairs(data = PlotDC[Membership > 0.01], columns = 1:4, aes(col = Cluster, alpha = Membership), upper = list(continuous = "blank")) + 
  scale_colour_manual(values = mycolours2) + scale_fill_manual(values = mycolours2)

# Adding cluster numbers to the original dataframe
creditcards <- data.table(creditcards, ClusterDC = factor(DenseClust100$cluster), 
                          MembershipDC = DenseClust100$membership_prob)

# Variable medians by cluster
ClustSummary <- list()
for(i in 1:5) {
  ClustSummary[[i]] <- as.data.table(summary(creditcards[ClusterDC == i, 2:18]))[, 2:3][, ClusterDC := i]
  setnames(ClustSummary[[i]], new = c("Variable", "Statistic", "Cluster"))
  ClustSummary[[i]] <- separate(ClustSummary[[i]], 2, into = c("Statistic", "Value"), sep = ":")
  ClustSummary[[i]] <- dcast(ClustSummary[[i]], Cluster + Variable ~ Statistic, value.var = "Value")
}

ClustSummary <- rbindlist(ClustSummary)
setcolorder(ClustSummary, c("Cluster", "Variable", "Min.   ", "1st Qu.", "Median ", "Mean   ", "3rd Qu.", "Max.   "))
dcast(ClustSummary[, .(Cluster, Variable, Mean = as.numeric(`Mean   `))], Variable ~ Cluster, value.var = "Mean")

# Compute and plot average silhouette for k = 2 to k = 15
silhouette_score <- function(k) {
  cm <- cmeans(creditcardsPCA$x[, 1:4], centers = k)
  ss <- silhouette(cm$cluster, dist(creditcardsPCA$x[, 1:4]))
  mean(ss[, 3])
}
k <- 2:15
avg.sil <- sapply(k, silhouette_score)

plot(k, avg.sil, type = "b", pch = 19,  
     xlab = "Number of clusters", ylab = "Average Silhouette Scores",
     col = ifelse(avg.sil == max(avg.sil), "red", "black") , frame=FALSE)

cm5 <- cmeans(creditcardsPCA$x[, 1:4], 5)

PlotCM <- data.table(creditcardsPCA$x[, 1:4], 
                     Cluster = factor(cm5$cluster), Membership = apply(cm5$membership, 1, max))
ggplot(data = PlotCM[Membership > 0.6], aes(x = PC1, y = PC2, col = Cluster, alpha = Membership)) + 
  geom_point() + scale_colour_manual(values = mycolours2) + scale_fill_manual(values = mycolours2)

ggpairs(data = PlotCM[Membership > 0.6], columns = 1:4, aes(col = Cluster, alpha = Membership), upper = list(continuous = "blank")) + 
  scale_colour_manual(values = mycolours2) + scale_fill_manual(values = mycolours2)

# Adding cluster numbers to the original dataframe
creditcards <- data.table(creditcards, ClusterCM = PlotCM$Cluster, 
                          MembershipCM = PlotCM$Membership)

ClustSummary <- list()
for(i in 1:5) {
  ClustSummary[[i]] <- as.data.table(summary(creditcards[ClusterCM == i, 2:18]))[, 2:3][, ClusterCM := i]
  setnames(ClustSummary[[i]], new = c("Variable", "Statistic", "Cluster"))
  ClustSummary[[i]] <- separate(ClustSummary[[i]], 2, into = c("Statistic", "Value"), sep = ":")
  ClustSummary[[i]] <- dcast(ClustSummary[[i]], Cluster + Variable ~ Statistic, value.var = "Value")
}

ClustSummary <- rbindlist(ClustSummary)
setcolorder(ClustSummary, c("Cluster", "Variable", "Min.   ", "1st Qu.", "Median ", "Mean   ", "3rd Qu.", "Max.   "))
dcast(ClustSummary[, .(Cluster, Variable, Mean = as.numeric(`Mean   `))], Variable ~ Cluster, value.var = "Mean")

### Gaussian Mixtures
gausm <- Mclust(creditcardsPCA$x[, 1:4], 1:15)
gausm$BIC
gausm$uncertainty  <- 1 - gausm$uncertainty

gausm <- Mclust(creditcardsPCA$x[, 1:4], 5)
gausm$uncertainty  <- 1 - gausm$uncertainty

PlotGM <- data.table(creditcardsPCA$x[, 1:4], 
                     Cluster = factor(gausm$classification), InvUncertainty = gausm$uncertainty)

ggplot(data = PlotGM[InvUncertainty > 0.96], aes(x = PC1, y = PC2, col = Cluster, alpha = InvUncertainty)) + 
  geom_point() + scale_colour_manual(values = mycolours2) + scale_fill_manual(values = mycolours2)

ggpairs(data = PlotGM[InvUncertainty > 0.96], columns = 1:4, 
        aes(col = Cluster, alpha = InvUncertainty), upper = list(continuous = "blank")) +
  scale_colour_manual(values = mycolours2) + scale_fill_manual(values = mycolours2)
