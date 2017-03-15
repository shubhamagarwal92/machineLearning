# data("UCBAdmissions")
# View(UCBAdmissions)
# X<-as.data.frame(UCBAdmissions)
# D <- dist(X)
# resultChar <- hclust(D)
# plot(resultChar)

data("USArrests")
X = USArrests
D <- dist(X)

# K-means algorithm
resultKmeans = kmeans(X,2)
plot(X,col = resultKmeans$cluster)

# Hierarchical Clustering
resultHC <- hclust(D)
plot(resultHC,hang=-1)

# PAM Clustering
help.search("pam")
library(cluster)
?pam
resultPAM = pam(D,2)
plot(resultPAM)
# 0.58 and 0.6 looks good 
# Silhouette width negative means bad clusters. 
# -1 <= Sil(x) = (b-a)/ max(b,a) <= 1


help.search("pam")
# Self Organizing Maps
library(kohonen)
?som
boxplot(X)
## Variance too high, better to scale
XS<-scale(X)
boxplot(XS)
grid=somgrid(3,3,"rectangular")
plot(grid)
# resultSOM = som(XS,grid=grid)
resultSOM = som(scale(USArrests),grid=somgrid(3,3,"rectangular"))
plot(resultSOM)
som.hc <- cutree(hclust(dist(resultSOM$codes)),9)
add.cluster.boundaries(resultSOM,som.hc)
som.hc <- cutree(hclust(dist(resultSOM$codes)),3)
add.cluster.boundaries(resultSOM,som.hc)

# Spectral Clustering
# install.packages("kernlab")
library(kernlab)
X1 = as.matrix(X)
resultSpec = specc(X1,centers=3)
plot(resultSpec)

### Non-linear separable clusters
data("spirals")
spirals

# Visualizing Clusters
sc <- specc(spirals,centers=2)
plot(spirals,col = sc)

# K-means algorithm
resultKmeans = kmeans(spirals,2)
plot(spirals,col = resultKmeans$cluster)


## Kernels used linear, RBF and polynomial for kernel clustering