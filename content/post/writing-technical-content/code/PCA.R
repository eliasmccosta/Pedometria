# Análise de componentes principais
# pacotes necessários
library(FactoMineR)
library(factoextra)
library(cluster)
library(survMisc)
library(ggfortify)
library(lfda)
library(Factoshiny)
dados <- read.csv("../data/dataset.csv"); dados=dados[2:54]; dados=dados[c(1:71),]
a = base::subset(dados,top==0)
row.names(a) <- 1:nrow(a)
aa =a[c(32:51)]
aa1 =a[c(31:51)]
res = PCA(aa, graph=F)
eig.val=get_eigenvalue(res)
eig.val

var<-get_pca_var(res)
ind<-get_pca_ind(res)

fviz_pca_var(res, col.var = "black")

Grupo <- as.factor(a[,31])
Grupo
fviz_pca_biplot(res, habillage = Grupo, title = "PCA UM", col.var = "black")

res_pca = PCAshiny(aa1)
# writexl::write_xlsx(a, "vai.xlsx") slavar arquivo de excel


correlationMatrix <- stats::cor(aa[,c(1,8,9,11,12,14,15,16,18, 19,20)]) # correlation matrix
# summarize the correlation matrix

print(correlationMatrix)
# find attributes that have greater than 0.85 correlation

highlyCorrelated <- caret::findCorrelation(correlationMatrix, cutoff=0.85)
# print indexes of highly correlated attributes

print(highlyCorrelated) 
# the covariates with the high correlation that were taken off from the model 
# were: CHNB, RSP, band1, band2, band3, SAVI).

# making a plot of correlations. Considering the significance (p-value)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(correlationMatrix, conf.level = .95)

col4 <- grDevices::colorRampPalette(c("#7F0000", "red", "#FF7F00", "yellow", "#7FFF7F",
                                      "cyan", "#007FFF", "blue", "#00007F"))

# draw ellipses + decorations
corrplot::corrplot(correlationMatrix, type="lower", method="circle",
                   tl.pos="lt", tl.col="black", tl.srt=90, col=col4(10),
                   p.mat = p.mat, sig.level = 0.05, insig = "pch", tl.cex=1.5, cl.cex=1.5)
# draw labels in black (disabling all the other stuff already drawn)
corrplot::corrplot(correlationMatrix, add=T, type="upper", method="number",
                   col="black", diag=F, tl.pos="n", cl.pos="n", number.cex=1.5, number.digits=2)


# material de youtube
# https://www.youtube.com/watch?v=hmp9KIPb5Ighttps://www.youtube.com/watch?v=hmp9KIPb5Ig
# https://www.youtube.com/watch?v=jOo0L-Jypyc
# https://www.youtube.com/watch?v=KqZAC4jyJKc