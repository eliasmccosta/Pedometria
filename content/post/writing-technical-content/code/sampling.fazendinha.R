######                                    ######
###### Princípios da Amostragem Espacial   #####
######                                    ######
## Carregar os pacotes
library(sp)
library(raster)
library(sampling)
library(rgdal)
# Carregando a base de dados da fazendinha
dados <- read.csv("../data/dataset.csv"); dados=dados[2:54]
grid <- read.csv("../data/grid.csv"); grid=grid[2:23]

# esse cÃ³digo pode ser obtido em software como o QGIS
crsSIRGAS2000=CRS("+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# Fazendinha
limite <- maptools::readShapeSpatial("../Shape/limite_elias.shp", 
                                     proj4string=crsSIRGAS2000,verbose=TRUE); plot(limite)


# criando um cluster de unidades homogêneas usando os dados das covariáveis ambientais
set.seed(2018)
cluster <- kmeans(grid[, c(1,11,9,15,20)],10)
cluster = data.frame(cluster$cluster)
grid=cbind(grid,cluster)

# Foi necessário transformar os estratos para fator porque estava dando erro quando calculava o número
# de amostras por estrato 
grid$cluster.cluster=as.factor(grid$cluster.cluster) 

#cluster=data.frame(grid[,c("X","Y")],results=cluster$cluster)
#coordinates(cluster)= ~ X+Y; gridded(cluster)=T; cluster=raster(cluster)
#plot(cluster)


# Transformar o dataframe do grid em spatialdataframe
sp::gridded(grid) <- ~ X + Y
sp::proj4string(grid) <- sp::CRS("+proj=utm +zone=23 +south +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"); # (colocando sistema de referencia no shape de pontos)

####                              ######
####    Amostragem Probabilistica ######
####                              ######
### Amostragem Aleatória Simples
plot(
  grid@coords, type = "n", asp = 1, main = "Fazendinha",
  xlab = "Longitude (m)", ylab = "Latitude (m)")
image(
  grid, "cluster.cluster", col = terrain.colors(10), axes = TRUE, add = TRUE)

main <- "Simple Random Samples"
plot(
  grid@coords, type = "n", asp = 1, main = main,
  xlab = "Longitude (m)", ylab = "Latitude (m)")
image(
  grid, "cluster.cluster", col = terrain.colors(10), axes = TRUE, add = TRUE)
set.seed(2019)
pts <- grid[sampling::srswr(30, length(grid)) == 1, ]
set.seed(2019)
pts@coords <- 
  pts@coords + matrix(runif(prod(dim(pts@coords)), min = -0.5, max = 0.5), ncol = 2) * 
  grid@grid@cellsize
points(pts, pch = 21, cex = 0.75)
leg <- paste("Samples (n = ", length(pts), ")", sep = "")
legend(636120, 7483400, legend = leg, pch = 21, bty = "n")
plot(limite, add = TRUE)

ID=c(1,2,3,4,5,6,7,8,9,10,
     11,12,13,14,15,16,17,18,19,20,
     21,22,23,24,25,26,27,28,29,30)
pts@data$ID=ID
writeOGR(pts, ".", "../Shape/random.sampling", driver="ESRI Shapefile")

## Amostragem Aleatória Estratificada Simples
main <- "Simple Stratified Random Sample"
plot(
  grid@coords, type = "n", asp = 1, main = main,
  xlab = "Longitude (m)", ylab = "Latitude (m)")
image(
  grid, "cluster.cluster", col = terrain.colors(10), axes = TRUE, add = TRUE)
legend(
  "topleft", title = "Strata", fill = terrain.colors(10),
  legend = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"),
  border = terrain.colors(10), bty = "n")
n <- round(30 * summary(grid$cluster.cluster) / length(grid))
set.seed(2019)
pts <- sampling::strata(
  grid[order(grid$cluster.cluster), ], stratanames = "cluster.cluster", size = n, 
  method = "srswr")$ID_unit
pts <- grid[order(grid$cluster.cluster), ][pts, ]
set.seed(2019)
pts@coords <- pts@coords + 
  matrix(runif(prod(dim(pts@coords)), min = -0.5, max = 0.5), ncol = 2) * 
  grid@grid@cellsize
points(pts, pch = 21, cex = 0.75)
leg <- paste("Samples (n = ", length(pts), ")", sep = "")
legend(636120, 7483400, legend = leg, pch = 21, bty = "n")
plot(limite, add = TRUE)
ID=c(1,2,3,4,5,6,7,8,9,10,
     11,12,13,14,15,16,17,18,19,20,
     21,22,23,24,25,26,27,28,29,30)
pts@data$ID=ID
writeOGR(pts, ".", "../Shape/strata.sampling", driver="ESRI Shapefile")

