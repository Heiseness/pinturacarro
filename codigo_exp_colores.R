#costo del color
library(jpeg)
library(raster)
library(rgdal)
library(OpenImageR)
library(imager)

#setwd para la carpeta donde esta el proyecto
setwd("C:/Users/gamed/Desktop/PROJECT REDD/4.0/colores/")
dir_im <- "./MBDTF.jpg"

createPal <- function(photo, met = 1, graph = TRUE, k = 9){
  if(met == 1){
    colR <- getValues(raster(photo, band = 1))
    colG <- getValues(raster(photo, band = 2))
    colB <- getValues(raster(photo, band = 3))
    kMeans <<- kmeans(data.frame(colR, colG, colB), centers = k)
    kCol <- rgb(kMeans$centers, maxColorValue = 255)[order(table(
      kMeans$cluster), decreasing = TRUE)]
    if(graph == TRUE){
      op <- par(no.readonly = TRUE)
      par(mfrow = c (1, 2), mar = c(0, 2, 2, 0))
      myJpg <- readJPEG(photo, native = TRUE)
      plot(0:1, 0:1, type = "n", ann = FALSE, axes = FALSE)
      rasterImage(myJpg, 0, 0, 1, 1)
      barplot(table(kMeans$cluster)[order(table(kMeans$cluster), 
                                          decreasing = TRUE)], col = kCol, names.arg = NA)
      par(op)
    }
    return(kCol)
  } else {
    if(met == 2){
      kColR <- kmeans(x = getValues(raster(photo, band = 1)), 
                      centers = k)
      kColG <- kmeans(x = getValues(raster(photo, band = 2)), 
                      centers = k)
      kColB <- kmeans(x = getValues(raster(photo, band = 3)), 
                      centers = k)
      kCol <- (rgb(kColR$centers, kColG$centers, kColB$centers,
                   maxColorValue = 255))
      if(graph == TRUE){
        op <- par(no.readonly = TRUE)
        par(mfrow = c (1, 2), mar = c(0, 2, 2, 0))
        myJpg <- readJPEG(photo, native = TRUE)
        plot(0:1, 0:1, type = "n", ann = FALSE, axes = FALSE)
        rasterImage(myJpg, 0, 0, 1, 1)
        plot(x = 1:k, y = rep(1, k), ylim = c(0, 1), 
             xlim = c(0, k), axes = FALSE, xlab = "", 
             ylab = "", type = "n")
        for(i in 1:k){
          polygon(x = c(i-1, i, i, i-1), y = c(0, 0, 1, 1), 
                  col = kCol[i])
          text(x = i - 0.5, y = 0.5, 
               labels = as.character(kCol[i]), srt = 90)
        }
        par(op)
      }
      return(kCol)
    } else {
      print(paste0("No method ", met, "."))
      return(rgb(0, 0, 0))
    }
  }
}

kMeans <- 0
myPalMet1 <- createPal(photo = dir_im, 
                       met = 1, graph = TRUE, k = 5)
costo_total <- 0

ans <- as.numeric(readline(prompt = "Cual es la posicion del 
                           color del carro en la paleta 
                           (ingrese un numero del 1 al 5)"))

ans_col <- (col2rgb(myPalMet1[ans]))

#Ordenamos el vector de tamanios del color
vector_k <- c(kMeans$size)
vector_k <- sort(vector_k, decreasing = TRUE)
vector_k

#sacamos el costo total
for (i in 1:3) {
  dif_color <- (255 - as.numeric(ans_col[i,]))
  costo_total <- (costo_total) + (23400 - dif_color*(16700/255))
}

#sacamos las superficie, segun la cantidad de pixeles.
superficie <- vector_k[ans]
superficie <- (superficie*0.440)/10000

texto <- "hola"

if(superficie < 8){
  texto <- ("por lo tanto, tomara un litro o menos de pintar 
por pasada")
}else{
  texto <- ("por lo tanto, va a tomar mas de un litro para 
pintarse por pasada")
}


cat("
              [RESULTADO DEL ANALISIS]
El costo para pintar el carro seria de ",costo_total," pesos.
La supericie mostrada en la foto es de ", superficie, "metros 
cuadados ",texto)





