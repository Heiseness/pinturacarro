#Codgio completo
#Paqueterias
Packages <- c("raster", "png","imager",
              "dplyr","caret","readr","OpenImageR", "jpeg")

lapply(Packages, library, character.only = TRUE)
setwd("C:/Users/gamed/Desktop/PROJECT REDD/4.0/colores/")

#Iniciamos desarollanddo nuestras funciones.

limpio <- function(r){
  if(r==1){
    par(mfrow=c(1,1))
    d <- as.data.frame(im)
    m <- sample_n(d,1e4) %>% lm(value ~ x*y,data=.)
    im.c <- im-predict(m,d)
    img <- threshold(im.c)
    
    plot(img, main="Imagen usando un modelo lineal")
  }else{
    if(r==2){
      img <- threshold(im)
      par(mfrow=c(1,1))
      plot(img)
    }
  }
} #Solo proyecta el pixset seleccionado

dibujado <- function(im){
  par(mfrow=c(2,2))
  plot(im, main="imagen original")
  plot(threshold(im), main = "imagen con threshold")
  
  d <- as.data.frame(im)
  m <- sample_n(d,1e4) %>% lm(value ~ x*y,data=.)
  im.c <- im-predict(m,d)
  img <- threshold(im.c)
  
  plot(img, main="Imagen usando un modelo lineal")
} #dibuja las distintas alternativas

caracteristicas <- function(pix) {
  
  largo <- as.numeric(length(pix))
  
  y_min <<- min( pix[[1]][["y"]] )
  y_max <<- max( pix[[1]][["y"]] )
  x_min <<- min( pix[[1]][["x"]] )
  x_max <<- max( pix[[1]][["x"]] )
  
  for (i in 1:largo){
    if ( y_min > min( pix[[i]][["y"]] ) ) {
      y_min <<- min(pix[[i]][["y"]])
    }
    if ( y_max < max( pix[[i]][["y"]]) ) {
      y_max <<-  max( pix[[i]][["y"]] )
    }
    if ( x_min > min( pix[[i]][["x"]] ) ) {
      x_min <<- min(pix[[i]][["x"]])
    }
    if ( x_max < max( pix[[i]][["x"]]) ) {
      x_max <<-  max( pix[[i]][["x"]] )
    }
  }
  
  #Aqui acaba el for
  dist_x <<- ceiling(x_max-x_min)
  dist_y <<- ceiling(y_max-y_min)
  
  print(paste0("El largo del carro en pixeles es aproximadamente de: ", dist_x))
  print(paste0("Lo alto del carro en pixeles es aproximadamente de: ", dist_y))
  
  dim_x <- (dist_x/110)*0.401 #calculo de las distacias x/y
  dim_y <- (dist_y/110)*0.401
  
  print(paste0("El largo del carro en metros es aproximadamente de: ", dim_x))
  print(paste0("Lo alto del carro en metros es aproximadamente de: ", dim_y))
  
  car_wt <- as.numeric( modelo0$coefficients[1]+
                          (modelo0$coefficients[2]*car_cyl)+(modelo0$coefficients[3]*dim_x))
  
  
  car_weight <- as.numeric(modelo1$coefficients[1] 
                           + (modelo1$coefficients[2]*dim_x)
                           + (modelo1$coefficients[3]*car_wt))  #regresion para predecir el peso
  
  print(paste0("Lo alto del carro en metros es aproximadamente de: ", car_weight))
  
  car_am <- sample(0:1,1,replace=T) #Transmision
  
  car_vs <- 0
  car_carb <- 0
  car_cyl <- 0 
  car_gear <- 0 
  car_mpg <- 0
  car_disp <- 0
  car_qsec <- 0
  car_drat <- 0
  car_hp <- 0
  
  if(car_cyl == 0){ #cilindarje y tipo de motor
    car_cyl <- 4
    car_vs <- 1
  }else{
    car_cyl <- 8
    car_vs <- sample(0:1,1,replace=T) #Se hace un random por que hay motores
                #de 8 con motor recto, pero no hay de 4 con motor en v
  }
  
  car_mpg <-  as.numeric(modelo_mpg$coefficients[1] +
    (modelo_mpg$coefficients[2]*car_wt) + (modelo_mpg$coefficients[3]*car_cyl))
  
  car_disp <- as.numeric(modelo_disp$coefficients[1]+
    (modelo_disp$coefficients[2]*car_cyl) + (modelo_disp$coefficients[3]*car_wt))
 
  car_hp <- as.numeric(modelo_hp$coefficients[1]+
    (modelo_hp$coefficients[2]*car_cyl)+(modelo_hp$coefficients[3]*car_disp))
  
  car_drat <- as.numeric(modelo_drat$coefficients[1]+
    (modelo_drat$coefficients[2]*car_disp)+(modelo_drat$coefficients[3]*car_cyl))

  car_qsec <- as.numeric(modelo_qsec$coefficients[1]+
    (modelo_qsec$coefficients[2]*car_hp)+(modelo_qsec$coefficients[3]*car_vs))
  
  car_gear <- sample(3:5,1,replace=T)
 
  car_carb <- sample(1:8,1,replace=T)

  valores <- c(dim_x,dim_y,car_weight,car_mpg,car_cyl,
             car_disp,car_hp,car_drat,car_wt,car_qsec,
             car_vs,car_am,car_gear,car_carb)
  
  carros <<- rbind(carros,valores)
  
}

#Construccion del modelo predicitivo.

mtcars <- read_csv("mtcars.csv") #Cargamos el dataset con los 

modelo0 <- lm(mtcars$wt~mtcars$cyl+mtcars$Largo)

modelo1 <- lm(mtcars$peso~
                mtcars$Largo+mtcars$wt)#Creamos nuestro modelo de regresion lineal.

modelo_mpg <- lm(mtcars$mpg~
                   mtcars$wt+mtcars$cyl)
modelo_disp <- lm(mtcars$disp~
                   mtcars$cyl+mtcars$wt)
modelo_hp <- lm(mtcars$hp~
                  mtcars$cyl+mtcars$disp)
modelo_drat <- lm(mtcars$drat~
                  mtcars$disp+mtcars$cyl)
modelo_qsec <- lm(mtcars$qsec~
                    mtcars$hp+mtcars$vs)

#Descargamos la imagen a utilizar
dir_im <- "./MBDTF.jpg"

im <- load.image(dir_im) #imagen og 

plot(im) #desplegamos imagen en plots
px <- (isoblur(im,9)  > .22) #Ajustamos los niveles de brillo
highlight(px) #Reslatamos el pixset
px_data <- (highlight(px)) #Almacenamos la forma del pixset en datos

dist_x <- 0
dist_y <- 0

carros <- (mtcars[,2:15])

caracteristicas(px_data) #Aplicamos la funcion para las dimensiones
