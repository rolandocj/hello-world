### 15-05
#modificacion para la rama develop

######           Proyecto 1       #######
##      ESTADISTICA MULTIVARIADA      ##

#Se asigna el working directory
dir <- "/home/rolando/Dropbox/Semestre 19-2/Estadística Multivariada/Proyecto final"
setwd(dir)
setwd("~/Maestria/CIMAT/2 Semestre/Multivariado/Proyecto1/Proyecto1")
#se leen los datos
pizzas<-read.csv("pizzas.csv")
pizzas<-read.csv("pizzas.xls")
#Nombres de las columnas
names(pizzas)
names(pizzas)<-(c("Id","Humedad","Proteina","Grasa","Ceniza","Sodio","Carbohidratos","Calorias","Marca"))
#Fijamos los nombres como variables
attach(pizzas)
#Analisis de la variable Id
sort(Id)
length(Id)
max(Id)-min(Id)
plot(1:length(Id),sort(Id))
#Tres saltos relevantes, checar si estan relacionados
#con alguna otra variable. Puede ser por marca o algún tipo
summary(Id)


#Analisis...



#EN EL PDF:

#Cada
#una de las pizzas se convirtio en pure y se mezclo a conciencia, despues de lo cual se tomo una muestra
#de la mezcla para realizar un analisis de las sustancias nutritivas.


#investigar a fondo el proceso y buscar referencias a estos procesos
#¿como se tomo lo muestra?

#preguntar del origen de la base de datos

###############
#?datos atipicos
#summary
library(ggplot2)
#Para graficar PCA con ggplot
#install.packages("ggfortify")
library(ggfortify)
#para LDA
library(MASS)
#para multilogit
library(nnet)

pizzas <- na.omit(pizzas)

COLOR <- rainbow(length(levels(Marca)))
color.marca <- COLOR[Marca]
#considerar Marca como factor
pizzas[, "Marca"] <- factor(pizzas[, "Marca"])


### PCA ###
##2 componentes
vars.pca <- c("Humedad","Proteina","Grasa","Ceniza","Sodio","Carbohidratos","Calorias")
pizzas.pca <- prcomp(pizzas[,vars.pca], 
                     center = TRUE, scale = TRUE)

#screplot
screeplot(pizzas.pca)

#loadings
pizzas.pca$rotation[,1:2]

#grafica sencilla 
ggplot() +
  geom_text(data = data.frame(pizzas.pca$x[,1:2]),
            aes(x = PC1, y = PC2, colour = Marca),
            label = Marca, alpha = 1,
            size = 2, check_overlap = TRUE)

#biplot del PCA
autoplot(pizzas.pca, data = pizzas, alpha = 0,
         loadings = TRUE, loadings.colour = 'red',
         loadings.label = TRUE, loadings.label.size = 3,
         loadings.label.colour = 'black') +
  geom_text(aes(colour = Marca), label=Marca, 
            size = 2, vjust=-1,
            check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="PCA con datos normalizados")


### ANALISIS FACTORIAL ###
n.factores <- 2
vars.fa <- vars.pca
#promax
pizzas.fa <- factanal(x = pizzas[,vars.fa],factors = n.factores,scores="Bartlett",
                      rotation = "promax")
#varimax
pizzas.fa <- factanal(x = pizzas[,vars.fa],factors = n.factores,scores="Bartlett")
pizzas.fa

#hacer una tabla con comunalidades y varianzas especificas
(cargas <-pizzas.fa$loadings) # Cargas factoriales
(var.esp <-pizzas.fa$uniquenesses) # Singularidades

#aproximacion a la matriz de correlacion
pred_vc <- cargas%*%t(cargas)+diag(var.esp)
round(cor(pizzas[,vars.fa]) - pred_vc,digits=3)

#grafica de factanal
autoplot(pizzas.fa, pizzas = state.x77, alpha = 0,
         loadings = TRUE, loadings.label = TRUE) +
  geom_text(aes(colour = Marca), label=Marca, 
            size = 2, vjust=-1,
            check_overlap = TRUE) +
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title="Análisis factorial")

### LDA ###
vars.lda<- vars.pca
set.seed(123)
muestra.train <- sample(1:nrow(pizzas), nrow(pizzas)/2)
muestra.test <- setdiff(1:nrow(pizzas), muestra.train)

#datasets para train y test, considerar usar otra forma de validación
pizzas.train <- data.frame(pizzas[muestra.train,vars.lda], Marca = Marca[muestra.train])
pizzas.test <- data.frame(pizzas[muestra.test,vars.lda], Marca =  Marca[muestra.test])

#.+.+.+.+.+.+.+   LDA   .+.+.+.+.+.+.+#
pizzas.lda <- lda(Marca ~ ., data = pizzas.train)
summary(pizzas.lda)

pizzas.train.pred.lda <- predict(pizzas.lda, pizzas.train)
pizzas.test.pred.lda <- predict(pizzas.lda, pizzas.test)

#Error de entrenamiento
MC.train.lda <- table(pizzas.train$Marca,pizzas.train.pred.lda$class)
MC.train.lda
error.train.lda  <- 1-sum(diag(MC.train.lda))/sum(MC.train.lda)
error.train.lda

#Error de prueba
MC.test.lda <- table(pizzas.test$Marca,pizzas.test.pred.lda$class)
MC.test.lda
error.test.lda  <- 1-sum(diag(MC.test.lda))/sum(MC.test.lda)
error.test.lda

#.+.+.+.+.+.+.+   MULTILOGIT   .+.+.+.+.+.+.+#
pizzas.log <- multinom(Marca ~ ., data = pizzas.train, MaxNWts = 1500)
summary(pizzas.log)

pizzas.train.pred.log <- predict(pizzas.log, pizzas.train)
pizzas.test.pred.log <- predict(pizzas.log, pizzas.test)

#Error de entrenamiento
MC.train.log <- table(pizzas.train$Marca,pizzas.train.pred.log)
MC.train.log
error.train.log  <- 1-sum(diag(MC.train.log))/sum(MC.train.log)
error.train.log

#Error de prueba
MC.test.log <- table(pizzas.test$Marca,pizzas.test.pred.log)
MC.test.log
error.test.log  <- 1-sum(diag(MC.test.log))/sum(MC.test.log)
error.test.log


############## ESTA PARTE LA COPIE DEL CODIGO DE BENITO ################
################# TODAVIA ESTOY VIENDO LO QUE ES RESCATABLE ########

# Practica 4. Aplicacion de analisis de factores a un conjunto de datos con el fin 
# de reducir la dimensionalidad de los datos
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#  Librerias necesarias
#------------------------------------------------------------------------
library("MASS")
library("alr3")
library("stats")
library("scatterplot3d")
library("psych")

#------------------------------------------------------------------------
#------------------------------------------------------------------------
#   Analisis factorial con el metodo de estimacion de MV. 
#------------------------------------------------------------------------
#------------------------------------------------------------------------
# Se revisa la correlacion entre las variables para ver si vale la pena realizar 
# analisis de factores
pizzas.cor <- cor(pizzas[,vars.fa])

#------------------------------------------------------------------------
# Prueba de esfericidad de Bartlett para probar la hipotesis nula de que las 
# variables no estan correlacionadas. La idea es rechazar la hipotesis nula
# para proseguir con un analisis de factores
cortest.bartlett(pizzas.cor, n=nrow(pizzas))
#------------------------------------------------------------------------
# Indice KMO
# - KMO > 0.90    Muy bueno
# - 0.80<KMO<0.90 Bueno
# - 0.70<KMO<0.80 Aceptable
# - 0.60<KMO<0.70 Regular
# - 0.50<KMO<0.60 Malo
# - KMO < 0.50    Inaceptable
KMO(pizzas[,vars.fa])

#------------------------------------------------------------------------
# Se realiza un analisis de factores utilizando maxima verosimilitud para estimar 
# los parametros del modelo (las cargas y las varianzas especificas).
# Por default el analisis de factores se realiza sobre los datos estandarizados z 
# y utilizando la rotacion varimax
# Se prueba la solucion con un factor (m=2)
n.factores <- 2
vars.fa <- vars.pca
pizzas.fa <- factanal(x = pizzas[,vars.fa],factors = n.factores,scores="Bartlett",
                      rotation = "promax")
#varimax
pizzas.fa <- factanal(x = pizzas[,vars.fa],factors = n.factores,scores="Bartlett")
pizzas.fa

var_vend.fa2<- factanal(var_vend,factors=2)
(cargas <-pizzas.fa$loadings) # Cargas factoriales
(var.esp <-pizzas.fa$uniquenesses) # Singularidades
(prueba_hip <-pizzas.fa$STATISTIC) # Prueba de hipotesis

#------------------------------------------------------------------------
# Se calcula la diferencia entre las correlaciones observadas y las predichas 
# para m=2 factores
# Primero se obtiene la estimacion de la matriz de correlaciones
pred_vc <- cargas%*%t(cargas)+diag(var.esp)
round(cor(pizzas[,vars.fa]) - pred_vc,digits=3)

#------------------------------------------------------------------------
# Se calculan los factor scores con el modelo factorial para m=3
# ojo:cuando se deseen obtener los factor scores, se debe utilizar como datos 
# de entrada la matriz original de datos y no la matriz de covarianzas
pizzas.fa <- factanal(x = pizzas[,vars.fa],factors = 3,scores="Bartlett")

scores_Bar<-pizzas.fa$scores
scores_Bar
#corregir
scores_reg <- scores_Bar

#------------------------------------------------------------------------
# Se añaden los nombres de los casos en los factor scores
#dimnames(scores_reg)[[1]]<-nombre_ven

#------------------------------------------------------------------------

#se grafican los casos de acuerdo a los factor scores
scatterplot3d(scores_Bar, angle=45, col.grid="lightblue", color = as.numeric(Marca),
              main="Grafica de los factor scores (Bartlett)", pch=20)

#se genera la matriz con  los graficos de los factor scores tomando dos factores a la vez

pairs(scores_reg, col="blue")
pairs(scores_Bar, col="blue")
#------------------------------------------------------------------------
#se genera cada uno de los graficos de los factor scores

#f1 x f2
par(pty="s")
plot(scores_reg[,1],scores_reg[,2],
     ylim=range(scores_reg[,1]),
     xlab="Factor 1",ylab="Factor 2",type="n",lwd=2)
text(scores_reg[,1],scores_reg[,2], col = "red",
     labels=abbreviate(row.names(scores_reg),minlength=8),cex=0.6,lwd=2)
grid()

#f1 x f3
par(pty="s")
plot(scores_reg[,1],scores_reg[,3],
     ylim=range(scores_reg[,1]),
     xlab="Factor 1",ylab="Factor 3",type="n",lwd=2)
text(scores_reg[,1],scores_reg[,3], col = "red",
     labels=abbreviate(row.names(scores_reg),minlength=8),cex=0.6,lwd=2)
grid()

#f2 x f3
par(pty="s")
plot(scores_reg[,2],scores_reg[,3],
     ylim=range(scores_reg[,2]),
     xlab="Factor 2",ylab="Factor 3",type="n",lwd=2)
text(scores_reg[,2],scores_reg[,3], col = "red",
     labels=abbreviate(row.names(scores_reg),minlength=8),cex=0.6,lwd=2)
grid()

#------------------------------------------------------------------------
# Analisis factorial con el metodo de Componentes Principales
#------------------------------------------------------------------------
#------------------------------------------------------------------------
#  Librerias necesarias
#------------------------------------------------------------------------
library("ade4")
library("factoextra")
#------------------------------------------------------------------------
# USamos la funcion 'dudi.pca' para realizar el PCA

pizzas.pca2 <- dudi.pca(pizzas[,vars.pca], scannf = FALSE, nf = 2)

#------------------------------------------------------------------------
# Extraemos los autovalores
summary(pizzas.pca2)

eig.val <- get_eigenvalue(pizzas.pca2)
head(eig.val)

#------------------------------------------------------------------------
# Grafico de los autovalores usando la funcion 'screeplot'
screeplot(pizzas.pca2, main ="Grafico - Autovalores")

#------------------------------------------------------------------------
# También podemos personalizar el gráfico utilizando la función estándar 
# 'barplot()'. Dibujaremos tambien el porcentaje de varianzas retenidas 
# por cada componente:
barplot(eig.val[, 2], names.arg=1:nrow(eig.val), 
        main = "Varianzas",
        xlab = "Componentes Principales",
        ylab = "Porcentaje de varianzas",
        col ="steelblue")
# Agregamos lineas de conexion a las barras
lines(x = 1:nrow(eig.val), eig.val[, 2], 
      type="b", pch=19, col = "red")

#------------------------------------------------------------------------
# Podemos hacer el mismo grafico usando la funcion 'fviz_screeplot' del
# paquete factoextra
fviz_screeplot(pizzas.pca2, ncp=7)

#------------------------------------------------------------------------
# Las coordenadas de las variables en el mapa de factores son
head(pizzas.pca2$co)
# Grafico
s.corcircle(pizzas.pca2$co)
# con factoextra
fviz_pca_var(pizzas.pca2)
# Con colores personalizados
fviz_pca_var(pizzas.pca2, col.var="steelblue")+
  theme_minimal()

#------------------------------------------------------------------------
# La calidad de la representacion y las contribuciones de las variables (columnas)
# / individuos (filas) se calculan usando la función inertia.dudi() como sigue
inertia.pizzas <- inertia.dudi(pizzas.pca2, row.inertia = TRUE,
                               col.inertia = TRUE)
# Contributiones relativas de las columnas
var.con <- abs(inertia.pizzas$col.rel/100)
head(var.con)
# Tambien las podemos calcular como las coordenadas al cuadrado
head(pizzas.pca2$co^2)

#------------------------------------------------------------------------
# Mapa factorial
fviz_pca_var(pizzas.pca2, col.var="contrib")+
  scale_color_gradient2(low="blue", mid="yellow",high="red", midpoint=13)+
  theme_minimal()

#------------------------------------------------------------------------
# Contribuciones de las variables a los componentes principales
var.contrib <- inertia.pizzas$col.abs/100
head(var.contrib)

#------------------------------------------------------------------------
# Contribuciones de las variables a las Componentes Principales
# Primera componente
fviz_contrib(pizzas.pca2, choice = "var", axes = 1)
# Segunda componente
fviz_contrib(pizzas.pca2, choice = "var", axes = 2)


#------------------------------------------------------------------------
# Coordenadas de los individuos a los componentes principales
head(pizzas.pca2$li)

#------------------------------------------------------------------------
# Calidad de la representación de los individuos en los componentes principales
ind.cal <- abs(inertia.pizzas$row.rel)/100
head(ind.cal)

#------------------------------------------------------------------------
# Contribución de los individuos a los componentes principales
ind.contrib <- inertia.pizzas$row.abs
head(ind.contrib)

#------------------------------------------------------------------------
# Contribución de los individuos a los componentes principales
# Primera componente
fviz_contrib(pizzas.pca2, choice = "ind", axes = 1)
# Segunda componente
fviz_contrib(pizzas.pca2, choice = "ind", axes = 2)


#------------------------------------------------------------------------
# Biplot de individuos y variables utilizando ade4
# Plot of individuals
s.label(pizzas.pca2$li, xax = 1, yax = 2)
# Add variables
s.arrow(7*pizzas.pca2$c1, add.plot = TRUE)
# Usando 'scatter'
scatter(pizzas.pca2, posieig = "none", clab.row = 0)

#------------------------------------------------------------------------
# Calidad de los individuos en el mapa factorial
fviz_pca_ind(pizzas.pca2, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.60)

#------------------------------------------------------------------------
# Biplot de individuos y variables
fviz_pca_biplot(pizzas.pca2, geom = "text") +
  theme_minimal()

#------------------------------------------------------------------------
# Conclusiones

# La dimension de los datos se redujo de 7 variables a tres factores aplicando 
# analisis factorial con el metodo de estimacion de MV y ACP. Aunque la interpretacion
# de los factores depende de las relaciones definidas por la matriz de cargas 
# rotadas, los beneficios de reducir la dimension de los datos y el uso de predictores
# no correlacionados son mayores y son muy utiles para analisis posteriores.
#------------------------------------------------------------------------