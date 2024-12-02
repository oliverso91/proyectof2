install.packages("haven")

install.packages("rpart")
install.packages("rpart.plot")

library(haven)
library(rpart)
library(rpart.plot)

# Cambia "ruta/del/archivo.sav" por la ruta a tu archivo SPSS
data <- read_sav("/Users/oliverrodas/Documents/tar/mdd/educacion_formal.sav")

# Verifica los datos cargados
print(head(data))


#pprueba arbol1
arbol <- rpart(Grado ~
                 Jornada_Est+
                 Graduando+
                 Plan_Est+
                 Sector,
               data = data, method = "class")


rpart.plot(arbol, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de area", cex = 1)

#pprueba arbol2

arbol <- rpart(Sector ~
                 Sexo+
                 Plan_Est+
                 Jornada_Est+
                 Resultado_F,
               data = data, method = "class")

rpart.plot(arbol, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de area", cex = 1)


#pprueba arbol3
#Alta verapaz
dataFP2D <- subset(data, Departamento_F == 16)

dataFP2D <- na.omit(dataFP2D)


arbol <- rpart(Departamento_F ~
                 Sector+
                 Grado+
                 Pueblo_Per+
                 Graduando,
               data = data, method = "class")

rpart.plot(arbol, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de area", cex = 1)


#pprueba arbol4

dataFP2D <- subset(data, Departamento_F == 16)

dataFP2D <- na.omit(dataFP2D)


arbol <- rpart(Departamento_F ~
                 Área,
                 Sexo+
                 Plan_Est+
                 Repitente+
                 Jornada_Est,
               data = data, method = "class")

rpart.plot(arbol, type=2, extra=0, under = TRUE, fallen.leaves = TRUE, box.palette = "BuGn", 
           main ="Predicción de area", cex = 1)

###############
install.packages("randomForest")
library(randomForest)

bosque_data <- small_data[, c("Departamento_F", "Área", "Sexo", "Plan_Est", "Repitente", "Jornada_Est")]

bosque_data$Departamento_F <- as.factor(bosque_data$Departamento_F)

set.seed(100)

bosque_data <- bosque_data[sample(1:nrow(bosque_data)),]

small_data <- data[sample(nrow(data), size = 100000), ]  # Toma una muestra aleatoria


index <-sample(1:nrow(bosque_data), 0.8*nrow(bosque_data))

train <- bosque_data[index,]
test <- bosque_data[-index,]



bosque <- randomForest(Departamento_F ~ Sexo + Plan_Est + Repitente, ,
                       data = train,
                       ntree = 1000,
                       mtry = 10
)

entreno <- predict(bosque, test)
getOption("max.print")

dato_nuevo <- data.frame(
  Área=2,
  Sexo=2,
  Plan_Est=5,
  Repitente=2,
  Jornada_Est=4
)

dato_nuevo <- data.frame(
  Área=2,
  Sexo=1,
  Plan_Est=3,
  Repitente=1,
  Jornada_Est=9
)


prediccion <- predict(bosque, dato_nuevo)
prediccion


# Importancia de variables
varImpPlot(bosque, main = "Nivel de Escolaridad en entreno")
