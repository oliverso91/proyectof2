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



bosque <- randomForest(Departamento_F ~ Sexo + Plan_Est + Repitente,
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



#Redes neuronales 

#install.packages("rpart") 
install.packages("neuralnet") 
install.packages("caret")  

library(neuralnet)
library(caret)



red_neuronal <- createDataPartition(bosque_data$Departamento_F, p = 0.7, list = FALSE)
train <- bosque_data[red_neuronal,]
test <- bosque_data[-red_neuronal,]


modelo1 <- rpart(Departamento_F ~ Sexo + Plan_Est + Repitente, Jornada_Est,
                     data = train, 
                     method = "class")

train_data <- train
train_data <- cbind(train_data, model.matrix(~Departamento_F - 1, data = train))
train_data <- train_data[, -1]  # Eliminar la columna original 'Departamento'


response_var <- "Plan_Est"
predictors <- setdiff(colnames(train_data), response_var)

formula <- as.formula(paste(response_var, "~", paste(predictors, collapse = " + ")))

print(formula)

nn_model <- neuralnet(formula, data = train_data, hidden = c(5, 3), linear.output = FALSE)


# Visualizar el modelo
plot(nn_model, main = "Red Neuronal")
# Predicciones
predictions <- compute(nn_model, train_data[, predictors])

# Convertir resultados de predicción a la clase más probable
predicted_class <- apply(predictions$net.result, 1, which.max)

# Comparar con la variable real
table(Predicted = predicted_class, Actual = train_data[[response_var]])
