# Cargar las librerías
library(dplyr)  
library(ggplot2)  
library(readr)  
library(caret)
library(pROC) 
library(tidyr)
library(tidyverse)
library(ggthemes) #estilo de gráficos
library(corrplot) #correlaciones
library(GGally)  # matrices de dispersión

# Cargar datos 
anemia_data <- read_csv("C:/Users/niuyu/OneDrive/Documentos/UNAM/Clases/Estadística/BioestadisticaProyecto/anemia_data.csv")

# Inspeccionar las primeras filas del conjunto de datos
head(anemia_data)

# Resumen estadístico de las variables
summary(anemia_data)

# Verificar si hay valores perdidos
sum(is.na(anemia_data))

# Verificar los nombres de las columnas
colnames(anemia_data)

# Convertir 'Result' en factor para el análisis de clasificación
anemia_data$Result <- as.factor(anemia_data$Result)

# Histogramas de variables numéricas
ggplot(anemia_data, aes(x = Hemoglobin, fill = Result)) +
  geom_histogram(binwidth = 0.5, alpha = 0.6, position = "identity") +
  labs(title = "Distribución de Hemoglobina por Resultados de Anemia", x = "Hemoglobina", y = "Frecuencia") +
  theme_minimal()

ggplot(anemia_data, aes(x = MCH, fill = Result)) +
  geom_histogram(binwidth = 0.5, alpha = 0.6, position = "identity") +
  labs(title = "Distribución de MCH por Resultados de Anemia", x = "MCH", y = "Frecuencia") +
  theme_minimal()

ggplot(anemia_data, aes(x = MCHC, fill = Result)) +
  geom_histogram(binwidth = 0.5, alpha = 0.6, position = "identity") +
  labs(title = "Distribución de MCHC por Resultados de Anemia", x = "MCHC", y = "Frecuencia") +
  theme_minimal()

ggplot(anemia_data, aes(x = MCV, fill = Result)) +
  geom_histogram(binwidth = 0.5, alpha = 0.6, position = "identity") +
  labs(title = "Distribución de MCV por Resultados de Anemia", x = "MCV", y = "Frecuencia") +
  theme_minimal()

# Matriz de correlación para variables numéricas
numeric_data <- anemia_data > select(Hemoglobin, MCH, MCHC, MCV)
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)

# Matriz de dispersión (Pair plot)
ggpairs(anemia_data, aes(color = Result, alpha = 0.6))


# Modelo de regresión logística
log_model <- glm(Result ~ Gender + Hemoglobin + MCH + MCHC + MCV, 
                 data = train_data, family = binomial)

# Resumen del modelo de regresión logística
summary(log_model)

# Predicciones sobre el conjunto de prueba
log_pred <- predict(log_model, newdata = test_data, type = "response")

# Convertir las predicciones en 0 o 1 según el umbral de 0.5
log_pred_class <- ifelse(log_pred > 0.5, 1, 0)
log_pred_class <- as.factor(log_pred_class)

# Evaluar el desempeño del modelo
conf_matrix <- confusionMatrix(log_pred_class, test_data$Result)
print(conf_matrix)

# Curva ROC y AUC
plot(roc_curve, col = "blue", main = "Curva ROC para el Modelo de Regresión Logística")

# Visualización de las probabilidades de predicción
ggplot(data.frame(log_pred = log_pred, Result = test_data$Result), aes(x = log_pred, color = Result)) +
  geom_density() +
  labs(title = "Distribución de Probabilidades de Predicción", x = "Probabilidades de Anemia", y = "Densidad") +
  theme_minimal()

# Boxplot de resultados por grupos (si existiera alguna variable de grupo)
ggplot(anemia_data, aes(x = Result, y = Hemoglobin, fill = Result)) +
  geom_boxplot() +
  labs(title = "Distribución de Hemoglobina por Resultados de Anemia") +
  theme_minimal()

#conclusion

# Este análisis de datos permite dar una comparativa y además de dar una pequeña predicción sobre si se tiene anemia o no en los distintos casos muestreados.
