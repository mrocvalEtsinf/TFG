X <- data[, setdiff(names(data), c("X1ª_eval", "Mejor_resp"))]
X <- X[, sapply(X, is.numeric)]
Y <- as.factor(data$X1ª_eval)

# Creamos un vector vacío para almacenar las predicciones
mypred <- character(nrow(X))  # o factor() si prefieres
names(mypred) <- rownames(X)

# LOO
for (i in 1:nrow(X)) {
  # Creamos datos de entrenamiento sin la i-ésima fila
  X_train <- X[-i, ]
  Y_train <- Y[-i]
  
  # Modelo PLS-DA con entrenamiento
  pls_model <- opls(X_train, Y_train, predI = 2, orthoI = 0)
  
  # Predicción de la muestra dejada fuera
  X_test <- X[i, , drop = FALSE]
  pred_i <- predict(pls_model, newdata = X_test)
  
  # Guardamos la predicción
  mypred[i] <- as.character(pred_i)
}

# Convertimos a factor con los mismos niveles que Y
mypred <- factor(mypred, levels = levels(Y))

# Confusion matrix
confusionMatrix(mypred, Y)