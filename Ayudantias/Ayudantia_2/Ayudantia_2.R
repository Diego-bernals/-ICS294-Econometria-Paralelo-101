# Econometric 01-2025: Linear regression
# TA: Diego Bernal Soto


install.packages("tidyverse")

library(tidyverse)

#Matrices

#Create a lineal function of the form m(x) = 2x +1 and return a result


f <- function(x){
  return (2*x + 1)
}

h <- function(x){
  return (x**2 +1)
}

#Create a function that generates a 1 dimensional data to estimate the function f(x)**2

create_1_dm <- function(q_numbers,w,b,random_scale = 1){
  #Simulate data with different distributions to Make the matrices to start the calculation
  X <- 0: (q_numbers -1)
  set.seed(707)
  deltas <- runif(length(X),min = -random_scale,max= random_scale)
  Y <- b + deltas + w*X
  
  return(list(X = X, Y = Y))
}

data <- create_1_dm(20,2,1,1)

data$X
data$Y
df <- data.frame(X = data$X, Y = data$Y)

ggplot(df, aes(x = X, y = Y)) +
  geom_point() +
  labs(title = "Simulated values",
       x = "X",
       y = "Y") +
  theme_minimal()



#Vectorization of the problem of linear regression in functions 
#Let's add the vector of 1s 

add_ones <- function(X){
  cbind(X, rep(1,length(X)))
}

#Create some parameters w,b to produce vectors of predictions

predict_params <- function(X, params) {
  as.vector(X %*% params)
}


X_augmented <- add_ones(data$X)

M1 <- c(2,1)
M2 <- c(4,3)


preds_M1 <- predict_params(X_augmented, M1)
preds_M2 <- predict_params(X_augmented, M2)


#Print the dimensions 
cat("Dim of Y: ", length(data$Y), "\n")
cat("Dim of M1 pred", length(preds_M1),"\n")
cat("Dim of M2 pred", length(preds_M2))

##Solving the minimization problem


#Algebraic using orthogonal projection


solve_orthogonal <- function(X_augmented,Y){
  XtX_inv <- solve(t(X_augmented) %*% X_augmented)
  XtY <- t(X_augmented) %*% Y
  w_opt <- XtX_inv %*% XtY
  return(w_opt)
}


#Create a function to calculate the MSE

compute_mse <- function(Y,preds){
  mean((Y - preds)^2)
}

w_star <- solve_orthogonal(X_augmented,data$Y)
print(w_star)

predictions <- as.vector(X_augmented %*% w_star)
df$predictions <- as.vector(X_augmented %*% w_star)


ggplot(df, aes(x = X)) +
  geom_point(aes(y = Y), color = "blue", size = 2) +
  geom_line(aes(y = predictions), color = "red", linewidth = 1) +
  labs(title = "Simulated values with a Orthogonal Proyection",
       x = "X",
       y = "Y / Prediction") +
  theme_minimal()


#Create a func of n dimensions


create_1_dm_ <- function(q_numbers,w,b,random_scale = 1){
  #Simulate data with different distributions to Make the matrices to start the calculation
  X <- runif(q_numbers, min = 0, max = 200)
  set.seed(707)
  deltas <- runif(length(X),min = -random_scale,max= random_scale)
  Y <- b + w*X
  
  return(list(X = X, Y = Y))
}


true_func <- function(X){
  X^2
}

generate_true_values <- function(size) {
  set.seed(707)
  X <- runif(size, min = 0, max = 200)
  Y <- true_func(X)
  return(data.frame(X = X, Y = Y))
}

true_data <- generate_true_values(200)
X_true <- true_data$X
Y_true <- true_data$Y

mse_list <- list()
pred_list <- list()



plot <- ggplot() + 
  geom_point(data = true_data,aes(x = X, y = Y), alpha = 0.3,color = "blue") +
  geom_line(data = true_data,aes(x=X,y=Y),color = "grey",linewidth = 1.5)


for (i in 1: 100){
  set.seed(i)
  
  data <- create_1_dm_(200,w = 1, b = mean(Y_true), random_scale = 10)
  X_augmented <- add_ones(data$X)
  
  w_star <- solve_orthogonal(X_augmented,Y_true)
  
  Y_pred <- predict_params(X_augmented,w_star)

  
  mse_list[[i]] <- compute_mse(Y_true, Y_pred)
  pred_list[[i]] <- Y_pred
  
  plot <- plot + geom_line(aes(x = data$X, y = Y_pred), color = "red", alpha = 0.5) 
}

plot + ggtitle("Bias vs Variance")


#Ejercicio 1

#Cambiar la ruta a relativa

library(lubridate)

df_elec <- read.csv("C:\\Users\\Diego\\OneDrive\\Escritorio\\Ayudantias\\Ayudantia_2\\Book1.csv",sep = ";")


glimpse(df_elec)


ggplot(df_elec, aes(x=IMACEC,y=Con_gwh)) + geom_point(colour = '#03254b', alpha = 0.5) +
  theme_minimal() + labs(title = 'IMACEC y Consumo energetico (2019-2020)')


var_ima <- var(df_elec$IMACEC)
var_con_gwh <- var(df_elec$Con_gwh)

cov <- cov(df_elec$IMACEC, df_elec$Con_gwh)

cor <- cor(df_elec$IMACEC, df_elec$Con_gwh)

cat("Varianza IMACEC:", var_ima, "\n")
cat("Varianza Consumo GWh:", var_con_gwh, "\n")
cat("Covarianza:", cov,"\n")
cat("Correlación:", cor)

#Calcule con prueba de hipotesis y un grado de significancia de 0.05 si la correlacion es significativa
