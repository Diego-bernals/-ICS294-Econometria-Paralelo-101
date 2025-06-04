## Ayudantía N°8

#Diego Bernal Soto

library(tidyverse)
library(wooldridge)
library(cowplot)
library(stargazer)
library(ggeffects)

#Pregunta N°1
data("mroz")

glimpse(mroz)

#The first model is with the variables 

model_1 <- lm(inlf~nwifeinc+educ+exper+expersq,data = mroz)
summary(model_1)

pred <- predict(model_1,newdata = data.frame(nwifeinc = 10,
                                     educ = 12,
                                     exper = 5,
                                     expersq = 5^2))

cat("La probabilidad de que una mujer participe en el mercado laboral es de: \n",round(pred,1))

#Pregunta N°2


data("loanapp")

model_2 <- lm(approve ~ white + hrat + obrat,data = loanapp)

summary(model_2)

#Pregunta N°3

set.seed(2231)

genero_dict <- c(
  "1" = "Balada romántica",
  "2" = "Pop latino",
  "3" = "Cumbia",
  "4" = "Ranchera",
  "5" = "Bolero",
  "6" = "Salsa",
  "7" = "Merengue",
  "8" = "Reggaetón",
  "9" = "Rock",
  "10" = "Música tropical de vallenar",
  "11" = "Música folklórica",
  "12" = "Corridos Tumbados",
  "13" = "Música infantil",
  "14" = "Pop internacional",
  "15" = "Mexicana"
)

profesion_dict <- c(
  "1" = "Ingeniero Comercial",
  "2" = "Ingeniero Civil Industrial",
  "3" = "Periodista",
  "4" = "Abogado",
  "5" = "Ingeniero Civil Eléctrico",
  "6" = "Ingeniero Civil Matemático"
)

genero_id <- sample(1:15, 1)
profesion_id <- sample(1:6, 1)


genero_dict[as.character(genero_id)]
profesion_dict[as.character(profesion_id)]

tiempo <- 1990:2015
df <- data.frame()

#Como supuesto asumimos que se transmitio esta parte del programa en la seccion de juegos
# apareciendo de forma aleatorio entre los episodios de un año, no en todos. Como supuesto
# ganan siempre entre 3 o 4 personajes. prop hombres de mujeres
#viendo el programa cual es la probabilidad de que el participante tenga conexion con el publico aunque no haya ganado
for (t in tiempo){
  edad_don <- t - 1940
  episodios_max <- sample(40:50,1)
  episodios <- 1:episodios_max
  for(i in episodios){
    participantes_max <- sample(5:10, 1)
    participantes <- 1:participantes_max
    n_ganan <- sample(3:4,1)
    ganadores <- sample(participantes, n_ganan)
    for (p in participantes){
      genero_id <- sample(1:15, 1)
      profesion_id <- sample(1:6, 1)
      gano = ifelse(p %in% ganadores, 1, 0)
      
      if(gano == 1){
        cone <- 1
      }else{
        cone <- sample(c(0,1),1,prob = c(0.6, 0.4))
      }
      
      fila <- data.frame(
        year = t,
        episodio = paste0(t,"_",i),
        Y = gano,
        genero = genero_dict[as.character(genero_id)],
        orden_part = p,
        gender = sample(c(0,1),1),
        profesion = profesion_dict[as.character(profesion_id)],
        est_civ = sample(c(0,1),1),
        edad_don = edad_don,
        cone_pub = cone,
        dur_seg = max(1, round(rnorm(1,mean=20,sd=8)))
      )
      df <- rbind(df, fila)
    }
  }
  
}



glimpse(df)

#Transformar a factor
df$gender <- factor(df$gender, levels = c(0, 1), labels = c("Hombre", "Mujer"))
df$gano <- factor(df$Y, levels = c(0, 1), labels = c("Perdio", "Gano"))
df$est_civ <- factor(df$est_civ, levels = c(0, 1), labels = c("Casado", "Soltero"))
df$cone_pub <- factor(df$cone_pub, levels = c(0, 1), labels = c("No conecta", "Conecta"))

#Las var que ya son chr

df$genero <- factor(df$genero)
df$profesion <- factor(df$profesion)

glimpse(df)

#Grafico

p1 <- ggplot(data = df, aes(x = dur_seg)) + 
  geom_histogram(binwidth = 5,alpha = 1,fill="#69b3a2", color="#e9ecef") +
  labs(title = "Distribución de los tiempos",
       x = 'Tiempo',
       y = 'Cantidad')
p1

#veamoslo por estado civil 

p2 <- ggplot(data = df, aes(x = dur_seg, fill = gender)) + 
  geom_histogram(binwidth = 5, alpha = 0.6, color = "#e9ecef", position = "identity") +
  labs(
    title = "Distribución de los tiempos por género",
    x = "Tiempo",
    y = "Cantidad",
    fill = "Género"
  ) +
  theme_minimal()
p2

#

p3 <- df %>% count(genero,sort = TRUE) %>%ggplot(aes(x=reorder(genero,-n),y = n,fill = genero)) + 
  geom_bar(stat = 'identity') + coord_flip()+
  labs(title = "Ranking de los generos mas utilizadas",
       x='genero',
       y='Cantidad') + scale_fill_viridis_d() + theme(axis.text.x=element_text(angle=90))
p3

p4 <- ggplot(data = df, aes(y = Y,fill=gano)) + 
  geom_bar() +
  labs(title = "Participantes que fueron eliminados :c",
       x = 'Cantidad') +  scale_fill_brewer(palette = "Set2")

p4


#mlp

p5<- ggplot(df,aes(x = year, y = dur_seg, color = genero)) +
  geom_jitter(alpha = 0.7) +
  labs(
    title = "Tiempos de los participantes",
    x = "Fecha",
    y = "Duración",
    color = "Pista"
  ) +
  scale_color_viridis_d()
p5
#modelo
model_3 <- lm(Y~genero+orden_part+gender+profesion+est_civ+edad_don+cone_pub+dur_seg,
              data = df) 

summary(model_3)

#La clave era ganarse al publico con el chiste de Don francisco jaja.

#agregue las interacciones entre variables.
#Interaccio nentre edad del don y la cone con el publico

model_4 <- lm(Y~genero+edad_don*cone_pub+orden_part+gender+profesion+est_civ+edad_don+cone_pub+dur_seg,
              data = df) 

summary(model_4)



df$pred_1 <- predict(model_3, data = df)
df$pred_2 <- predict(model_4, data = df)
p6 <- ggplot(df, aes(x = dur_seg, y = pred_1)) +
  geom_point(alpha = 0.2, color = "gray") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(
    title = "Duración y probabilidad predicha de pasar",
    x = "tiempo",
    y = "Probabilidad predicha"
  ) +
  theme_minimal()

p7 <-ggplot(df, aes(x = dur_seg, y = pred_2)) +
  geom_point(alpha = 0.2, color = "gray") +
  geom_smooth(method = "lm", se = FALSE, color = "pink") + 
  labs(
    title = "Duración y probabilidad predicha de pasar",
    x = "tiempo",
    y = "Probabilidad predicha"
  ) +
  theme_minimal()

plot_grid(p6, p7, labels = c("A", "B"), ncol = 2)


#Efectos cruzados

pred_df <- ggpredict(model_3, terms = c("genero", "gender"),
                     vcov.fun = "vcovHC", vcov.type = "HC0")

s