#         
#                                       Ayudant�a N�4 
#                       Exploraci�n de datos & Construcci�n de Modelos
#                                   diego.bernals@usm.cl
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
#                 Fuente datos: AnalizaR datos politicos UC &&
# Huber, E., Nielsen, F., Pribble, J., & Stephens, J. D. (2006). Politics and Inequality in Latin America and
# the Caribbean. American Sociological Review, 71(6), 943-963. http://www.jstor.org/stable/25472438


# Si no los tiene recuerde descargarlos con install.packages("")
#Paquetes
library(stargazer) #Tablas
library(paqueteadp)
library(remotes)
library(ggcorrplot) #Correlaci�n
library(tidyverse)#Graficos ,  Manejo de datos
library(cowplot) # dos o m�s gr�ficos uno al lado del otro
library(caret) # data partition
library(glmnet) # Ridge
library(car)
library(lmtest)


#Plantear MCO




#Llamamos al DF, a trav�s de una promesa
data("bienestar")
bienestar



# Siempre debemos iniciar con un reconocimiento de las variables con las cuales estamos trabajando o atributos
# Podemos buscar valores faltantes, como encontrar valores atipicos.
head(bienestar)
tail(bienestar)
str(bienestar)
glimpse(bienestar)

#Generar estadistica descriptiva de nuestras variables
summary(bienestar)
#Para observar posibles outliers o valores faltantes.


#Limpiezza o selecci�n de nuestros datos

bienestar_1 <- bienestar %>% select(gini, gasto_educ, dualismo_sectorial, inversion_extranjera, pib, 
                                    diversidad_etnica, tipo_regimen, gasto_salud, gasto_segsocial,
                                    bal_legislativo, poblacion,represion,anio)


summary(bienestar_1)


#Identifiquemos nuestras variables y tratemos de comprender cuales son de nuestro interes para nuestro modelo

# Grafico de correlaci�n

M <- cor(bienestar_1, use = "pairwise") %>% round(1)
plot1 <- ggcorrplot(M, 
                    hc.order = TRUE,
                    type = "lower",
                    outline.color = "white",
                    colors = c("#6D9EC1", "white", "#E46726"),lab = TRUE)
plot1

#Grafico variable dependiente, histograma y grafico de densidad.

#H_0 -> Shapiro.test




plot2 <- ggplot(bienestar_1, aes(x = gini)) + 
  geom_histogram(aes(y = ..density..), colour = "orange", fill = "orange", alpha = 0.1, binwidth = 2)+
  geom_density(alpha = .2, fill = "#FF6666", colour = 'red') +
  theme_minimal()
plot2



plot2

summary(bienestar_1)


#Prueba de normalidad, para ver si debemos realizar transformaciones funcionales.
shapiro.test(bienestar_1$gini)


#Graficos con las variables que tienen mayor correlaci�n con nuestra variable dependiente.

pl3 <- ggplot(data = bienestar_1, aes(x = diversidad_etnica, y = gini)) +
  geom_point(colour = '#B713A1', alpha = 0.5) + 
  theme_minimal()
pl4 <- ggplot(data = bienestar_1, aes(x = pib, y = gini)) +
  geom_point(colour = '#1DA42A', alpha = 0.5) +
  theme_minimal()
pl5<- ggplot(data = bienestar_1, aes(x = bal_legislativo, y = gini)) +
  geom_point(colour = '#1DA42A', alpha = 0.5) +
  theme_minimal()

pl3
pl4
pl5


#Que puede decir de las relaci�nes, comente ...


#Nuestra variable de interes no es normal..

#Limpiar los valores faltantes
bienestar_no_na <- bienestar_1 %>% drop_na(gini, gasto_educ , inversion_extranjera , gasto_salud , gasto_segsocial , poblacion, 
                                           dualismo_sectorial, diversidad_etnica, pib, tipo_regimen, bal_legislativo,represion,anio)

#Utilizando dyplr
bienestar_no_na %>% 
  group_by(diversidad_etnica) %>%
  summarise(mean(gini))

#

glimpse(bienestar_no_na)

bienestar_no_na$diversidad_etnica = as.factor(bienestar_no_na$diversidad_etnica)
unique(bienestar_no_na$tipo_regimen)
bienestar_no_na$tipo_regimen = as.factor(bienestar_no_na$tipo_regimen)


count(bienestar_no_na$represion)

# pob
library(viridis)
library(hrbrthemes)
install.packages("viridis")

ggplot(bienestar_no_na,aes(x=gasto_salud, y=gini, size=poblacion, color=tipo_regimen)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 12), name="Population (M)") + labs(title = "Gasto en salud y GIni") + theme_bw()
  
ggplot(bienestar_no_na,aes(x=gasto_salud, y=pib, size=poblacion, color=diversidad_etnica)) +
  geom_point(alpha=0.5) +
  scale_size(range = c(.1, 12), name="Population (M)") + labs(title = "Gasto en salud y PIB") + theme_bw()




#Diversidad etnica

ggplot(bienestar_no_na,aes(x = pib, y = gini, color = diversidad_etnica)) +
  geom_jitter(alpha = 0.7) +
  labs(
    title = "Bienestar",
    x = "PIB",
    y = "gini",
    color = "Diversidad_etnica"
  ) +
  theme(legend.position = c(0.95, 0.15)) +
  scale_color_viridis_d()





ggplot(bienestar_no_na,aes(x=diversidad_etnica,y=pib)) + geom_boxplot()


ggplot(data = bienestar_no_na, aes(x=tipo_regimen, y = pib)) + geom_boxplot()

ggplot(data = bienestar_no_na, aes(x=tipo_regimen,y = gini,color = diversidad_etnica)) + geom_boxplot()


#Cuando hay mayor diversidad el coeficiente gini es mayor.

#Near zero variance criterio para seleccionar variables en un modelo.

#Fuente: https://tgmstat.wordpress.com/2014/03/06/near-zero-variance-predictors/

glimpse(bienestar_no_na)

unique(bienestar_no_na$tipo_regimen)
sum(bienestar_no_na$tipo_regimen)

nzv(mode_1)
nearZeroVar(
  bienestar_no_na,
  freqCut = 95/5,
  uniqueCut = 10,
  saveMetrics = TRUE,
  names = TRUE,
  foreach = TRUE,
  allowParallel = TRUE
)

#En este caso no debemos remover ninguna variable para nuestro objeto de estudio.

#Planteamiento de Modelos

summary(bienestar_no_na$represion)


unique(bienestar_no_na$represion)
#Modelo 1
mode_1 <- lm(gini~.,data = bienestar_no_na[,-12:-13])
#, -12:-13
summary(mode_1)

mode_2<- lm(gini~.,data=bienestar_no_na[,-12:-13])
summary(mode_2)


stargazer(mode_1,mode_2,out = 'latex')


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Diagnostico modelos


#Analisis de residuos 

#Normalidad en residuos
#Media 0
#Varianza constante(Homocedasticidad)?




par(mfrow=c(2, 2))
plot(mode_1, las=1, col='deepskyblue4', which=1:3)
plot(mode_2, las= 1, col='chartreuse', which=1:3)

bienestar_no_na$predichos <- mode_1$fitted.values
bienestar_no_na$rstandard = rstandard(model = mode_1)


#ggplot con algunas variables


#Bonferroni(outliers)

library(car)#Pruebas de Bonferroni y distancias


outlierTest(mode_1,cutoff=Inf, n.max=15)

influenceIndexPlot(mode_1, vars="Bonf", las=1)
#Cook distance(Puntos influyentes)

cooks.distance(mode_1)

cutoff <- 4 / (167-2-2)  # Cota, n cantidad de datos
plot(mode_1, which=4, cook.levels=cutoff, las=1)
abline(h=cutoff, lty="dashed", col="dodgerblue2")





# Multicolinealidad ~VIF

#Dos formas de detectarla mediante matriz de correlaci�n y con el estadistico VIF

library(lmtest)

vif(mode_2)
vif(mode_1)

sqrt(vif(mode_2)) > 2
sqrt(vif(mode_1)) >2

#AnalzaR datos politicos UC
#True
#False


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
