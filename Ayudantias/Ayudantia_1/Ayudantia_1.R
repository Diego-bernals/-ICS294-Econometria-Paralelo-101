# Econometric 01-2025: Intro to statistics
# TA: Diego Bernal Soto


#Matrices and vectors

#Let's start with the download of the packages 

install.packages("tidyverse") #Meta package that brings a lot of functions jejeje
install.packages("sswr")

install.packages("remotes")  # If not already installed
remotes::install_github("DickBrus/sswr")  # Replace 'username' with the actual repository owner

library(remotes)
install_github("DickBrus/sswr")
library(tidyverse) #We have to activate the libraries
library(sswr)


#Variables


#Some text...

#Vectors

#Conditionals

#My first function

#loops



#Matrices and vector operations 

vector <- c(1,1)
print(vector)

x <- matrix(c(1,1,3,2,1,0),ncol=3)
print(x)

A <- c(4, 5, 6)
B <- matrix(2:17, nrow = 4, byrow = TRUE)
C <- matrix(2, nrow = 2, ncol = 3)
D <- diag(4)

holi <- list(A,B,C,D)
for (i in holi){
  print(i)  
}


#Solve the nexts equations for matrices manipulation 

res_1 <- 3*A +1
res_2 <- rowSums(B)
res_3 <- colSums(B)
res_4 <- sum(B>6)
res_5 <- C + C
res_6 <- D %*% B
res_7 <- (B * B) - D

#Rainfall Coquimbo

library(sp)
library(sf)
library(raster)
library(tmap)
library(ggplot2)
library(cowplot)
library(viridis)
library(terra)
library(cowplot)
library(png)
library(gridExtra)

# Read raster images

# Relative paths
# A raster image consists of a matrix of cells or pixels, where each cell contains a value
# representing information, such as rainfall.

img_2007 <- raster("./Ayudantia_1/imgs/07/lluvia_07.tif")
img_2012 <- raster("./Ayudantia_1/imgs/12/lluvia_12.tif")
img_2017 <- raster("./Ayudantia_1/imgs/17/lluvia_17.tif")
img_2022 <- raster("./Ayudantia_1/imgs/22/lluvia_22.tif")

# Create a geometry for each cell within the total extent using the coordinates below.

ext <- as(extent(-71.717346,-69.809082,-32.282465,-29.037289), 'SpatialPolygons')
crs(ext) <- crs(img_2007) #Coordinate reference system

#Clip of the image
img_2007 <- crop(img_2007,ext)
img_2012 = crop(img_2012,ext)
img_2017 = crop(img_2017,ext)
img_2022 = crop(img_2022,ext)
plot(img_2007)

#Bring the ROI

ROI <- st_read("./Ayudantia_1/Data/COMUNA_C17.shp")

plot(ROI)


# Let's create a map of our information.
map1 <- tm_style("natural")+tm_shape(img_2007) +
  tm_raster(palette = rev(mako(n=7)),style = "cont",legend.show = FALSE,n = 7,legend.is.portrait = TRUE, title = "Median Precipitation \n (mm)") +
  tm_shape(ROI) + tm_borders(col = "grey",alpha = 0.9) + tm_layout(scale =0.8 ,title.size = 0.6, legend.outside = TRUE,
                                                                        legend.outside.position = c("right","top"),
                                                                        panel.labels = c("Median Rainfall of Coquimbo July 2017. Mapping: R"),
                                                                        panel.label.color = "black",
                                                                        panel.label.size = 0.5,legend.outside.size = 0.15, legend.text.size = 0.6,legend.format = "fun",
                                                                        inner.margins = 0) +
  tm_grid(alpha = 0.4,col = "grey",n.x = 5,n.y = 5,labels.size = 0.6) +tm_compass(type = "8star",
                                                                                  cardinal.directions = c("N", "E", "S", "W"),
                                                                                  size = 1.2,
                                                                                  position = c(0.7, 0.09),show.labels = 1) +tm_scale_bar(
                                                                                    width = 0.2,
                                                                                    text.size = 1.3,
                                                                                    text.color = "black",
                                                                                    color.dark = "lightsteelblue4",
                                                                                    color.light = "white",
                                                                                    position = c(0.68, 0),  
                                                                                    lwd = 1) 

map1

# Sampling in R
# Simple random sampling is the most basic form of probability sampling. There are two types:  
# with replacement and without replacement (in the former, a unit may be selected more than once).

# We can use the sample() function.


sample(c(1:10),size = 5,replace=FALSE)

# When we have a continuous and infinite spatial population, such as sampling points from an area,  
# we need to discretize it using a grid. Alternatively, we can select points directly from the cells.

set.seed(707)

sample_1 <- as(img_2007,"SpatialPointsDataFrame")
sample_1 <- as.data.frame(sample_1)
n <- 200

units <- sample(nrow(sample_1), size = n, replace = FALSE)#Retry the nrow number to sample

mysample_SS <- sample_1[units, ]

point_2007 <- st_as_sf(mysample_SS, coords = c("x", "y"), crs = 4326)

map1 <- tm_style("natural")+tm_shape(img_2007) +
  tm_raster(palette = rev(mako(n=7)),style = "cont",legend.show = TRUE,n = 7,legend.is.portrait = TRUE, title = "Median Precipitation \n (mm)") +
  tm_shape(ROI) + tm_borders(col = "grey",alpha = 0.9)+tm_shape(point_2007)+tm_dots(size = .1,col = "black") + tm_layout(scale =0.8 ,title.size = 0.6, legend.outside = TRUE,
                                                                                                                              legend.outside.position = c("right","top"),
                                                                                                                              panel.labels = c("Median Rainfall of Coquimbo July 2017. Mapping: R"),
                                                                                                                              panel.label.color = "black",
                                                                                                                              panel.label.size = 0.5,legend.outside.size = 0.15, legend.text.size = 0.6,legend.format = "fun",
                                                                                                                              inner.margins = 0) +
  tm_grid(alpha = 0.4,col = "grey",n.x = 5,n.y = 5,labels.size = 0.6) +tm_compass(type = "8star",
                                                                                  cardinal.directions = c("N", "E", "S", "W"),
                                                                                  size = 1.2,
                                                                                  position = c(0.7, 0.09),show.labels = 1) +tm_scale_bar(
                                                                                    width = 0.2,
                                                                                    text.size = 1.8,
                                                                                    text.color = "black",
                                                                                    color.dark = "lightsteelblue4",
                                                                                    color.light = "white",
                                                                                    position = c(0.68, 0),  # Colocar la barra de escala a la izquierda
                                                                                    lwd = 1) + tm_credits("Author: Diego Bernal, 2023", size = 2, position = c(0, 0), alpha = 1, col = "black")

map1

# Create a function that takes n_t_samples and the n value you calculated  
# for your estimator as arguments.


mean_popu <- mean(sample_1$lluvia_07)

n_samples <- function(data,n_samples,n){
  set.seed(777)
  N <- nrow(data)
  mean_rain = c()
  var_rain = c()
  for (i in 1:n_samples){
    units <- sample(N,size = n,replace = FALSE)
    mean_rain[i] <- mean(data[units,1])
    var_rain[i] <- var(data[units,1])/n
  }
  mean_rainfall_si <- data.frame(mean_rain = mean_rain,var_rain = var_rain)
  return(mean_rainfall_si)
}

df_rain <- n_samples(sample_1,10000,200) 





# Create a plot to visualize the density of the phi estimator of the mean for  
# 1,000, 5,000, and 10,000 samples.  
# Then, concatenate the results using cowplot.





ggplot(df_rain) + 
  geom_histogram(aes(x = mean_rain, y = ..density..), bindwidth = 2, fill = "orange",alpha = 0.5, colour = "black") +
  geom_density(aes(x = mean_rain, y = ..density..), lwd = 1) +
  geom_vline(aes(xintercept=mean(df_rain$mean_rain)),color = "red",linetype = "dashed",size = 0.5)+
  geom_vline(aes(xintercept=mean_popu),color = "blue",size=0.5)+
  ggtitle("Approximated sampling distribution of the \n Φ estimator of the mean rain for 10.000 samples")+
  scale_x_continuous(name = "Estimated mean Rain") +
  scale_y_continuous(name ="Density") + 
  theme_minimal()


plot_density <- function(df){
  return(ggplot(df) + 
           geom_histogram(aes(x = mean_rain, y = ..density..), bindwidth = 2, fill = "orange",alpha = 0.5, colour = "black") +
           geom_density(aes(x = mean_rain, y = ..density..), lwd = 1) +
           geom_vline(aes(xintercept=mean(df$mean_rain)),color = "red",linetype = "dashed",size = 0.5)+
           geom_vline(aes(xintercept=mean_popu),color = "blue",size=0.5)+
           ggtitle(paste("Approximated Sampling Distribution of the\nΦ Estimator of the Mean Rain for", nrow(df), "Samples"))+
           scale_x_continuous(name = "Estimated mean Rain") +
           scale_y_continuous(name ="Density") + 
           theme_minimal())
}

values <- c(100,1000,5000,10000)

graf <- list()

for (value in values){
  df <- n_samples(sample_1,value,200)
  graf[[as.character(value)]] <- plot_density(df)
}

plot_grid(graf[[1]], graf[[2]], graf[[3]], graf[[4]],nrow = 2)


# Now, let's calculate the confidence intervals for this estimator.  
# For this example, consider a threshold of 21 mm to separate the population.


for (i in 1:nrow(sample_1)){
  if (round(sample_1[i,1]) >22){
    sample_1$prop_rain[i] <- 1
  }else{
    sample_1$prop_rain[i] <- 0
  }
  
}

#Mutate option


sample_1 <- sample_1 %>% 
  mutate(prop_rain_mut = as.integer(round(.[[1]]) > 22))


#CF

ggplot(mysample_SS, mapping = aes(lluvia_07)) +
  stat_ecdf(geom = "step") +
  scale_x_continuous(name = "Rainfall") +
  scale_y_continuous(name = "F")




# The second way to express our uncertainty about the estimate is not merely a number,  
# but an interval. Assuming a normal distribution...




df_rain$upper<- df_rain$mean_rain + qt(0.05, n - 1, lower.tail = FALSE) * sqrt(df_rain$var_rain)
df_rain$lower <- df_rain$mean_rain - qt(0.05, n - 1, lower.tail = FALSE) * sqrt(df_rain$var_rain)
df_rain$ind <- (mean_popu > df_rain$lower & mean_popu < df_rain$upper)

coverage <- mean(df_rain$ind)




df_plot <- data.frame(
  id = rep(seq_along(df_rain$lower), times = 2),
  x = c(df_rain$lower, df_rain$upper),
  y = rep(seq_along(df_rain$lower), times = 2),
  ind = df_rain$ind
)


#Create a plot to see 
ggplot(df_plot) +
  geom_path(aes(x = x, y = y, group = id,color = ind)) +
  scale_x_continuous(name = "90% Interval Estimate of Mean") +
  scale_y_continuous(name = "Samples", limits = c(0, 100)) +
  scale_color_brewer(name = "Coverage", palette = "Set1") +
  geom_vline(xintercept = mean_popu, colour = "red", size = 1) +
  ggtitle("Estimated confidence intervals of the mean rainfall \n from 100 samples")+
  theme_minimal()


s#How many means are outside the scope?



#Hypothesis Testing

#Normality test
set.seed(7)
tarifas_tripulantes <- runif(40, min=150-22, max=150+22)
tarifas_pilotos <- runif(36, min=160-18, max=160+18)

shapiro.test(tarifas_tripulantes)
shapiro.test(tarifas_pilotos)

#means

t.test(tarifas_pilotos, tarifas_tripulantes, alternative = "greater")

#NEED

z_value <- qnorm(0.975)
sigma <- 22  
error <- 40  

# Tamaño de muestra necesario
n_needed <- (z_value * sigma / error)^2
ceiling(n_needed)  


#Hora de llegada v/s tarifas

early_checkin <- runif(50, min=6, max=14)  # (entre 6 AM y 2 PM)
tarifas_pilotos <- runif(50, min=140, max=180)  

cor_test <- cor.test(early_checkin, tarifas_pilotos, method = "pearson")
cat("Valor p:", cor_test$p.value, "\n")
