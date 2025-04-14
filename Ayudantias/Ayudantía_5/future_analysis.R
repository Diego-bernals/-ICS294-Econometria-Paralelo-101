
#Futurama

library(stargazer)
library(paqueteadp)
library(remotes)
library(ggcorrplot)
library(tidyverse)
library(cowplot) # dos o más gráficos uno al lado del otro
library(caret) # data partition
library(glmnet) # Ridge
library(car)
library(gghighlight)
library(rlang)
library(tidyverse)
library(tidymodels)
library(schrute)
library(lubridate)
library(knitr)
library("scales") #Paleta de colores de futurama
library(ggsci)

episode_list <- read.csv("C:/Users/Diego/Desktop/Futurama/episode_list.csv", sep=";")
text_futurama <- read.csv("C:/Users/Diego/Desktop/Futurama/only_spoken_text.csv", sep=",")

glimpse(episode_list)

#Cleaning
#We ve got ssome problems with the date in the episodes so we have to clean that part 

futurama <- episode_list %>%
  mutate(Air.Date = ifelse(str_detect(Air.Date, "^\\(.*\\)$"), gsub("\\(|\\)", "", Air.Date), Air.Date),  # Eliminar paréntesis si está presente
         Air.Date = parse_date_time(Air.Date, c("%Y-%m-%d", "%B %d, %Y"))) %>%  filter(IMDBrating != "na") %>%
  mutate_at(vars(IMDBrating, Votos), as.numeric) %>% mutate(Season = factor(Season,as.character(1:7)))

glimpse(futurama)
summary(futurama)

pl0 <- ggplot(futurama, aes(x = IMDBrating)) + 
  geom_histogram(aes(y = ..density..), colour = "orange", fill = "orange", alpha = 0.2, binwidth = 1)+
  geom_density(alpha = .2, fill = "#FF6666", colour = 'red') +
  theme_minimal() + labs(title = "Densidad del IMDB rating",
                         x="IMDB rating",
                         y="Densidad")

pl0

pl1 <- ggplot(futurama, aes(x = Votos, y = IMDBrating, color = Season)) +
  geom_jitter(alpha = 0.7) +
  labs(
    title = "Futurama ratings",
    x = "Total votes",
    y = "IMDB rating",
    color = "Season"
  ) +
  theme(legend.position = c(0.9, 0.5)) + scale_color_futurama()
pl1

#Boxplots
pl2 <- ggplot(futurama,aes(x=Season,y=IMDBrating,color = Season)) +
  geom_boxplot() + labs(title = "Futurama ratings",
                        x = "Season",
                        y = "IMDB rating",
                        color = "Season"
  ) + scale_color_futurama()
pl2

pl3 <- ggplot(futurama,aes(x=Season,y=Votos,color = Season)) +
  geom_boxplot() + labs(title = "Futurama ratings",
                        x = "Season",
                        y = "Votos",
                        color = "Season"
  ) + scale_color_futurama()
pl3


pl4 <- ggplot(futurama, aes(x = Votos, y = IMDBrating)) +
  geom_jitter(col = "darkred")  +
  gghighlight(Votos > 3000, label_key = title, unhighlighted_colour = alpha("darkgreen", 0.4)) +
  labs(
    title = "Futurama ratings",
    x = "Total votes",
    y = "IMDB rating"
  )

pl5 <- ggplot(futurama,aes(x = Air.Date, y = IMDBrating, 
                            color = Season, size = Votos)) +
  geom_point() +
  labs(x = "Air date", y = "IMDB rating",
       title = "Futurama Ratings")+ scale_color_futurama()
pl5

#Analisis de texto

futurama<- futurama %>%
  rename(Episode_name = Title)

text_futurama <- text_futurama %>%
  rename(Episode_name = Episode)

text_futurama<- merge(text_futurama, futurama, by = "Episode_name")


glimpse(text_futurama)

text_futurama %>%
  count(Character, sort = TRUE) %>% head(10)
#Kif da igual
summary(text_futurama)
#Como podemos ver no esta la temporara 7 por lo tanto nuestro modelo de regresión tendra que considerar este ajuste
#posteriormente



library(tidytext)
library(stm)
library(quanteda)
library(wordcloud)
library(reshape2)
library(geometry)
library(Rtsne)
library(rsvd)
library(syuzhet)
library(scales)
library(geniusr)
library(purrr)
library(jsonlite)
library(wordcloud2)
library(kableExtra)
library(textdata)

text_futurama$Season <- factor(text_futurama$Season, levels = as.character(1:7))

# Continuar con el código original

text_futurama %>%
  filter(!is.na(Line)) %>%
  mutate(
    character = fct_lump_n(Character, 10)
  ) %>%
  count(Season, character) %>%
  mutate(character = reorder_within(character, n, Season)) %>%
  ggplot(aes(n, character, fill = Season)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Season, scales = "free", labeller = labeller(Season = function(x) paste("Temporada", x))) +
  scale_y_reordered() +
  scale_fill_futurama() +
  labs(x = "Cantidad de diálogos", y = NULL) +
  ggtitle("Cantidad de diálogos por personaje en cada temporada")+
  theme(strip.background = element_blank())+
  theme(strip.text = element_text(size = 10, face = "bold")) +
  labs(x = "Cantidad de diálogos", y = NULL) +
  labs(title = "Cantidad de diálogos por personaje en cada temporada")

#Separando los dialogos en palabras por separado

text_futurama_1 <- text_futurama %>%
  mutate(Line = strsplit(Line, " ")) %>%
  unnest(cols = Line)


#Eliminando Na
text_futurama_1 <- text_futurama_1  %>%
  filter(Line != "" & Line != "...")


data(stop_words)

#Limpiando stop words

tidy_lines <- text_futurama_1 %>%
  unnest_tokens(word, Line) %>% 
  anti_join(stop_words, by=c("word"="word")) 

tidy_lines <- tidy_lines  %>% filter(word != "em", word != "y'all",word != "da",
                                     word != "ooh", word != "shit", word != "duh",
                                     word != "na", word != "woah", word != "300",
                                     word != "fi", word != "ba", word != "ugh",
                                     word != "mm", word != "wah", word != "phi", word != "yeah", word != "mmh", word != "la", word != "uh", word != "ya", word != "ooh", word != 
                                       "ayy", word != "i'ma", word != "mmm", word != "shit", word != "fuckin", word != "huh", word !="gon",
                                     word != "dem", word != "gwaan", word != "ey", word != "bam", word != "hey", word != "ah")

word_freq <- tidy_lines %>%
  count(word) %>%
  arrange(desc(n)) 

top_25_words <- head(word_freq, 25)

p1 <- ggplot(top_25_words, aes(x = reorder(word, n), y = n)) +  # Utilizar reorder() para ordenar las barras de mayor a menor
  geom_bar(stat = "identity", fill = "#8A4198FF", alpha = 0.6) +
  coord_flip() +
  ylab("Number of times used") +xlab("Word")+
  theme_bw()+ ggtitle("Most frequently used words in Futurama")
p1

#Topicos repetidos tenemos robots, tiempo, amor. time,robot, bender, zoidberg

futurama_words_bing <- tidy_lines %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  ungroup()


futurama_words_counted <- futurama_words_bing %>%
  count(word, sentiment, name = "count")


futurama_words_counted %>%
  filter(count > 3) %>%
  top_n(20, count) %>%
  ggplot(aes(x = reorder(word, count), y = count, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = count), colour = "black", hjust = 1, fontface = "bold") +
  coord_flip() +
  labs(x = "\n Word \n", y = "Word Count \n", title = "Sentiment Scores Of Words \n Under bing Lexicon") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face = "bold", colour = "black", size = 12),
        axis.title.y = element_text(face = "bold", colour = "black", size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("#C71000FF","#008EA0FF"),labels = c("negative", "positive")) +
  guides(fill = guide_legend(title = "Sentiment"))

negvandpos <- tidy_lines %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0)

comparison.cloud(negvandpos, colors = c("#C71000FF", "#008EA0FF"),
                 max.words = 100, scale = c(3.5, 0.50))


#Tenemos que usar tidy lines para crear estas nuevas variables palabras mas usadas y cantidad de lineas por 
#personajes palabras time,robot, bender, zoidberg y lineas por personajes 4 Farnsworth, Leela, Bender, Fry

futurama<- futurama %>%
  rename(Episode_name = Title)

text_futurama <- text_futurama %>%
  rename(Episode_name = Episode)

text_futurama <- merge(text_futurama, futurama, by = "Episode_name")
glimpse(text_futurama)

head(tidy_lines)
head(df_result)
head(text_futurama)

oli <- tidy_lines %>%
  group_by(Season, Episode) %>%
  summarise(total_palabras = n())


df_result <- tidy_lines %>%
  filter(word %in% palabras) %>%
  group_by(Season, Episode, word) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from = word, values_from = count, values_fill = 0)




#Contando la cantidad de lineas de cada personaje
text_futurama <- text_futurama %>% mutate(
  Line = str_to_lower(Line),
  bender = if_else(str_detect(Line, "bender"),1,0),
  time = if_else(str_detect(Line,"time"),1,0),
  robot = if_else(str_detect(Line,"robot"),1,0),
  zoidberg = if_else(str_detect(Line,"zoidberg"),1,0)
)

df_futurama <- text_futurama %>%
  group_by(Season.y,Episode,Episode_name,IMDBrating,Votos,Air.Date) %>%
  summarize(
    n_lines = n(),
    lines_fry = sum(Character == "Fry") / n_lines,
    lines_bender = sum(Character == "Bender") / n_lines,
    lines_leela = sum(Character == "Leela") / n_lines,
    lines_farnsworth = sum(Character == "Farnsworth") / n_lines,
    bender_mention = if_else(sum(bender==1)>=1,sum(bender)/n_lines,0),
    time_mention = if_else(sum(time==1)>=1,sum(time)/n_lines,0),
    robot_mention = if_else(sum(robot==1)>=1,sum(robot)/n_lines,0),
    zoidberg_mention = if_else(sum(zoidberg==1)>=1,sum(zoidberg)/n_lines,0),
    .groups = "drop"
  ) %>%
  select(-n_lines)


glimpse(df_futurama)

#spatial engeniering

#Matriz de correlacion primero

corr_selected <- subset(df_futurama, select = c(IMDBrating,Votos,lines_fry,lines_bender,lines_leela,lines_farnsworth,
                                                bender_mention,time_mention,robot_mention,zoidberg_mention))

corr_matrix <- cor(corr_selected) %>% 
  round(1)

pepe <- ggcorrplot(corr_matrix, 
                   hc.order = TRUE,
                   type = "lower",
                   outline.color = "white",
                   colors = c("#1A5354FF", "white", "#C71000ff"),lab = TRUE)

pepe


#NZV

nzv <- nearZeroVar(df_futurama, saveMetrics= TRUE)
nzv
nearZeroVar(df_futurama)

#Separacion de datos
set.seed(707) #Fijamos la semilla 

fry <- initial_split(df_futurama)
fry_train <- training(fry)
fry_test <- testing(fry)

glimpse(fry_train)


#Modelo 1 con todas las variables

mod_1 <- lm(IMDBrating~Season.y+Votos+lines_fry+lines_bender+lines_leela+lines_farnsworth+
              bender_mention+time_mention+robot_mention+zoidberg_mention,data=fry_train)

summary(mod_1)

futurama_coeficientes <- mod_1$coefficients %>%
  enframe(name = "predictor", value = "coeficiente")

futurama_coeficientes %>%
  filter(predictor != "(Intercept)") %>%
  ggplot(aes(x = predictor, y = coeficiente)) +
  geom_col() +
  labs(title = "Coeficientes del modelo OLS") +
  theme_bw() +
  theme(axis.text.x = element_text(size = 9, angle = 45))

#Multicolinealidad


vif(mod_1)
sqrt(vif(mod_1))>2



#Modelo Backward y Forward

library(MASS)

mod_back <- stepAIC(mod_1,trace = TRUE,direction = "backward")

summary(mod_back)


mod_forward <- stepAIC()


