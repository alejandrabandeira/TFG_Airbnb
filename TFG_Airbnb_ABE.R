###TFG Alejandra Bandeira
##Análisis Airbnb en Twitter

setwd("G:/Mi unidad/ICADE/5. Quinto/TFG ANALYTICS")

#### Librerias ####
install.packages("tidyverse");
install.packages("data.table");
install.packages("textclean");
install.packages("tm");
install.packages("stringr");
install.packages("dplyr");
install.packages("tidyr");
install.packages("ggplot2");
install.packages ("readxl");
install.packages("openxlsx")
library(tidyverse);
library(data.table);
library(textclean);
library(tm);
library(stringr);
library(dplyr);
library(tidyr);
library(ggplot2);
library(readxl);
library(openxlsx)
install.packages("janeaustenr")
library(janeaustenr)
install.packages("emojifont")
library(emojifont)
library(stringr)
library(dplyr)
library(tidyr)

###### Upload dataset #####
dataset <- read_excel("Airbnb_Processed_Dataset_(Without Full Name).xlsx")

######### SELECCION ######

#Columnas que nos interesan para el analisis
#Separamos la fecha y la hora
#Convertir fecha a tipo date
data<- select(dataset, "Date", "Tweet", "Location (Continent)" )
data <- separate(data, "Date", into = c("Date", "Hour"), sep = " ")
class(data$Date)
data$Date <- as.Date(dataset$Date, format = "formato_de_fecha")

#Creamos nueva columna con los hashtags
hashtags <- regmatches(data$Tweet, gregexpr("#\\w+", data$Tweet))
data$Hashtags <- sapply(hashtags, function(x) paste(x, collapse = ", "))



#### Frecuencia por fecha ####
number_tweets_bydate <- data %>%
  group_by(Date) %>%
  summarise(count = n())
write.xlsx(number_tweets_bydate, "number_tweets_bydate.xlsx")


##### Frecuencia de los hashtags ####
all_hashtags <- unlist( 
  regmatches(data$Hashtags,  gregexpr('\\w+', data$Hashtags)))
freq_count <- as.data.frame(table(all_hashtags))
freq_count<-freq_count[order(freq_count$Freq, decreasing = TRUE),]
total_different_hashtags <- nrow(freq_count)
write.xlsx(freq_count, "freq_hashtags.xlsx")


##### Frecuencia emojis #####
emojis <- data %>%
  mutate(emoji = str_extract_all(Tweet, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{2600}-\\x{26FF}]")) %>%
  unnest(cols = emoji) %>%
  count(emoji, sort = TRUE) %>%
  top_n(20)
write.xlsx(emojis, "emojis.xlsx")


#CSV para continuar en Python
write.csv2 (data,"data_airbnb.csv", row.names = FALSE)
