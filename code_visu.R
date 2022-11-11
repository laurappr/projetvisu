library(dplyr)
library(ggplot2)
library(stringr)
library(countrycode)

# --------------------------------------------------------------------------------
# importation des données 
data = read.csv("data/obesity-cleaned.csv",sep=",",header=T)
summary(data)

#Import des données countries of the world

data = data[,-1]
data$Country = as.factor(data$Country)
data$Year = as.factor(data$Year)
data$Sex = as.factor(data$Sex)

# on découpe la variable obésité en 2 : pourcentage et intervalle de confiance
data = data %>% rename(Obesity = Obesity....) %>% 
  mutate(Std_Dvt= sub(".* ", "", Obesity)) %>%
  mutate(ObesityPercentage=as.double(sub(" .*", "", Obesity))) %>%
  filter(!is.na(ObesityPercentage)) %>% # supp les na
  select(Country, Year, Sex,Obesity=ObesityPercentage, Std_Dvt)

head(data)

# --------------------------------------------------------------------------------
# Marion

# un graph moyenne mondiale de l'obésité selon les années (total, homme et femme)
# pondéré par le nombre d'habitants : weighted.mean()
# sans pondération



# calcul de l'obesité moyenne par an (pondéré par pays)

# --------------------------------------------------------------------------------
# courbe d'évolution de l'obésite par continent 

# Laura

# --------------------------------------------------------------------------------
# 2 graphs : carte avec l'obésite par pays en 1975 et en 2016

# Carte Laura : 1975
# Carte Marion : 2016





