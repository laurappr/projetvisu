library(dplyr)
library(ggplot2)


# --------------------------------------------------------------------------------
# importation des données 
data = read.csv("data/obesity-cleaned.csv",sep=",",header=T)
summary(data)

data = data[,-1]
data$Country = as.factor(data$Country)
data$Year = as.factor(data$Year)
data$Sex = as.factor(data$Sex)

# on découpe la variable obésité en 2 : pourcentage et intervalle de confiance
data = data %>% rename(Obesity = Obesity....) %>% 
  mutate(Std_Dvt= sub(".* ", "", Obesity)) %>%
  mutate(ObesityPercentage=as.double(sub(" .*", "", Obesity))) %>%
  filter(!is.na(ObesityPercentage)) %>%
  select(Country, Year, Sex,Obesity=ObesityPercentage, Std_Dvt)

head(data)

# --------------------------------------------------------------------------------
# un graph moyenne mondiale de l'obésité selon les années (total, homme et femme)


# --------------------------------------------------------------------------------
# courbe d'évolution de l'obésite par continent 



# --------------------------------------------------------------------------------
# 2 graphs : carte avec l'obésite par pays en 1975 et en 2016







