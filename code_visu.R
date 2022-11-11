library(dplyr)
library(ggplot2)
library(stringr)
library(countrycode)

# --------------------------------------------------------------------------------
# importation des données 
data = read.csv("data/obesity-cleaned.csv",sep=",",header=T)
summary(data)

#Import des données countries of the world
countries = read.csv('data/countries of the world.csv', header = TRUE)

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

#on merge countries et data 

#il faut d'abord renommer le nom des pays selon le même mode de notation (ici iso3c)

countries = countries %>% mutate(CountryISO = countrycode(Country, origin="country.name", destination = "iso3c"), Region=trimws(Region)) 
data = data %>% mutate(CountryISO =countrycode(Country, origin="country.name", destination = "iso3c"))
obesitycountries <- left_join(data,countries, by= "CountryISO")

# --------------------------------------------------------------------------------
# Marion

# un graph moyenne mondiale de l'obésité selon les années (total, homme et femme)

male = filter(obesity_data, Sex=="Male")
female = filter(obesity_data, Sex=="Female")
all = filter(obesity_data, Sex=="Both sexes")

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





