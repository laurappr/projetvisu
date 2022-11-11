library(dplyr)        # grammar of data manipulation
library(ggplot2)      # grammar of graphics
library(stringr)      # consistent wrappers for common string operations
library(countrycode)  # convert country names and country codes
library(lubridate)    # dealing with data easier
library(RColorBrewer) # color palette

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

countries = countries %>% 
  mutate(CountryISO = countrycode(Country, origin="country.name", destination = "iso3c"), 
         Region=trimws(Region)) 

data = data %>% 
  mutate(CountryISO =countrycode(Country, origin="country.name", destination = "iso3c"))
obesitycountries <- left_join(data,countries, by= "CountryISO")

# --------------------------------------------------------------------------------
# Marion

# un graph moyenne mondiale de l'obésité selon les années (total, homme et femme)

male = filter(data, Sex=="Male")
female = filter(data, Sex=="Female")
all = filter(data, Sex=="Both sexes")

# calculer par an le taux d'obésite (entre 1975 et 2016)


# pondéré par le nombre d'habitants : weighted.mean()
# sans pondération



# calcul de l'obesité moyenne par an (pondéré par pays)

# --------------------------------------------------------------------------------
# courbe d'évolution de l'obésite par continent 

# Laura

all_obesity_country = filter(obesitycountries, Sex=="Both sexes")

#voir pour rajouter les pays enlevés 

obesity_by_region = all_obesity_country %>% 
  group_by(Region, Year) %>% 
  summarise(TotalPopulation = sum(Population), AvgObesity = mean(Obesity)) %>%
  filter(Region %in% c("ASIA (EX. NEAR EAST)", "EASTERN EUROPE", "LATIN AMER. & CARIB", "NEAR EST", "NORTHERN AFRICA", "NORTHERN AMERICA", "OCEANIA", "SUB-SAHARIAN AFRICA", "WESTERN EUROPE"))

p = ggplot(obesity_by_region, aes(Year,AvgObesity* TotalPopulation,color=Region)) +
    ggtitle("Obesity evolution from 1975 to 2016 by Region") +
    geom_line(aes(linetype=Region, group=Region))+
    scale_x_discrete(breaks = seq(1975, 2016, 10)) +
    xlab('Dates') +
    ylab('Obesity %') +
    theme_minimal()
p



# --------------------------------------------------------------------------------
# 2 graphs : carte avec l'obésite par pays en 1975 et en 2016

# Carte Laura : 1975
# Carte Marion : 2016





