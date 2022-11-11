library(dplyr)        # grammar of data manipulation
library(ggplot2)      # grammar of graphics
library(stringr)      # consistent wrappers for common string operations
library(countrycode)  # convert country names and country codes
library(lubridate)    # dealing with data easier
library(RColorBrewer) # color palette
library(viridis)      #color for daltonian


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

# on merge countries et data 

# il faut d'abord renommer le nom des pays selon le même mode de notation (ici iso3c)

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

# -----------------------------------
# -------- sans pondération ---------

# calculer par an le taux d'obésite (entre 1975 et 2016)
all_year = all %>%
              group_by(Year) %>%
              summarise_at(vars(Obesity), list(mean = mean))

# convertion au format date
all_year <- mutate(all_year, date = str_c(all_year$Year, "01-01", sep = "-") %>% ymd())

# representation graphique
theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, hjust=0.5, face = "bold"),
        plot.caption = element_text(face = "italic", hjust=0.5))

col_strip <- brewer.pal(11, "RdYlGn")

ggplot(all_year,
       aes(x = date, y = 1, fill = mean))+
  geom_tile()+
  scale_x_date(date_breaks = "6 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "Obesity 1975-2016",
       caption = "Data : Obesity among adults by country, 1975-2016")+
  theme_strip

# -----------------------------------
# pondéré par le nombre d'habitants : 

all_bis = filter(obesitycountries, Sex=="Both sexes")

all_year_bis = all_bis %>%
  group_by(Year) %>%
  weighted.mean(Obesity,Population)

weighted.mean(all_bis$Obesity,all_bis$Population)

# calcul de l'obesité moyenne par an : pondéré par pays
weighted.mean(all_year_bis$mean,all_year_bis$Population)




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
    theme_minimal() + 
    scale_color_brewer(palette = "Dark2")

p



# --------------------------------------------------------------------------------
# 2 graphs : carte avec l'obésite par pays en 1975 et en 2016

# Carte Laura : 1975
# Carte Marion : 2016





