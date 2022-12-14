library(tidyverse)        # functions that help get to tidy data
library(dplyr)            # grammar of data manipulation
library(ggplot2)          # grammar of graphics
library(stringr)          # consistent wrappers for common string operations
library(countrycode)      # convert country names and country codes
library(lubridate)        # dealing with data easier
library(RColorBrewer)     # color palette
library(viridis)          # color for daltonian
library(tmap)             # package for maping
library("gridExtra")      # package pour fusionner des graphes
library("cowplot")        # package pour fusionner des graphes
library("flexdashboard")  # create a flexdashboard

# --------------------------------------------------------------------------------
# importation des données 
data = read.csv("data/obesity-cleaned.csv",sep=",",header=T)
summary(data)

# Import des données countries of the world
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

# Ajout des pays au format Iso3 pour la fusion des 2 fichies
data = data %>% 
  mutate(CountryISO =countrycode(Country, origin="country.name", destination = "iso3c"))

#on merge avec World où on a direct les continents 
# importation des données du monde nécessaire à la carto
data("World")
colnames(World) = c("CountryISO",colnames(World[-1]))
obesitycountries2 <- left_join(data, World, by= "CountryISO")

# --------------------------------------------------------------------------------

# un graph moyenne mondiale de l'obésité selon les années (total, homme et femme)

# -----------------------------------
# pondéré par le nombre d'habitants : 

all_obesity_country = filter(obesitycountries2, Sex=="Both sexes")

df = all_obesity_country[c("Country","Year","pop_est","Obesity")]
summary(df)

# sup des données manquantes pour la pop
df=df[!is.na(df$pop_est),]

# calcul de la moyenne pondérée par an
w_all_year = tapply(seq_along(df$Obesity),df$Year,
       function(xx){return(weighted.mean(x=df$Obesity[xx],w=df$pop_est[xx]))})

w_all_year = as.data.frame(cbind(year=1975:2016,mean=w_all_year))
w_all_year$year = as.factor(w_all_year$year)

# convertion au format date
w_all_year <- mutate(w_all_year, date = str_c(w_all_year$year, "01-01", sep = "-") %>% ymd())

# representation graphique
theme_strip <- theme_minimal()+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title = element_blank(),
        panel.grid.major = element_blank(),
        #legend.title = element_blank(),
        axis.text.x = element_text(vjust = 3),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 14, hjust=0.5, face = "bold"),
        plot.caption = element_text(face = "italic", hjust=0.5))

col_strip <- brewer.pal(11, "RdYlGn")

ggplot(w_all_year,
       aes(x = date, y = 1, fill = mean))+
  geom_tile()+
  scale_x_date(date_breaks = "6 years",
               date_labels = "%Y",
               expand = c(0, 0))+
  scale_y_continuous(expand = c(0, 0))+
  scale_fill_gradientn(colors = rev(col_strip))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "Obesity 1975-2016",
       caption = "Data : Adult obesity, population-weighted by country, between 1975-2016.",
       fill="Obesity %")+
  theme_strip

# --------------------------------------------------------------------------------
# courbe d'évolution de l'obésite par continent 

# 1 avec dataset World et région non continents
# courbe d'évolution de l'obésite par continent 

# 1 avec dataset World et région non continents
all_obesity_country = filter(obesitycountries2, Sex=="Both sexes")

# voir pour rajouter les pays enlevés 

all_obesity_country=all_obesity_country[!is.na(all_obesity_country$pop_est),]
all_obesity_country=all_obesity_country[!is.na(all_obesity_country$continent),]

obesity_by_region = all_obesity_country %>% 
  group_by(continent, Year) %>% 
  summarise(TotalPopulation = sum(pop_est), AvgObesity = mean(Obesity),
            weighted_mean = weighted.mean(x=Obesity,w=pop_est))


p2 = ggplot(obesity_by_region, aes(Year,weighted_mean,color=continent)) +
  ggtitle("Obesity evolution from 1975 to 2016 by continent") +
  geom_line(aes(linetype=continent, group=continent), linetype="solid", size=1.5)+
  scale_x_discrete(breaks = seq(1975, 2016, 10)) +
  scale_colour_discrete(name  ="Continent",
                        breaks=c("North America", "Oceania", "Europe", "South America", "Africa", "Asia"))+
  xlab('Year') +
  ylab('Obesity %') +
  theme_minimal() 
#scale_color_brewer(palette = "Dark2")


# --------------------------------------------------------------------------------
# 2 graphs : carte avec l'obésite par pays en 1975 et en 2016

# Carte 1975 --------------------

# création d'un fichier avec taux d'obésité par pays en 1975
obesity_1975 = filter(obesitycountries2, Year=="1975")
obesity_1975 = filter(obesity_1975, Sex=="Both sexes")
obesity_1975 = filter(obesity_1975, CountryISO!="ATA")
World=filter(World, CountryISO!="ATA")

# on fusionne les données du monde et notre dataset sur l'obésité en 2016 (grace à CountryISO)
obesity_1975_world <- World %>%
  left_join(obesity_1975)

# carte choroplèthe selon l'obésité en 1975
map_sdg_indicators1 <- obesity_1975_world %>% 
  ggplot() + 
  geom_sf(aes(fill = Obesity),color="white",size=.2)+
  theme_void()+
  theme(panel.background = element_rect(fill = "white"))+
  scale_fill_gradientn(colors = rev(col_strip), limits = c(0, 40),
                       breaks=c(10, 20, 30))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "Obesity rates by country in 1975",
       caption = "Data : Adult obesity by country in 1975.")
map_sdg_indicators1

# ------------------------------
# Carte 2016 ---------------

# création d'un fichier avec taux d'obésité par pays en 2016
obesity_2016 = filter(obesitycountries2, Year=="2016")
obesity_2016 = filter(obesity_2016, Sex=="Both sexes")
obesity_2016 = filter(obesity_2016, CountryISO!="ATA")
World=filter(World, CountryISO!="ATA")

# on fusionne les données du monde et notre dataset sur l'obésité en 2016 (grace à CountryISO)
obesity_2016_world <- World %>%
  left_join(obesity_2016)

# carte choroplèthe selon l'obésité en 2016
map_sdg_indicators2 <- obesity_2016_world %>% 
  ggplot() + 
  geom_sf(aes(fill = Obesity),color="white",size=.2)+
  theme_minimal()+
  theme(panel.background = element_rect(fill = "light blue"))+
  
  scale_fill_gradientn(colors = rev(col_strip),limits = c(0, 40),
                       breaks=c(10, 20, 30))+
  guides(fill = guide_colorbar(barwidth = 1))+
  labs(title = "Obesity rates by country in 2016",
       caption = "Data : Adult obesity by country in 2016.")
map_sdg_indicators2

# On combine les 2 graphes
# http://www.sthda.com/french/wiki/ggplot2-combiner-plusieurs-graphiques-sur-la-m-me-page-logiciel-r-et-visualisation-de-donn-es

plot_grid(map_sdg_indicators1, map_sdg_indicators2, labels=c("1975", "2016"), ncol = 2, nrow = 1)

grid.arrange(map_sdg_indicators1,map_sdg_indicators2,nrow=1)

