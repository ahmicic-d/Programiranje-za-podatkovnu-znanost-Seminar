install.packages('ggplot2')
install.packages('plyr')
library(readr)
library(tidyr)
library(ggplot2)
library('plyr')
library(dplyr)

#------------------------------- ZADATAK 1 - UČITAVANJE PODATAKA - GLAVNI DATASET ----------------------------------
automobili <- read_csv("Car_Sales.csv")
automobili
#-------------------------------------------------------------------------------------------------------------------


#------------------------------- FREKVENCIJA POJAVLJIVANJA BRANDA AUTOMOBILA ----------------------------------
brandovi <- factor(automobili$Manufacturer)
brandovi
brandovi_automobila <- summary(brandovi)
brandovi_automobila
#--------------------------------------------------------------------------------------------------------------


#-------------------------------ZADATAK 2 - UREDJIVANJE PODATAKA  ----------------------------------
cijena_nakon_godine <- mean(automobili$`__year_resale_value`, na.rm = T) %>% round(1)
prosjek_cijena <- mean(automobili$Price_in_thousands, na.rm = T) %>% round(1)
prosjek_cijena

automobili_bez_na <- replace_na(automobili, replace=list(`__year_resale_value`=cijena_nakon_godine, Price_in_thousands=prosjek_cijena))
automobili_bez_na
#----------------------------------------------------------------------------------------------



#-------------------------------ZADATAK 3 - RAD SA DATUMIMA  ----------------------------------
datumi_zadnjih_modela <- paste(automobili$Manufacturer, automobili$Model, as.Date(automobili$Latest_Launch, format='%m-%d-%Y'))
datumi_zadnjih_modela

razlika <- difftime(max(automobili$Latest_Launch), min(automobili$Latest_Launch), units = "weeks")

sprintf("Razlika u vremenu najstarijeg i najnovijeg modela jest %f tjedana", razlika)

#----------------------------------------------------------------------------------------------



#-------------------------------ZADATAK 4 - STATISTIKA  ---------------------------------------
automobili
prosjecna_snaga <- mean(subset(automobili$Horsepower, automobili$Manufacturer=="BMW", na.rm = TRUE))

medijan_prodaje_u_tisucama<- median(automobili$Sales_in_thousands)
sprintf("Medijan prodaje u tisućama iznosi: %f ", medijan_prodaje_u_tisucama)

auto <- mode(automobili$Manufacturer)
boxplot(automobili$Engine_size, main="Zapremnina motora", ylab="Zapremnina u L")
#----------------------------------------------------------------------------------------------



#-------------------------------ZADATAK 5 - PRILAGODBA PODATKOVNIH OKVIRA  --------------------
automobili_bez_na <- automobili_bez_na %>% mutate(Sales_in_10_thousands=Sales_in_thousands/10)
automobili_bez_na

filter_automobila <- automobili_bez_na %>% filter(Sales_in_thousands>50, Price_in_thousands<35, Horsepower>150) %>% arrange(desc(Sales_in_thousands))
filter_automobila
#----------------------------------------------------------------------------------------------



#-------------------------------ZADATAK 6 - VIZUALIZACIJA PODATAKA  ---------------------------
ggplot(automobili, aes(x = Sales_in_thousands, y = Price_in_thousands)) + geom_point()

ggplot(automobili, aes(x = Price_in_thousands)) + geom_histogram()+scale_x_log10()
#----------------------------------------------------------------------------------------------








