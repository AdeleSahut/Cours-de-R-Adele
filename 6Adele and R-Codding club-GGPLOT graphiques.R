datcount<- read.csv("C:/Users/retoschm/Documents/R Adele/Cours-de-R-Adele/Data/UKBMS_100/ukbms_count.csv", header=TRUE)
datcoord<-read.csv("C:/Users/retoschm/Documents/R Adele/Cours-de-R-Adele/Data/UKBMS_100/ukbms_coord.csv", header=TRUE)
dat<-read.csv("C:/Users/retoschm/Documents/R Adele/output/tableau dates.csv", header=TRUE)
datall<-aggregate(dat$butterfly_count/dat$section_length, list( annee=year(dat$Date),nom = dat$species_name), mean)
library(dplyr)
library(lubridate)
library(ggplot2)
library(readr)
library(gridExtra)
install.packages("tidyr")
library(tidyr)

#Exo codding club (https://ourcodingclub.github.io/2017/01/29/datavis.html)
LPI<-read.csv("C:/Users/retoschm/Documents/R Adele/Cours-de-R-Adele/Data/LPIdata_CC.csv",header=TRUE)
LPI2<-gather(LPI,"Years", "Abundance", 9:53 )
LPI2$Years<-parse_number(LPI2$Years) #On transforme les annees en numerique, sans lettres
LPI2$Abundance <- as.numeric(LPI2$Abundance)
unique(LPI2$Common.Name)
Grif<-filter(LPI2,Common.Name=="Griffon vulture / Eurasian griffon")
#Par defaut on aura en y la frequence de chaque abondance de delphin
install.packages("colourpicker")
library(colourpicker)


##Graphique en points ou lignes
  ##geom_point()  geom_line()
#On veux etudier comportement juste en croatie et Italie
gridIC<-filter(Grif, Country.list %in% c("Croatia","Italy"))
head(gridIC)
summary(gridIC)
ggplot(gridIC, aes(Years,Abundance, colour=Country.list))+ 
  geom_point()+
  xlim(1985,2010)+
  geom_point(size = 4)+ 
  geom_smooth(method = "lm", aes(fill=Country.list))+   #ajouter modele lineaire pour chaque pays
  scale_fill_manual(values = c("#CD3278", "#66CDAA")) + #Changer le theme couleur pour chaque pays
  scale_colour_manual(values = c("#CD3278", "#66CDAA"))
 #Tout pour embellir un graphique:https://ourcodingclub.github.io/2017/01/29/datavis.html

##Boxplot
  ##geom_boxplot()
#On veux comparer la croatie et l'Italie
ggplot(gridIC,aes(Country.list,Abundance))+
  geom_boxplot(aes(fill = Country.list)) +
  scale_colour_manual(values = c("#B03060", "#FFB90F"))+
  scale_fill_manual(values = c("#CD3278", "#66CDAA"))+
  xlab("Abondance")+ylab("Pays")+
  theme(panel.grid=element_blank()) # supprimer le fond quadrille

##Barplot
  head(LPI2)
  rich <- LPI2 %>% filter(Country.list %in% c("Canada","United States","Brazil"))%>%
  group_by(Country.list) %>% mutate(richesse=length(unique(Common.Name)))


 a <- unique(select(rich,Country.list, richesse))
 
   ggplot(a, aes(x = Country.list, y = richesse)) + 
     geom_bar(position = position_dodge(), , stat = "identity",colour = "black", fill = "#00868B")
   
  
   