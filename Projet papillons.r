datcount<- read.csv("C:/Users/retoschm/Documents/R Adele/Cours-de-R-Adele/Data/UKBMS_100/ukbms_count.csv", header=TRUE)
datcoord<-read.csv("C:/Users/retoschm/Documents/R Adele/Cours-de-R-Adele/Data/UKBMS_100/ukbms_coord.csv", header=TRUE)
library(dplyr)
library(lubridate)
#Dans datcount on reunit les colonnes jour-mois-year sous forme de Date
datcount$Date<-dmy(paste(datcount$day,datcount$month,datcount$year))
#on cree deux nouvelles tables avec les colonnes concernees par le projet
datcount1<-select(datcount,"site_id","species_name", "butterfly_count", "Date")
datcoord1<-select(datcoord,"site_id","section_length")
#On joint les deux tables par le numero du site
dat<-right_join(datcount1,datcoord1, by = "site_id")
#On exporte cette nouvelle table
write.csv(dat, "./R Adele/output/tableau dates.csv")

#STEP ONE: idee pas subtile: recuperer la moyenne de comptage d'un papillon sur une annee, par metre
  dat1990<-filter(dat, year(Date)==1990 & species_name=="Anthocharis cardamines")
  #moyenne du nombre de Anthocharis par metre en 1990
  mean1990<-mean(dat1990$butterfly_count/dat1990$section_length)

  dat1995<-filter(dat, year(Date)==1995 & species_name=="Anthocharis cardamines")
  mean1995<-mean(dat1995$butterfly_count/dat1995$section_length)

  dat2000<-filter(dat, year(Date)==2000 & species_name=="Anthocharis cardamines")
  mean2000<-mean(dat2000$butterfly_count/dat2000$section_length)

  dat2005<-filter(dat, year(Date)==2005 & species_name=="Anthocharis cardamines")
  mean2005<-mean(dat2005$butterfly_count/dat2005$section_length)

  dat2010<-filter(dat, year(Date)==2010 & species_name=="Anthocharis cardamines")
  mean2010<-mean(dat2010$butterfly_count/dat2010$section_length)

  dat2015<-filter(dat, year(Date)==2015 & species_name=="Anthocharis cardamines")
  mean2015<-mean(dat2015$butterfly_count/dat2015$section_length)

  evol<-data.frame(Annee=c(1990,1995,2000,2005,2010,2015),Anthocharis_par_metre=c(mean1990,mean1995,mean2000,mean2005,mean2010,mean2015))
  evol
  plot(evol)
  
#STEP TWO: Essayer de le faire pour toutes les annees, un tableau par papillon, sans y passer mille ans
  datall<-aggregate(dat$butterfly_count/dat$section_length, list( annee=year(dat$Date),nom = dat$species_name), mean)
  #la on reunit les moyennes des comptages en fonction des annees papillon par papillon c'est parfait
  #Comme il faut pas oublier de ponderer par la longueur de chaque section, on a /section_lenght
  filtAntho<-filter(datall, nom=="Anthocharis cardamines")
  plot(filtAntho$x~datall$annee)
  #la on a trace une ligne qui ne represente que l'evol des Antho
  plot(filter(datall, nom=="Anthocharis cardamines")~datall$annee)
  
  #Final countdown:
  
  list_sp <- as.character(unique(datall$nom))

  
  plot(datall[datall$nom==list_sp[1], "annee"],datall[datall$nom==list_sp[1], "x"], type = 'l', ylim = c(0, max(datall$x)))
  
  for(i in c(2:length(list_sp))){
  points(datall[datall$nom==list_sp[i], "annee"],datall[datall$nom==list_sp[i], "x"], type = 'l', col = i)
  }

  library(ggplot2)

  ggplot(data=datall,
         aes(x=annee, y=x, colour=nom)) +
    geom_line()
  
  
    
  