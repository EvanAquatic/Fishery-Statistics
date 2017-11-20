# Fishery-Statistics
# R
#Data Setup for Gabelhouse Length Categories 

DataManipInchLake<- read.csv('C:/Users/ekwityn/Desktop/InchLake2.csv',header=TRUE, blank.lines.skip = TRUE,na.strings = "NA",stringsAsFactors=FALSE); 
data(DataManipInchLake)
str(DataManipInchLake)

levels(DataManipInchLake$species)   #Determines the different species in the dataframe


library(dplyr)     
library(FSA)      
library(FSAdata)  


DataManipInchLake$length_cm<-DataManipInchLake$length*2.54  #Creation of new variables; new column for fish length in mm
DataManipInchLake$length_mm<-DataManipInchLake$length*25.4  #Creation of new variables; new column for fish length in mm
DataManipInchLake$gcat<-psdAdd(DataManipInchLake$length_cm, DataManipInchLake$species) #('Stock', 'Quality', 'Preferred', 'Memorable','Trophy')


NewInchLake<-data.frame(DataManipInchLake$year, DataManipInchLake$species, DataManipInchLake$length, 
                        DataManipInchLake$length_cm, DataManipInchLake$length_mm, DataManipInchLake$weight)   #Creation of new dataframe
names(NewInchLake)<-c("year", "species", "tl_inch", "tl_cm","tl_mm", "wt_g") #New datadrame output with new header names; length=lt and weight=wt


NewInchLake<-mutate(NewInchLake, gcat=psdAdd(tl_mm,species)) #Determining Gabelhouse length categories all species output indicates substock, stock, quality, preferred, memorable, and trophy
NewInchLake<-mutate(NewInchLake, gcatv=psdAdd(tl_mm,species,use.names = FALSE)) # #Determining Gabelhouse length categories all species output indicates substock, stock, quality, preferred, memorable, and trophy; , units="English" indicates measurement, do not include if using metric, formula generally assumes (mm,g) 
NewInchLake<-mutate(NewInchLake, Wr=wrAdd(wt_g,tl_mm,species)) #Determining Gabelhouse length categories all species output indicates substock, stock, quality, preferred, memorable, and trophy
NewInchLake[seq(1,501,50),] #Examines every 50th row of your data; used as a check 


bg.il<-subset(NewInchLake, species=="Bluegill")          #output contains only Bluegill for 2007 & 2008, and also removed NedID and fishID, Also relabeled length to length_inch
lmb.il<-subset(NewInchLake, species=="Largemouth Bass")  #output contains only Largemouth Bass for 2007 & 2008, and also removed NedID and fishID, Also relabeled length to length_inch


nongame.il<-subset(NewInchLake, species %in% c("Bluntnose Minnow", "Fathead Minnow", "Iowa Darter", "Tadpole Madtom"))  #subsetting dataframe by nongame fish species for 2007 & 2008
nongame.il$species  #Indicates the proper output
nongame.il[order(nongame.il[,1],nongame.il[,2], nongame.il[,3]),] #Sorting data by species alp and also speices length within year ; https://chartio.com/resources/tutorials/how-to-sort-a-data-frame-by-multiple-columns-in-r/


NewInchPSD <- NewInchLake %>%
  filter(gcat!="zero") %>%
  mutate(gcat=droplevels(gcat)) %>%
  filter(!is.na(gcat))   %>%
  mutate(species=droplevels(species)) #Drops species that are not in the Gabelhouse length catergory list

  
freq <- xtabs(~species+gcat,data=NewInchPSD)
iPSDs <- prop.table(freq,margin=1)*100
round(iPSDs,1)


PSDs <- t(apply(iPSDs,MARGIN=1,FUN=rcumsum))
round(PSDs,1)

Summarize(Wr~species,data=NewInchLake,digits=1)

Summarize(Wr~gcat*species,data=NewInchLake,digits=1)

