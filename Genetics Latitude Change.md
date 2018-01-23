# Fishery-Statistics
R
divloci1 = read.csv('~/Desktop/Fishery_lat/Fishery_lat_msats_000_DHL.csv',header=TRUE, blank.lines.skip = TRUE,na.strings = "NA", stringsAsFactors=FALSE)
divloci2 = read.csv('~/Desktop/Fishery_lat/Fishery_lat_msats_001_DHL_2012-06-07.csv',header=TRUE, blank.lines.skip = TRUE,na.strings = "NA", stringsAsFactors=FALSE)
divloci3 = read.csv('~/Desktop/Fishery_lat/Fishery_lat_msats_002_DHL_2012-06-11.csv', header=TRUE, blank.lines.skip = TRUE,na.strings = "NA", stringsAsFactors=FALSE)
divloci4 = read.csv('~/Desktop//Fishery_lat/Fishery_lat_msats_100_KJG_(all_pdfs_2).csv',header=TRUE, blank.lines.skip = TRUE,na.strings = "NA", stringsAsFactors=FALSE)
divloci5 = read.csv('~/Desktop/Fishery_lat/Fishery_lat_msats_101_KJG_MLP_2012-06-13.csv',header=TRUE, blank.lines.skip = TRUE,na.strings = "NA", stringsAsFactors=FALSE)

divloci6 <- rbind(divloci1, read.csv( '~/Desktop/Fishery_lat/Fishery_lat_msats_100_KJG_(all_pdfs_2).csv'))
divloci10 <- rbind(divloci6, read.csv('~/Desktop/Fishery_lat/Fishery_lat_msats_002_DHL_2012-06-11.csv'))
divloci20 <- rbind(divloci10, read.csv('~/Desktop/Fishery_lat/Fishery_lat_msats_000_DHL.csv' ))
divloci <- rbind(divloci20, read.csv('~/Desktop/Fishery_lat/Fishery_lat_msats_001_DHL_2012-06-07.csv'))

divloci <- lapply(divloci1, divloci2, divloci3, divloci4, divloci5, read.csv, header = TRUE)

#rbind all datasheets will be characters 
divloci$CommonName
species = length(divloci$spp)
sum(!is.na(divloci$He)) # 846
#c<-as.numeric(divloci$spp)
divloci$lat_deg<-as.numeric(divloci$lat_deg)
divloci$lat_min<-as.numeric(divloci$lat_min)
divloci$lat_sec<-as.numeric(divloci$lat_sec)

summary(divloci)
head(divloci) #first rows

# convert deg/min/sec to decimal deg


divloci$lat = rowSums(cbind(divloci$lat_deg, 
                            divloci$lat_min/60, 
                            divloci$lat_sec/3600), na.rm=TRUE)


div= data.frame (divloci$spp)
  
######################################################################
####PLOT ALL SPECIES###########
allspp = unique(divloci$spp)
spp=divloci$spp

plot(x=1, y=1, xlim=range(-70:70),ylim=c(0.0,1.0),col="white", xlab='Latitude', ylab='He') #plots empty graph

lm = lm(He ~ lat + I(lat^2), data=divloci)
summary(lm)
x = seq(-70,70, by=1) 
y = predict(lm, newdata=data.frame(lat = x))
lines(x,y, col='red')

#p1-p68
plotcolorvector <- rainbow(length(allspp))
#plotcolorvector <- c("red", "green", "blue")

pdf(width=6, height=6,file="test.pdf")
for(i in 1:length(spp)) {
for(i in 1:length(allspp)) {
    thisdata <- subset(divloci, subset = divloci$spp==allspp[i])
    plot(thisdata$lat, thisdata$He, col=plotcolorvector[i], cex=0.5, main=allspp[i],xlim=c(-70, 70), ylim=c(0,1))
    
 if(length(unique(thisdata$lat))>=2 & 
      sum(!is.na(thisdata$He))>3 )
     #length(unique(thisdata$spp))>=10)
    #sum(!is.na) sum of TRUE/FALSE !"Not" is.na; Needs to have 3 true statements to proceed 
  #  else(length(unique(thisdata$He))>=NA)
     # aggregate(nrow(thisdata$p1-thisdata$p68)Fun=length)
    {
 
 lm = lm(He ~ lat + I(lat^2), data=thisdata)
      # summary(lm)
    y = predict(lm, newdata=data.frame(lat = x))
 lines(x,y, col='red')

  }
}

dev.off()
}

require(lattice)
xyplot(He~lat|allspp, thisdata) 

lm = lm(He ~ lat + I(lat^2), data=thisdata)
y = predict(lm, new.data=data.frame(lat_deg = seq(-80, 80, by=1), col='red')
            panel = function(xy)) {
              panel.xyplot(x,y)
}}

 

###################################
##END PLOT FOR ALL SPECIES######



##################################################
#LATTICE##########################################
##################################################


glm.out = glm(cbind(He) ~ lat_deg, family=quasibinomial(logit), data=divloci)

species <-as.factor(divloci$spp)
species=(1:10) #Gadus macrocephalus 
species=[11:130]#Fundulus heteroclitus
speceis=[131:154]# Epinephelus coioides

c<-cbind(divloci$spp, divloci$He)


Piv<-tapply(divloci$He, 
            divloci$spp, FUN=length)

require(lattice)

xyplot(He ~ lat_deg | Source, data=divloci)

y = predict(lm, new.data=data.frame(lat_deg = seq(-70, 70, by=1)))


length(unique(divloci$Source))
