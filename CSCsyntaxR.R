rm(list=ls(all=TRUE)) 
library(stringr)
library(RCurl)
library(googleVis)
fema="https://docs.google.com/spreadsheet/pub?key=0AgEOIhph65nJdGI3d3NkY2llR3FrSUdTY05HNkU5X2c&single=true&gid=0&output=csv"
credDates="https://docs.google.com/spreadsheet/pub?key=0AgEOIhph65nJdHV2dzEwdkFVaVhvZ3NFcDZSMUpzS3c&single=true&gid=0&output=csv"
credDetails="https://docs.google.com/spreadsheet/pub?key=0AgEOIhph65nJdDA4VTA3Yy1CQjRPR2dYa2xKUlg1YXc&single=true&gid=0&output=csv"
glide="https://docs.google.com/spreadsheet/pub?key=0AgEOIhph65nJdHdTSWhtQXdlYjRnNmxyR3JPOV9hbXc&single=true&gid=0&output=csv"
strauss="https://docs.google.com/spreadsheet/pub?key=0AgEOIhph65nJdEVQZUtxc1Y3WjBfeV9CUmJIakYzdFE&single=true&gid=0&output=csv"
canada="https://docs.google.com/spreadsheet/pub?key=0AhVqDdZgThPldEo5Y3dvblA0OTRFT1dGRTZsTVpmcXc&output=csv"
###CANADA
myCsv <- getURL(canada)
data=read.csv(textConnection(myCsv))
names(data)
EventCount=as.data.frame(table(data$LOC))
EventPercent=as.data.frame(prop.table(table(data$LOC)))
EventPercent=EventPercent[-3,]
EventCount=EventCount[-3,]
names(EventCount)=c('ProvinceOrTerritory','Count')
names(EventPercent)=c('ProvinceOrTerritory','Percent')
CanadaDis=gvisGeoMap(EventPercent, locationvar="ProvinceOrTerritory", numvar="Percent",
                  options=list(height=300, region="CA"), chartid="Events") 
plot(CanadaDis)

CanadaDis2<-gvisGeoChart(EventPercent,
                         "ProvinceOrTerritory",
                         "Percent",
                         options=list(region="CA",displayMode="Regions",
                                      backgroundColor="lightblue", 
                                      resolution="provinces",
                                      keepAspectRatio=FALSE,width=800,height=600),
                               chartid="DisastersperRegion")
plot(CanadaDis2)

##CRED
myCsv <- getURL(credDates)
data=read.csv(textConnection(myCsv))
EventCount=as.data.frame(table(data$Gcountry))
EventPercent=as.data.frame(prop.table(table(data$Gcountry)))
EventCount=subset(EventCount,EventCount[,1]!='')
EventPercent=subset(EventPercent,EventPercent[,1]!='')

names(EventCount)=c('Country','Count')
names(EventPercent)=c('Country','Percent')

Count=EventCount$Count
Ranking=rank(-Count,ties.method = c("random"))
par(mfrow=c(2,1))
plot(Ranking,Count)
plot(log(Ranking),log(Count))

Map<- data.frame(EventPercent$Country, EventPercent$Percent)
names(Map)<- c("Country", "Percent")
Geo=gvisGeoMap(Map, locationvar="Country", numvar="Percent",
               options=list(height=300, dataMode='regions'))
plot(Geo)
## just africa 
dataAfrica=subset(data,str_sub(data[,4],-6,-1)=='AFRICA') #LAST 6 CHARACTERS 'AFRICA'?
AfricaEventCount=as.data.frame(table(droplevels(dataAfrica)$Gcountry))
AfricaEventPercent=as.data.frame(prop.table(table(droplevels(dataAfrica)$Gcountry)))
AfricaEventCount=subset(AfricaEventCount,AfricaEventCount[,1]!='')
AfricaEventPercent=subset(AfricaEventPercent,AfricaEventPercent[,1]!='')

names(AfricaEventCount)=c('Country','Count')
names(AfricaEventPercent)=c('Country','Percent')

Count=AfricaEventCount$Count
Ranking=rank(-Count,ties.method = c("random"))
par(mfrow=c(2,1))
plot(Ranking,Count)
plot(log(Ranking),log(Count))

Africa=gvisGeoMap(AfricaEventPercent, locationvar="Country", numvar="Percent",
               options=list(height=300, region="002")) 
plot(Africa)


## FEMA 
#########

myCsv <- getURL(fema) 
data=read.csv(textConnection(myCsv))
EventCountbyState=as.data.frame(table(data$State))
names(EventCountbyState)=c('state.name','Count')

EventPercentbyState=as.data.frame(prop.table(table(data$State)))
names(EventPercentbyState)=c('state.name','Percent')

Count=EventCountbyState$Count
Ranking=rank(-Count,ties.method = c("random"))
par(mfrow=c(2,1))
plot(Ranking,Count)
plot(log(Ranking),log(Count))

require(datasets)
statesUS <- data.frame(state.name)
femaPlot=merge(statesUS,EventPercentbyState,by="state.name")
fema1 <- gvisGeoMap(femaPlot, "state.name", "Percent",
                 options=list(region="US", dataMode="regions",
                              width=600, height=400))
plot(fema1)


####STRAUSS
# issue1 / issue2 / issue3 - variables 32, 33, 34
# 3 = food, water, subsistence
# 4 = environmental degradation

myCsv <- getURL(strauss)
dataAfrica=read.csv(textConnection(myCsv))
summary(dataAfricaP[,32])
table(dataAfricaP[,33])

dataAfrica$Gcode
names(dataAfricaP)
dataAfrica1=subset(dataAfrica, (dataAfrica[,32]==3  | dataAfrica[,32]==4))
dataAfricaP= subset(dataAfrica, (dataAfrica[,32]!=3 & dataAfrica[,32]!=4))
dataAfrica2=subset(dataAfricaP, (dataAfricaP[,33]==3  | dataAfricaP[,33]==4))
dataAfricaP= subset(dataAfricaP, (dataAfricaP[,33]!=3 & dataAfricaP[,33]!=4))
dataAfrica3=subset(dataAfricaP, (dataAfricaP[,34]==3  | dataAfricaP[,34]==4))
AfricaConflictClimate=rbind(dataAfrica1,dataAfrica2,dataAfrica3)

AfricaEventCount=as.data.frame(table(AfricaConflictClimate$Gcode))
AfricaEventPercent=as.data.frame(prop.table(table(AfricaConflictClimate$Gcode)))

AfricaEventCount=subset(AfricaEventCount,AfricaEventCount[,1]!='')
AfricaEventPercent=subset(AfricaEventPercent,AfricaEventPercent[,1]!='')

names(AfricaEventCount)=c('Country','Count')
names(AfricaEventPercent)=c('Country','Percent')

Count=AfricaEventCount$Count
Ranking=rank(-Count,ties.method = c("random"))
par(mfrow=c(2,1))
plot(Ranking,Count)
plot(log(Ranking),log(Count))

Africa=gvisGeoMap(AfricaEventPercent, locationvar="Country", numvar="Percent",
                  options=list(height=300, region="002"))
plot(Africa)
