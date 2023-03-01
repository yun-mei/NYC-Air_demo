
#clear enviroment 
rm(list = ls())

#----Package -----
#install.packages("readxl")
library("readxl")

#install.packages("dplyr")
library("dplyr")

#install.packages("stringr")
library("stringr")

#----Population -----
#Data for demographic in NYC includes 2010, 2020 and changes in 2010-2020 
demogr <- read_excel("~/Downloads/nyc_demogr.xlsx", sheet = 2)
demogr <- demogr %>% 
  mutate_at(c(7:98), as.numeric)

#keep only 2020 data
demogr_20<- subset (demogr, select = c(2,3,6,39,51,53,55,57,59,61))

#renaming columns 
colnames(demogr_20)[1:10] <- c('Geotype','Borough','Name','Pop_20',
                              'Hispanic','White','Black',
                              'Asian','Others','NH 2 or more')
demogr_20<-demogr_20[-1:-3,]

#population of the the whole broughs( total)
Borough<- demogr_20[str_detect(demogr_20$Geotype,"Boro"),na.rm = T]
Borough<-Borough[-1:-2]
Community<- demogr_20[str_detect(demogr_20$Geotype,"CD"),na.rm = T]
Community<-Community[,-1]
demogr_20 <- demogr_20[str_detect(demogr_20$Geotype,"NTA2020"),na.rm = T]
demogr_20<-demogr_20[,-1]

sapply(demogr_20,class)

demogr_20_rm <- demogr_20[,-3]

#____

#call only queens in 2020 
qu_pop<- demogr_20[str_detect(demogr_20$Borough,"Queens"),na.rm = T]
qu_pop <- na.omit(qu_pop) 


#EDA
barplot(Borough$Hispanic ~ Borough$Name,     
        las = 3,cex.names = 0.5,
        main= "Hispanic pop2020",
        xlab="boroughs", ylab= "population")

#least pop
minpop<- demogr_20  %>%
 # filter(Pop_20 == min(Pop_20)) %>%
  filter(Pop_20 >= 1)%>%
  select(Name,Borough,Pop_20)
minpop

#Kmean
clust <- kmeans(qu_pop$Pop_20,centers=9)
clust
plot(qu_pop$Pop_20,clust$cluster,pch=16, col=factor(clust$cluster))
clust$centers
print(clust)
plot(1:82,qu_pop$Pop_20,pch=16,col=factor(clust$cluster) )


#regression 
m1 <- lm(Asian ~ White, data = demogr_20)

plot(m1)
summary(m1)


qu_pop<- demogr_20[str_detect(demogr_20$Borough,"Queens"),na.rm = T]


s<- qu_pop  %>%
  filter(Asian == max(Asian)) %>%
  select(Name)




#---- Air Quality------

#data for airq within diff broiugh from 2014 to 2022
airq <-read.csv("~/Downloads/Air_Quality.csv") 
airq<- subset (airq, select = c(3:5,8:11))
colnames(airq)[1:7] <- c('Type','Measure','Measure_info','Area',
                               'Time','Date','Data')

airq$Date <- as.Date(airq$Date, '%m/%d/%Y')
air_20<-airq[format(airq$Date, '%Y') == "2020",] 

air_Boro<-filter(air_20, Area %in%  c("Queens", "Bronx","Brooklyn",
                              "Staten Island" ,"Manhattan"  ))

#air_O3<-filter(air_20, Type %in%  c("Fine Particulate Matter (PM2.5)"))
#air_O3cd<- air_O3[str_detect(air_O3$Area,"CD"),]
#air_da<-air_O3[!(air_O3$Area %in% air_O3cd$Area),]

#max(air_da$Data)



a<- airq[,-2:-3]


#air quality 2020 in summer 
air_Boro_su<-filter(air_Boro, Time %in%  c("Summer 2020"  ))

NO<-c(10,13.6,10.82,7.83,15.97)
PM<-c(6.87,7.24,6.94,6.58,7.80)
O3<-c(30.52,30.85,29.69,28.61,28.77)

a<-rbind(NO,PM,O3)
a <- as.data.frame(a)

colnames(a)<-c("Queens", "Bronx","Brooklyn",
               "Staten Island" ,"Manhattan"  )
rownames(a)<-c("Nitrogen Dioxide", 
               "Fine Particulate Matter","Ozone (O3)")





# call pm only 
pmonly<- subset(airq,airq$Type == 'PM2.5-Attributable Deaths')


#plot(pmonly$Data.Value,main="PM Histogram", 
# ylab= "Attributable Deaths")

#cluster 
i <- grep("Data", names(pmonly))
x <- pmonly[, i]
cl <- kmeans(x, 8, nstart = 100)
plot(x, col = cl$cluster, pch=10, main="Pm in NYC ")
#let's evaluate the model: What is the between_SS / total_SS?
print(cl)

#kmean 
cluster <- kmeans(pmonly$Data,centers=13)
cluster
plot(pmonly$Data,cluster$cluster,pch=16, col=factor(cluster$cluster))
cluster$centers
print(cluster)
plot(1:192,pmonly$Data,pch=16,col=factor(cluster$cluster) )

cluster <- kmeans(air_Q$Data,centers=12)
cluster
plot(air_Q$Data,cluster$cluster,pch=16, col=factor(cluster$cluster))
cluster$centers
print(cluster)
plot(1:146,air_Q$Data,pch=16,col=factor(cluster$cluster) )




