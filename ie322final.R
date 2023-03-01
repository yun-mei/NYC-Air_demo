
#clear enviroment 
rm(list = ls())

#----Package -----
#install.packages("readxl")
library("readxl")

#install.packages("dplyr")
library("dplyr")

#install.packages("stringr")
library("stringr")

#install.packages("ggplot2")
library("ggplot2")

#install.packages("tidyverse")
library("tidyverse")

#install.packages('C50')
library(C50)
#----data cleaning -----
#Data for demographic in NYC includes 2010, 2020 and changes in 2010-2020 
demogr <- read_excel("~/Downloads/nyc_demogr.xlsx", sheet = 2)

#keep only 2020 data
demogr_20<- subset (demogr, select = c(2,3,6,39,51,53,55,57,59,61))
demogr_20 <- demogr_20 %>% 
  mutate_at(c(4:10), as.numeric)

#renaming columns 
colnames(demogr_20)[1:10] <- c('Geotype','Borough','Name','Pop_20',
                               'Hispanic','White','Black',
                               'Asian','Others','NH2+')
demogr_20<-demogr_20[-1:-3,]

#population of the the whole neighborhood( total)
demogr_20 <- demogr_20[str_detect(demogr_20$Geotype,"NTA2020"),na.rm = T]
demogr_20<-demogr_20[,-1]

sapply(demogr_20,class)



#EDA
barplot(Borough$Hispanic ~ Borough$Name,     
        las = 3,cex.names = 0.5,
        main= "Hispanic pop2020",
        xlab="boroughs", ylab= "population")

#eda
demogr_20 %>% 
  ggplot(aes(x = Borough, y = Pop_20,
             main="Population in Borough",
             xlab="boroughs", ylab= "population"))+
  geom_point()


                       
# to be fix 
demogr_20 %>% 
  filter(demogr_20== "Asian" |demogr_20== "White") %>% 
  ggplot(aes(demogr_20,fill= Borough))+
  geom_bar(position="dodge", alpha=0.5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(title="hs",
       x="f",
       y="d")
  
#2 variables 
qplot(Black, Asian, data = demogr_20,color=Borough)+
  theme_bw()+
theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank())+
  labs(title="hs",
       x="f",
       y="d")


# combine with gg plot 
qplot(White,data = demogr_20, fill = Borough)


                       


#regression 
m1 <- lm(Asian ~ White, data = demogr_20)

plot(m1)
summary(m1)




#---models -----
demogr20_brrm<- demogr_20[,-2:-3] 
sapply(demogr20_brrm,class)
demogr20_brrm<-demogr20_brrm%>%select(-Borough,everything())


set.seed(666)
n = nrow(demogr20_brrm)
index = sample(n, 0.7*n, replace = FALSE) # select index

#---C4.5 --- 

# training and test split
d_train <- demogr20_brrm[index,]
d_test <- demogr20_brrm[-index,]

indep_vars = c("Hispanic","White","Black","Asian","Others","NH2+")
dep_var = c("Borough")

demogr20_brrm$Borough<-as.factor(demogr20_brrm$Borough)
str(demogr20_brrm$Borough)

m1<-C5.0(d_train[,indep_vars], d_train[,dep_var])

m1<-C5.0(d_train[1:6],demogr20_brrm$Borough)
plot(m1)
summary(m1)


#---Cart--- 
#install.packages("rpart")
library(rpart)
library(rpart.plot)
cart <- rpart(Borough~.,data=demogr20_brrm, method= "class" )
rpart.plot(cart)
rpart.plot(cart, type=5, extra=102)

prediction <-predict(cart, d_test[,1:6], type= "class")
table(d_test[,1], prediction)

summary(cart)




