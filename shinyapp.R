#----Package -----
#install.packages("readxl")
library("readxl")

#install.packages("dplyr")
library("dplyr")

#install.packages("stringr")
library("stringr")

#-----pop-------
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

#------data------
data <- as.data.frame(t(Borough))
data
names(data)<-data[1,]
data=data[-1:-2,]

data <- data %>% 
  mutate_at(c(1:5), as.numeric)

#-----air----
airq <-read.csv("~/Downloads/Air_Quality.csv") 
airq<- subset (airq, select = c(3:5,8:11))
colnames(airq)[1:7] <- c('Type','Measure','Measure_info','Area',
                         'Time','Date','Data')

airq$Date <- as.Date(airq$Date, '%m/%d/%Y')
air_20<-airq[format(airq$Date, '%Y') == "2020",] 

air_Boro<-filter(air_20, Area %in%  c("Queens", "Bronx","Brooklyn",
                                      "Staten Island" ,"Manhattan"  ))
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


#-----ui-----
# Use a fluid Bootstrap layout
ui<-fluidPage(    
  
  # Give the page a title
  titlePanel("Demographics and Air Quality in NYC"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("Boroughs", "Boroughs:", 
                  choices=colnames(data)),
      hr(),
      helpText("Data for demographic in NYC from Census 2020")
    ),
    
    # Create a spot for the barplot
    mainPanel( 
      
      tabsetPanel(type = "tabs",
                  tabPanel("Demo Plot", plotOutput("racePlot")),
                  tabPanel("Air Plot", plotOutput("airPlot")),
                 # tabPanel("Table", tableOutput("table")),
      )
    )
    
  ))

names <- c('Hispanic','White','Black',
           'Asian','Others','NH 2 or more')

#------server----
server<- function(input, output) {
  
  # Fill in the spot we created for a plot
  output$racePlot <- renderPlot({
    
    # Render a barplot
    barplot(data[,input$Boroughs], 
            names.arg=names, cex.names = 0.8,
            main=input$Boroughs,
            ylab="Population",
            xlab="Race")
  })
  
  airname<-c("Nitrogen Dioxide", 
             "Fine Particulate Matter","Ozone (O3)")
  
  output$airPlot <- renderPlot({
    
    # Render a barplot
    barplot(a[,input$Boroughs], 
            names.arg=airname, cex.names = 0.8,
            main=input$Boroughs,
            ylab="Value",
            xlab="Air Q")
  })
  
}


shinyApp(ui = ui, server = server)




