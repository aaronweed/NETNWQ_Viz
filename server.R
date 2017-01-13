#The server.R script contains the instructions that your computer needs to build your app

library(shiny)
library(leaflet)
library(reshape)
library(rgdal)
library(shinyjs)
library(jsonlite,pos=100)
library(httr)
library(dplyr)
library(DT)
library(ggplot2)
library(RColorBrewer)

df<-read.csv("./Data/NETN_Water_Data_RViz.csv")
units<-read.csv("./Data/tlu_Units.csv") ## table with units for lableing plots

##### Begin Server Function ####

shinyServer(function(input,output){
  
### time series plot
   output$plot <- renderPlot({
    
     data<-subset(df, Description %in% input$x & Local.Characteristic.Name %in% input$z)
     data$Value<-as.numeric(as.character(data$Value))
     
     data$Visit.Start.Date<-as.Date(data$Visit.Start.Date, format= "%Y-%m-%d") #convert to StartDate
     data$Year<-as.factor(format(data$Visit.Start.Date,"%Y")) #extract Year
     
     parm<-input$z
     
     if(input$AA == "No"){
       
       if(data$LocationType == "Stream"){
       
          p <- ggplot(data, aes(x= Visit.Start.Date, y = Value))+ labs(y = units$unit[units$parm %in% parm], x= "Date") + 
          geom_point(colour = "black", size = 2,na.rm=TRUE)
     
       }else{
         
        ## plot parm by depth for lakes and ponds
         
         p <- ggplot(data, aes(Visit.Start.Date, y = Value))+ labs(y = units$unit[units$parm %in% parm], x= "Date", colour= "Depth (m)") + 
           geom_point(aes(colour= DEPTH), size = 1.5,na.rm=TRUE) 
         
       }
     
   
     if(input$y == "Linear"){
       
      fit <- lm(Value ~ Visit.Start.Date, data = data, na.action=na.omit)
     
     p<- (p + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
             geom_smooth(method= "lm", se= TRUE) +
              theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
              theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
              theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
              theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
              theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
              theme(legend.key = element_rect(fill = "white", color = "black"),legend.key.size = unit(0.5, "in")) +
              theme(panel.background =  element_rect(fill="white", colour="black")) +
              theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(paste(data$ParkCode, "                                  Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
               " Intercept =",signif(fit$coef[[1]],3 )," Slope =",signif(fit$coef[[2]], 3)," P =",signif(summary(fit)$coef[2,4], 2))))

          }
     
      if(input$y == "No"){
        
        if(data$LocationType == "Stream"){
          
          p <- ggplot(data, aes(Visit.Start.Date, y = Value))+ labs(y = units$unit[units$parm %in% parm], x= "Date") + 
            geom_point(colour = "black", size = 2,na.rm=TRUE)
          
        }else{
          
          ## plot parm by depth for lakes and ponds
          p <- ggplot(data, aes(Visit.Start.Date, y = Value))+ labs(y = units$unit[units$parm %in% parm], x= "Date", colour= "Depth (m)") + 
            geom_point(aes(colour= DEPTH), size = 1.5,na.rm=TRUE)
          
        }
      
       p<- (p + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
              theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
              theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
              theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
              theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
              theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
              theme(legend.key = element_rect(fill = "white", color = "black"),legend.key.size = unit(0.5, "in")) +
              theme(panel.background =  element_rect(fill="white", colour="black")) +
              theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
       
       
      }
     print(p)
   }
     
     if(input$AA == "Histogram"){
       
       data<-subset(df, Description %in% input$x & Local.Characteristic.Name %in% input$z)
       data$Value<-as.numeric(as.character(data$Value))
       
       data$Visit.Start.Date<-as.Date(data$Visit.Start.Date, format= "%Y-%m-%d") #convert to StartDate
       data$Year<-as.factor(format(data$Visit.Start.Date,"%Y")) #extract Year
       
       ### add histogram
       p2 <- ggplot(data, aes(Value, fill= Year))+ labs(x = units$unit[units$parm %in% parm], y= "Frequency") + 
         geom_histogram()

       
       p2<- (p2 +
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
               theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
               theme(strip.text.x= element_text(size=12, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(legend.key = element_rect(fill = "white", color = "black")) +
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode))
       
       print(p2)
       
       
     }
     
     
     
     
   }
     
     , height = 600, width = 1000)
   
   
   ### add in interactive graph
   ### ERROR: xts doesn't like multiple observations pertime point. Will need to either aggregate data prior to plotting
   # create an mts with vectors corresponding to each rep (MIMA) or depth (lakes)
   
   # output$dygraph <- renderDygraph({
   #   
   #   data<-subset(df, Description %in% input$x & Local.Characteristic.Name %in% input$z)
   #   data<-droplevels(data)
   #   data$Value<-as.numeric(as.character(data$Value))
   #   
   #   parm<-input$z
   #   
   #   data.raw<-data[,c("Visit.Start.Date","Value")]
   #   time.raw <- as.POSIXct(data$Visit.Start.Date, format = "%m/%d/%Y")
   #   series.raw<-xts(data.raw, order.by= time.raw)
   #  #plot(series.raw)
   #   #str(series.raw)
   #   
   #   ##### Create dynamic plot
   #     y<- dygraph(series.raw$Value, main = site, xlab= "Date", ylab= units$unit[units$parm %in% parm])%>%
   #     dyRangeSelector()%>%
   #     dyOptions(drawPoints = TRUE,  pointSize = 2, strokeWidth=0)%>%
   #     #dySeries(parm, label = units$unit[units$parm %in% parm])%>%
   #     dyLegend(show = "always", hideOnMouseOut = FALSE)
   #   print(y)
   #   
   # })
   

}) ## end shiny serverfunc    



