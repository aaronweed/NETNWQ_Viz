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

## bring in data
# df<-read.csv("./Data/NETN_Water_Data_RViz.csv")
# units<-read.csv("./Data/tlu_Units.csv") ## table with units for lableing plots


##### Begin Server Function ####

shinyServer(function(input,output){

###################### Create set of reactive selection boxes in UI  ####################
  ### select site based on park
  output$SiteResults <- renderUI({ 
    
    df_sub<-subset(df, ParkCode %in% input$park & LocationType %in% input$loc)
    df_sub<-droplevels(df_sub)
    
    selectInput(inputId='site', label='Select Site',  unique(levels(df_sub$Description)))
  })
  
  
  output$VarResults <- renderUI({ 
    # dynamically select parm based on location type. This will cause the UI to update 
    
    if(input$loc == "Stream"){
    
    df_sub<-subset(df, LocationType %in% input$loc )
    df_sub<-df_sub[!is.na(df_sub$Value),]
    df_sub<-droplevels(df_sub)
    
    }
    
    if(input$loc == "Lake"){
      
      df_sub<-subset(df, LocationType %in% input$loc )
      df_sub<-df_sub[!is.na(df_sub$Value),]
      df_sub<-droplevels(df_sub)
      
    }
    
  selectInput(inputId='parm', label='Select variable to plot', choices=  unique(levels(df_sub$Local.Characteristic.Name)))
  })
  
  
  
  
#####################################################  
### download data table
    
    output$downloadData <- downloadHandler(
      
      filename = function() { 
        paste(input$site,input$parm, '.csv', sep='') 
      },
      content = function(file) {
        write.csv(data, file)
      }
    )
  
#######################################################   
### time series plot
   
      output$plot <- renderPlot({
        
        data<-subset(df, Description %in% input$site & Local.Characteristic.Name %in% input$parm)
        data$Value<-as.numeric(as.character(data$Value))
        
        data$Visit.Start.Date<-as.Date(data$Visit.Start.Date, format= "%Y-%m-%d") #convert to StartDate
        data$Year<-as.factor(format(data$Visit.Start.Date,"%Y")) #extract Year
         
        if(nrow(data)== 0){
          stop("Sorry, this variable has not been collected at this site.")
        }
        
      parm<-input$parm
     
     if(input$plottype == "Time Series"){
       
       if(data$LocationType == "Stream"){
         
         if(input$logscale ==TRUE){
           
           p <- ggplot(data, aes(x= Visit.Start.Date, y = log(Value)))+ labs(y = paste("log", units$unit[units$parm %in% parm]), x= "Date") + 
             geom_point(colour = "black", size = 2,na.rm=TRUE)
          }else{
           
          p <- ggplot(data, aes(x= Visit.Start.Date, y = Value))+ labs(y = units$unit[units$parm %in% parm], x= "Date") + 
          geom_point(colour = "black", size = 2,na.rm=TRUE)}
     
       }else{
         
        ## plot parm by depth for lakes and ponds
         if(input$logscale ==TRUE){
           
          p <- ggplot(data, aes(Visit.Start.Date, y = log(Value)))+ labs(y = paste("log", units$unit[units$parm %in% parm]), x= "Date", colour= "Depth (m)") + 
             geom_point(aes(colour= DEPTH), size = 1.5,na.rm=TRUE)
         }else{
           
         p <- ggplot(data, aes(Visit.Start.Date, y = Value))+ labs(y = units$unit[units$parm %in% parm], x= "Date", colour= "Depth (m)") + 
           geom_point(aes(colour= DEPTH), size = 1.5,na.rm=TRUE)}
         
       }
     
   
     if(input$trend == TRUE){
       
      fit <- lm(Value ~ Visit.Start.Date, data = data, na.action=na.omit)
     
     p<- (p + scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
             geom_smooth(method= "lm", se= TRUE) +
              theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
              theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
              theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
              theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
              theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
              theme(legend.key = element_rect(fill = "white", color = "black"),legend.key.size = unit(0.5, "in")) +
              theme(panel.background =  element_rect(fill="white", colour="black")) +
              theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(paste(data$ParkCode, "                                  Adj R2 = ",signif(summary(fit)$adj.r.squared, 3),
               " Intercept =",signif(fit$coef[[1]],3 )," Slope =",signif(fit$coef[[2]], 3)," P =",signif(summary(fit)$coef[2,4], 2))))

          }
     
      if(input$trend == FALSE){
        
        
       p<- (p + facet_wrap(~Description)+ scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
              theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
              theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
              theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
              theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
              theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
              theme(legend.key = element_rect(fill = "white", color = "white"),legend.key.size = unit(0.5, "in")) +
              theme(panel.background =  element_rect(fill="white", colour="black")) +
              theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode[1]))
       
       
      }
     
       print(p)
       
   }
     
     if(input$plottype == "Histogram"){
       
       data<-subset(df, Description %in% input$site & Local.Characteristic.Name %in% input$parm)
       data$Value<-as.numeric(as.character(data$Value))
       
       data$Visit.Start.Date<-as.Date(data$Visit.Start.Date, format= "%Y-%m-%d") #convert to StartDate
       data$Year<-as.factor(format(data$Visit.Start.Date,"%Y")) #extract Year
       
       ### add histogram
       
       if(input$logscale ==TRUE){
         
       p2 <- ggplot(data, aes(log(Value), fill= Year))+ labs(x = paste("log", units$unit[units$parm %in% parm]), y= "Frequency", colour= "Year") + 
         geom_histogram() + scale_colour_brewer(palette = "Set1")
       }else{
         
        p2 <- ggplot(data, aes(Value, fill= Year))+ labs(x = units$unit[units$parm %in% parm], y= "Frequency", colour= "Year") + 
          geom_histogram(binwidth = input$binwidth)+ scale_colour_brewer(palette = "Set1")
         
       }
          
       p2<- (p2 +facet_wrap(~Description)+
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
               theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
               theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 12, face ="bold", vjust= 1, debug=F))+
               theme(legend.key = element_rect(fill = "white", color = "white"),legend.key.size = unit(0.5, "in"))+
               theme(legend.text = element_text(size = 12), legend.position = "top", legend.box ="horizontal") + 
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode))
       
       print(p2)
       
       
     }
     
     if(input$plottype == "Box Plot (monthly)"){
       
       data<-subset(df, Description %in% input$site & Local.Characteristic.Name %in% input$parm)
       data$Value<-as.numeric(as.character(data$Value))
       
       data$Visit.Start.Date<-as.Date(data$Visit.Start.Date, format= "%Y-%m-%d") #convert to StartDate
       data$Year<-as.factor(format(data$Visit.Start.Date,"%Y")) #extract Year
       data$month_num<-as.numeric(format(data$Visit.Start.Date,"%m")) #extract month
       data$month<-as.factor(months(as.Date(data$Visit.Start.Date)))
       
         
       #data<-factor(data$month, levels=data$month[order(data$month_num)] ) 
       
       ### add boxplot
       
       if(input$logscale ==TRUE){
         
       p2 <- ggplot(data, aes(x= reorder(month, month_num), y= log(Value)))+ labs(y = paste("log", units$unit[units$parm %in% parm]), x= "Month", colour= "Year") + 
         geom_boxplot(outlier.shape = 1, outlier.colour ="red", outlier.size= 1.5) + geom_point(aes(colour= Year), size =2) +scale_colour_brewer(palette = "Set1")
       }else{
         p2 <- ggplot(data, aes(x= reorder(month, month_num), y= Value))+ labs(y = units$unit[units$parm %in% parm], x= "Month", colour= "Year") + 
           geom_boxplot(outlier.shape = 1, outlier.colour ="red", outlier.size= 1.5) + geom_point(aes(colour= Year), size =2) +scale_colour_brewer(palette = "Set1")
         
       }
       
       
       p2<- (p2 + facet_wrap(~Description)+
               theme(axis.text.y = element_text(color="black", vjust= 0.5,size = 16 * 0.8,face="bold"))+
               theme(axis.text.x = element_text(angle = 0,  vjust=0,size = 16 * 0.8)) +
               theme(strip.text.x= element_text(size=14, face=c("bold.italic"))) +
               theme(axis.title.x =element_text(size = 14, face ="bold", vjust= 1, debug=F))+
               theme(axis.title.y =element_text(size = 14, face ="bold", vjust= 1, debug=F))+
               theme(legend.key = element_rect(fill = "white", color = "white"),legend.key.size = unit(0.5, "in"))+
               theme(legend.text = element_text(size = 12), legend.position = "top", legend.box ="horizontal") + 
               theme(panel.background =  element_rect(fill="white", colour="black")) +
               theme(panel.grid.major = element_line(colour = "grey90"))+ggtitle(data$ParkCode))
       
       print(p2)
       
       
     }
     
     
     
   }
     
     , height = 600, width = 800)
   
   
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



