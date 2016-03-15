library(shiny)
library(shinyBS)
library(ggplot2)
library(scales)
library(zoo)
library(lubridate)
library(RColorBrewer)
dc_po_data <- read.csv("Purchase_order_FY15.csv",header=TRUE)
colnames(dc_po_data) <- c('Agency','Commodity','Supplier','Date','PO','Amount')
###>>> Basic Transformations
dc_po_data$Amount <- as.numeric(dc_po_data$Amount)
dc_po_data$Date <- as.Date(as.character(dc_po_data$Date), "%d-%b-%y")
dc_po_data$MonthYear <- paste(year(dc_po_data$Date),'-',month(dc_po_data$Date),sep='')
fil_set <- dc_po_data

shinyServer(function(input, output) {
  output$plot <- renderPlot({
     fil_set <- filter(fil_set,Agency==input$datasetchoice)
    
    
    ggplot(fil_set,aes(x=Date,y=Amount,size=Amount,color=Amount))+
      geom_point()+
      geom_smooth()+
      scale_colour_gradient(low = "blue", high = "orange") +
      theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position=c(.78,.25))+
      scale_x_date(date_breaks='1 month')+
      #coord_trans(y="log10")+
      scale_y_continuous(labels=dollar)+
      theme(axis.text.x = element_text(angle=90,hjust=1))
  })
  
  output$table <- renderDataTable({
    filter(fil_set,Agency==input$datasetchoice)
  })
  output$text1 <- renderText({ 
    paste("Selected Agency:", input$datasetchoice)
  })
})