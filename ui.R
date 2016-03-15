library(dplyr)
dc_po_data <- read.csv("Purchase_order_FY15.csv",header=TRUE)
colnames(dc_po_data) <- c('Agency','Commodity','Supplier','Date','PO','Amount')

Agencies <- distinct(select(dc_po_data,Agency))

#for shiny UI selectInput choices arg.
row.names(Agencies) <- Agencies$Agency

shinyUI(fluidPage(
  titlePanel("DC Purchase Orders 10/2014 - 3/2015"),
  helpText("Data Explorer for Washington D.C. public purchase orders"),
  p(
    class="text-muted",
    paste("data source:http://opendata.dc.gov/datasets"),
    br(),
    paste("author:twitter.com/neilkutty")
  ),
  

  
  fluidRow(
          column(width=10,offset=.25,
      
           helpText(""),
           
           selectInput("datasetchoice", "Choose DC Agency to view PO data",
                       choices =  as.character(row.names(Agencies)),
                       selected = Agencies$Agency[10])
           
           #selectInput("")
          )
    
  ),
  h4(textOutput("text1")),
  plotOutput("plot", height = "600px"),
  dataTableOutput("table")

  
))