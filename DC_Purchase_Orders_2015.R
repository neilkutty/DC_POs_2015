#
#
####

library(ggplot2)
library(scales)
library(dplyr)
library(RColorBrewer)


#DC Purchase Order 2015 Data
dc_po_data <- read.csv("Purchase_order_FY15.csv",header=TRUE)
colnames(dc_po_data) <- c('Agency','Commodity','Supplier','Date','PO','Amount')
###>>> Basic Transformations
dc_po_data$Amount <- as.numeric(dc_po_data$Amount)
dc_po_data$Date <- as.Date(as.character(dc_po_data$Date), "%d-%b-%y")
dc_po_data$MonthYear <- paste(year(dc_po_data$Date),'-',month(dc_po_data$Date),sep='')


### --- Aggregate() to sum(PO.amount) by Date
bydate_agg_data <- filter(aggregate(cbind(Amount)~Date+Agency,dc_po_data,sum),Amount>250000)

### --- Currency formatting for Amount column
bydate_agg_data$DollarsSpent <- paste("$",format(bydate_agg_data$Amount, big.mark=","),sep="") 

### --- Quick plot of 
qplot(bydate_agg_data$Date,bydate_agg_data$Amount) +
  aes(group=bydate_agg_data$Date, color = bydate_agg_data$Agency) +
  geom_point(stat="identity") + 
  ggtitle("'Big Spenders' - Agencies with > $250,000 Individual POs")+
  #geom_text(aes(label=bydate_agg_data$Amount), vjust=0) +
  #scale_color_hue()+
  scale_x_date(date_breaks = '1 week', limits = c(as.Date('2014-11-01'), NA))+
  scale_y_log10(labels=dollar) + theme(legend.position="right") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



#--------------<>------D---P---L---Y---R-------<>------<>------<>------<>------<>
#dplyr fxs
#Unique fields
Agencies <- distinct(select(dc_po_data,Agency))

#for shiny UI selectInput choices arg.
row.names(Agencies) <- Agencies$Agency

#distinct Supplier names
Supplier <- distinct(select(dc_po_data,Supplier))

#dplyr summarisations ###
by_agency <- group_by(dc_po_data,Agency)
purchases_by_agency <- arrange(summarise(by_agency,
                                 count_suppliers = n_distinct(Supplier),
                                 count=n(),
                                 amount=sum(Amount),
                                 avg_po_amt=amount/count,
                                 avg_inv_per_supplier=count/count_suppliers), desc(amount))

#Filter purchases_by_agency to include only agencies who have spent more than $1,000,000 in the year
##
###
###% FILTERED DATASET & PLOTS
###%
###
##
#

#dplyr filter purchases_by_agency for top agencies
top_agencies <- arrange(filter(purchases_by_agency,amount>100000),desc(amount))

#order top_agencies for ggplot2
top_agencies$Agency <- factor(top_agencies$Agency,
                             levels=top_agencies$Agency[order(top_agencies$amount)])

##  ** CAN't Execute the below and still use as aesthetics **
#>>>  #Convert columns with dollar amounts to currency format
#top_agencies$amount <- paste("$",format(top_agencies$amount, big.mark=","),sep="")
#top_agencies$avg_po_amt <- paste("$",format(top_agencies$avg_po_amt, big.mark=","),sep="")


#  PLOT OF TOP_AGENCIES

top_agency_plot <- ggplot(top_agencies,aes(x=amount,y=Agency,size=avg_inv_per_supplier,color=Agency,frame=MonthYear))+
    geom_point()+
    #scale_color_continuous()+
    #geom_text(aes(hjust="inward",label=paste("$",top_agencies$amount)))+
    scale_x_log10(labels=dollar)+
      ggtitle("DC Public Purchase Orders \n Agencies with >$100K/Mo. \n Spending Y-M:") + 
    theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position="none")+
    labs(size="Avg. Purchases per Supplier", colour="Avg. PO Amount")

#gg_animate
gg_animate(top_agency_plot,"output.gif")

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  googleVis for Shiny implementation            %%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

agency_barchart <- gvisBarChart(top_agencies, xvar="Agency",yvar="amount",
                                options = list(height=1000, witdh=3000,
                                               chartArea="{left:250,top:50,width:\"50%\",height:\"75%\"}",
                                               fontName = "Arial",
                                               fontSize = 12
                                               )
                                )
plot(agency_barchart)


#-----<><><><><><>-----<><><><><><>-----<><><><><><>-----<><><><><><>-----<><><><><><>
bubble1 <- gvisBubbleChart(top_agencies, 
                           idvar="Agency", 
                           xvar="amount", 
                           yvar="count_suppliers", 
                           sizevar ="amount",
                           colorvar = "avg_po_amt",
                           options = list(height=1000,
                                          fontName = "Arial",
                                          fontSize = 10,
                                          hAxis='{minValue:250000,maxValue:60000000}',
                                          vAxis='{minValue:-100,maxValue:1000}',
                                          vAxis.logScale='true',
                                          explorer = list(),
                                          tooltip = list(
                                          textStyle = list(
                                             fontSize = 10
                                            )
                                           )
                                         )
                           )
plot(bubble1)


new_dataset <- dc_po_data  
plot(gvisTable(top_agencies))
#%%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%%
#%%%%%%%%%%%%%%%%%


#######%
######%
#####%
####%
###%
###
##
#


#----------------------------------------------------------------------------------------------------------<><><><><><><><><><><><>---#


######<><><><><><><><><><><><><>
by_type <- group_by(dc_po_data,Commodity)
purchases_by_type <- summarise(by_type,
                               count_suppliers = n_distinct(Supplier),
                               count=n(),
                               amount=sum(Amount),
                               average_po_amt=amount/count,
                               pos_per_supplier=count/count_suppliers)



qplot(top_agencies$Agency,top_agencies$amount)

plot(top_agency_plot)
#------<>------<>------D---P---L---Y---R-------<>------<>------<>------<>------<>

#plot of full data set individual points
fullplot <- ggplot(dc_po_data,aes(x=Date,y=Amount,color=Agency))+
  geom_point()+
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position=c(.78,.25))+
  scale_x_date(date_breaks='1 month')+
  #coord_trans(y="log10")+
  scale_y_log10(labels=dollar)+
  theme(axis.text.x = element_text(angle=90,hjust=1))
plot(fullplot)

#plot of purchases by agency ##modified for gg_animate
###
#
agencyplot <- ggplot(purchases_by_agency,aes(x=count,y=amount,size=count_suppliers,color=amount,frame=Agency))+
  geom_point()+ 
  ggtitle("DC Public Purchase Orders \n 10-2014 - 3-2015 \n Agency: ")+
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position=c(.78,.35))+
  coord_trans(x = "log10")+coord_trans(y="log10")+
  scale_y_log10(labels=dollar)+scale_x_log10()+
  xlab("Number of Transactions")
  theme(axis.text.x = element_text(angle=90,hjust=1))
plot(agencyplot)

gg_animate(agencyplot,"PO_animation2.gif",interval=1.3)

#purchases by commodity
commplot <- ggplot(purchases_by_type,aes(x=count_suppliers,y=amount,size=amount,color=average_po_amt))+
  geom_point()+
  theme(plot.title = element_text(lineheight=.8, face="bold"),legend.position=c(.78,.25))+
  #coord_trans(x = "log10")+coord_trans(y="log10")+
  scale_y_log10(labels=dollar)+scale_x_log10()+
  theme(axis.text.x = element_text(angle=90,hjust=1))
plot(commplot)



###DPLYR FOR SHINY IMPLEMENTATION
filtered_set <- filter(dc_po_data,Agency==Agency)
filtered_set$Agency <- factor(filtered_set$Agency,levels=filtered_set$Agency[order(filtered_set$Amount)])

fil_set <- dc_po_data
new_fil_set <- fil_set
#### ggplot + coord_trans() frequent no plot
######## Error resolved >>> make sure limits=c(minval exists,NA)
#  scale_x_date(limits = c(as.Date('2015-10-01'), NA))+
#  scale_y_continuous(labels = dollar) + theme(legend.position="none")
#Error in matrix(value, n, p) : 
#  'data' must be of a vector type, was 'NULL'

######
#
#



#filter() (and slice())
#_______________________________________
#
#  filter(flights, month == 1, day == 1)

#arrange()
#
#   arrange(flights, desc(arr_delay))

#select() (and rename())
#
#   Select all columns except those from year to day (inclusive)
#   select(flights, -(year:day))
#
#distinct()
#
#
#
#mutate() (and transmute())
#
#
#
#summarise()
#
#
#
#sample_n() and sample_frac()


###--------Example of geom_text adjustments -----------###
#geom_text(aes(x=filename, y=value, ymax=value, label=value, 
#hjust=ifelse(sign(value)>0, 1, 0)), 
#position = position_dodge(width=1))
###----------------------------------------------------###
#
#
############################################################
