# load csv

data <- read.csv("timberlake.csv", header = TRUE)

# eliminate crazy outliers of 9999.00 

data1 <- (data[data == -9999.00] <- NA)

#remove rows with NA

data2<-data[!is.na(data$DON), ]

data3 <- data2[!is.na(data2$SRP), ]


#change data format 

date_F<-as.Date(data3$Date, "%m/%d/%y")

date_N<- format(date_F,"%Y%m%d")

date_M <-format(date_F,"%m")

#add new row to data 

data3$Date <- date_N

data3$Month <- date_M

# subset data for inflow and outflow 

inflow <- subset.data.frame(data3,data3$Site == "Inflow",)
outflow <- subset.data.frame(data3,data3$Site == "Outflow",)
pump_Station <- subset.data.frame(data3,data3$Site == "Pump_station",)

# dates for inflow and outflow 

inflow_D <- inflow$Date

outflow_D <- outflow$Date

pump_Station_D <-pump_Station$Date

#merge the inflow and outflow and match the dates 

merge_IFL_OFL <- merge(inflow ,pump_Station, by="Date",all=FALSE,sort=FALSE)

Merge_INOUTPUMP <- merge(merge_IFL_OFL, outflow, by="Date",all=FALSE,sort=FALSE)
# create a % removal of total dissolved nitrogen 

Merge_INOUTPUMP$TN_DIFF <- (((Merge_INOUTPUMP$TDN.x +Merge_INOUTPUMP$TDN.y) - Merge_INOUTPUMP$TDN)/(Merge_INOUTPUMP$TDN.x+Merge_INOUTPUMP$TDN.y)*100)

# create a % removal of SRP

Merge_INOUTPUMP$SRP_DIFF <- (((Merge_INOUTPUMP$SRP.x +Merge_INOUTPUMP$SRP.y) - Merge_INOUTPUMP$SRP)/(Merge_INOUTPUMP$SRP.x+Merge_INOUTPUMP$SRP.y)*100)

# add previous landcover type 

Merge_INOUTPUMP$prev_lnd_cvr <- "AGR"

# add size to data 

Merge_INOUTPUMP$Size <- 4400000

#pull out date of final data stuff 

date_loop <- Merge_INOUTPUMP$Date

# for loop / creating restoration year progression
restore = c();
r = c();
for (x in date_loop) {
  if (x <= 20070801) {
    r <- 1
    }
  else if (x <= 20090801) {
    r <- 2
    }
  else if (x <= 20110801) {
    r <- 3
    }
  else  {
    r <- 4
  }
restore[x] <- r
}

# time of restoration inserted into dataframe 

Merge_INOUTPUMP$Restoration_Age <- restore
  
# for loop to create a categorical category for precipitation 

# 1. pull out the precipitation data 

pre_x <- Merge_INOUTPUMP$Precipitation.x

#create the four loop

p <- c(); #create an empty vector or eventually precip

z <- c();




for (i in pre_x) {
  if (i <= 1) {
    p <- 1
  }
  else if (x <= 2) {
    p <- 2
  }
  else if (x <= 5) {
    p <- 5
  }
  else if (x <= 10) {
    p <- 10
  }
  else if (x <= 20) {
    p <- 20
  }
  else {
    p <-50
  }
  
z[i] <- p
}



#create new dataframe with the rows that we need 
timb_data_remove <- Merge_INOUTPUMP

# remove unnessary rows 

timb_data_remove$Level.x <-NULL
timb_data_remove$Level.y <-NULL
timb_data_remove$Level <-NULL
timb_data_remove$NH4.x <-NULL
timb_data_remove$NH4.y <-NULL
timb_data_remove$NH4 <-NULL
timb_data_remove$NO3.x <-NULL
timb_data_remove$NO3.y <-NULL
timb_data_remove$NO3 <-NULL
imb_data_remove$NO3 <-NULL
timb_data_remove$DON.x <-NULL
timb_data_remove$DON.y <-NULL
timb_data_remove$DON <-NULL
timb_data_remove$Cl.x <-NULL
timb_data_remove$Cl.y <-NULL
timb_data_remove$Cl <-NULL
timb_data_remove$Site.x <-NULL
timb_data_remove$Site.y <-NULL
timb_data_remove$SO4.x <-NULL
timb_data_remove$SO4.y <-NULL
timb_data_remove$SO4 <-NULL
timb_data_remove$Precipitation.y <- NULL
timb_data_remove$Precipitation.x <- NULL
timb_data_remove$DOC.x <-NULL
timb_data_remove$DOC.y <-NULL
timb_data_remove$DOC <-NULL
timb_data_remove$Restoration.x <-NULL
timb_data_remove$Restoration.y <-NULL
timb_data_remove$Restoration <-NULL
timb_data_remove$Month.y <-NULL
timb_data_remove$Month <-NULL


Merge_INOUTPUMP$storm<-ifelse(Merge_INOUTPUMP$Precipitation.x > 0,1,0)



merge_IFL_OFL$Precipitation.x > 0