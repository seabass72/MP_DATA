# load csv

data <- read.csv("timberlake.csv", header = TRUE)

# eliminate crazy outliers of 9999.00 

data1 <- (data[data == -9999.00] <- NA)

#remove rows with NA

data2<-data[!is.na(data$DON), ]

#add new column

data2$Restoration <- NA

#change data format 

date_F<-as.Date(data2$Date, "%m/%d/%y")

date_N<- format(date_F,"%m%d%Y")

#add new row to data 

data2$Date <- date_N


