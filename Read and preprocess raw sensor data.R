#@author: Hoda Allahbakhshi
#@references: Allahbakhshi, H., Conrow, L., Naimi, B., & Weibel, R. (2020). Using accelerometer and GPS data for real-life physical activity type detection. Sensors, 20(3), 588.
library(tidyr)
library(dplyr)


studyRawDataFolder <- 'S:/course/geo885/stud/G2/test'
options(scipen = 6)
options(digits.secs = 6)
devices <- c("knee","Lhip-front","Rhip-side" )


# Read and preprocess accelerometer data
files_imu <- list.files(studyRawDataFolder,recursive = TRUE, pattern = "^IMU",full.names = TRUE)
imul <- lapply(files_imu,function(i){
  read.csv(i,header = TRUE,stringsAsFactors = FALSE, sep = ",", strip.white = TRUE )
})

# convert data and time columns to DateTime
for (i in seq_along(imul)) {
  # make the date and time column
  imul[[i]]$DateTime <- strptime(paste(imul[[i]]$Date, imul[[i]]$Time), "%d/%m/%Y %H:%M:%OS" , tz = 'GMT')
  # convert the datetime format from character to datetime
  imul[[i]]$DateTime <- as.POSIXct(imul[[i]]$DateTime, format = "%H:%M:%OS")
  imul[[i]]$DateTime <- imul[[i]]$DateTime + 0.0005
  op <- options(digits.secs = 4)}

# remove the rows containing NA values in imul
for (i in seq_along(imul)){
  imul[[i]] <- drop_na(imul[[i]])
}

# Read and preprocess GPS data
files_gps <- list.files(studyRawDataFolder,recursive = TRUE, pattern = "^GPS",full.names = TRUE)
gpsl <- lapply(files_gps,function(i){
  read.csv(i,header = TRUE,stringsAsFactors = FALSE, sep = ",", strip.white = TRUE )
})

# convert data and time columns to DateTime
for (i in seq_along(gpsl)){
  gpsl[[i]]$DateTime <- as.POSIXct(gpsl[[i]]$DateTime,format = '%Y-%m-%d %H:%M:%S', tz ='GMT')
}

# remove the na in activity label in gpsl
gps_na <- c(100, 110, 120)

for (i in gps_na){
  gpsl[[i]] <- gpsl[[i]][-243,]
}



# # make a new structure of imul
# imul_new <- list(list(list(), list(), list()), list(list(), list(), list()), list(list(), list(), list()), list(list(), list(), list()))
# # make a new structure of gpsl
# gpsl_new <- list(list(list(), list(), list()), list(list(), list(), list()), list(list(), list(), list()), list(list(), list(), list()))
# 
# p_num <- 1
# 
# for (i in seq(4)) {
#   for (j in seq(3)) {
#     for (p in seq(p_num, (p_num+9))) {
#       imul_new[[i]][[j]] <- append(imul_new[[i]][[j]], list(imul[[p]]))
#       gpsl_new[[i]][[j]] <- append(gpsl_new[[i]][[j]], list(gpsl[[p]]))
#     }
#     p_num = p_num + 10
#   }
# }






# # count the number of na in column activitylabel in imul
# imu_na_count = 0
# for (i in seq(length(imul))){
#   imu_na_count = imu_na_count + sum(is.na(imul[[i]][13]))
#   print(which(is.na(imul[[i]][13])))
# }
# print(imu_na_count)
# 
# 
# count the number of na in column activitylabel in gpsl
# gps_na_count = 0
# 
# for (i in seq(length(gpsl))){
#   gps_na_count = gps_na_count + sum(is.na(gpsl[[i]][13]))
#   print(which((is.na(gpsl[[i]][13]))))
#   print(i)
# }
# print(gps_na_count)

# gps <- gpsl[[101]]
# gps <- data.frame()
# which(is.na(gpsl))
# for(i in seq(1, 120)){
#   gps <- rbind(gps, gpsl[[i]])
# }
# nrow(gpsl[[101]])
# timedif_count = 0
# for(i in seq(length(gps))){
#   if(difftime(gps$DateTime[i+1], gps$DateTime[i], units = "secs") > 1){
#     timedif_count = timedif_count + 1
#   }
# }
# print(timedif_count)

# timedif_count = 0
# for (i in seq(length(gpsl))){
#   for(j in nrow(gpsl[[i]][[12]])){
#     if(difftime(gpsl[[i]][[12]][j+1], gpsl[[i]][[12]][j], units = "secs")>1){
#       timedif_count = timedif_count + 1
#     }
#   }
# }
# print(timedif_count)
# df_real_older <- data.frame()
# 
# df_real_younger <- data.frame()
# 
# df_semi_older <- data.frame()
# 
# df_semi_younger <- data.frame()
# 
# for (i in seq(1,30)){
#   df_real_older <- rbind(df_real_older, imul[[i]])
# }
# for (i in seq(31,60)){
#   df_real_younger <- rbind(df_real_younger, imul[[i]])
# }
# for (i in seq(61,90)){
#   df_semi_older <- rbind(df_semi_older, imul[[i]])
# }
# for (i in seq(91,120)){
#   df_semi_younger <- rbind(df_semi_younger, imul[[i]])
# }



# Read and preprocess accelerometer data
files <- list.files(studyRawDataFolder,recursive = TRUE, pattern = "IMU.csv",full.names = TRUE)
options(scipen = 6)
options(digits.secs = 6)
imul <- lapply(files,function(i){
  read.csv(i,header = TRUE,stringsAsFactors = FALSE, sep = ",", strip.white = TRUE )
})
devices <- c("knee","Lhip-front","Rhip-side" )
names(imul) <- devices
colnames1  <- c("Date","Time","Acc_X","Acc_Y","Acc_Z","Mag_X","Mag_Y","Mag_Z", "DateTime", "SVM", "activitylabel")
for (i in seq_along(imul)) {
  colnames(imul[[i]]) <- colnames1
}
for (i in seq_along(imul)) {
  for (j in 3:8) {
    imul[[i]][[j]] <- as.numeric(imul[[i]][[j]])}}
class(imul[[i]][[j]])
# convert data and time columns to DateTime
for (i in seq_along(imul)) {
  imul[[i]]$Time <- sub("(.*):", "\\1.", imul[[i]]$Time) 
  imul[[i]]$Time <- trimws(imul[[i]]$Time)
  # make the date and time column
  imul[[i]]$DateTime <- strptime(paste(imul[[i]]$Date, imul[[i]]$Time), "%d/%m/%Y %H:%M:%OS" , tz = 'GMT')
  # convert the datetime format from character to datetime
  imul[[i]]$DateTime <- as.POSIXct(imul[[i]]$DateTime, format = "%H:%M:%OS")
  imul[[i]]$DateTime <- imul[[i]]$DateTime + 0.0005
  op <- options(digits.secs = 3)}
# convert accelerometer values from mgal to m/s2
formatData <- function(x){
  x <- cbind(c(1:nrow(x)), x)
  colnames(x) <- c("list","Date","Time","Acc_X","Acc_Y","Acc_Z","Mag_X","Mag_Y","Mag_Z","DateTime")  #x$time <- x$time/1000
  #x$time <- as.POSIXct(x$time, origin = '1970-01-01')
  x$Acc_X <- (x$Acc_X/1000)*9.81
  x$Acc_Y <- (x$Acc_Y/1000)*9.81
  x$Acc_Z <- (x$Acc_Z/1000)*9.81
  # calculate total acceleration
  SVM <- (x$Acc_Y*x$Acc_Y) + (x$Acc_X*x$Acc_X) + (x$Acc_Z*x$Acc_Z)
  SVM <- sqrt(SVM)
  x <- cbind(x, SVM)
}
for (i in seq_along(imul)) {
  imul[[i]] <- formatData(imul[[i]])
  imul[[i]] = imul[[i]][!duplicated(imul[[i]]$DateTime),]}

# # Read and preprocess GPS data
files <- list.files(studyRawDataFolder,recursive=TRUE, pattern="Location.csv",full.names = TRUE)
options(scipen=6)
options(digits.secs=6)
dlocl<- lapply(files,function(i){
  read.csv(i,header = TRUE,stringsAsFactors = FALSE, sep =",", strip.white=TRUE )
})
names(dlocl) <- devices
colnames2  <- c("Date","Time","Longitude","Latitude","Satellites","Altitude","VDOP","HDOP","Recording","Speed")
for (i in seq_along(dlocl)){
  colnames(dlocl[[i]]) <- colnames2
}
# removing Na values and rows that are not recording
NullLocIndex <- as.list(seq_along(dlocl))
names(NullLocIndex) <- devices

for (i in seq_along(dlocl)){
  w <- which(dlocl[[i]]$Longitude %in% c("Charging")) # which rows/records has "\t\t\t\t\tNA" or  "\t\t\t\tNA" in the latitude column?
  if (length(w) != 0){
    dlocl[[i]] <- dlocl[[i]][-w,]
  }
  NullLocIndex[[i]] <- which(is.na(dlocl[[i]]$Latitude))
  if (length(NullLocIndex[[i]]) != 0){
    dlocl[[i]] <- dlocl[[i]][-NullLocIndex[[i]],]
  }
}
# convert data and time columns to DateTime
for (i in seq_along(dlocl)){
  
  dlocl[[i]]$Date <- as.Date(dlocl[[i]]$Date,format = '%d/%m/%Y')
  dlocl[[i]]$DateTime <- paste(dlocl[[i]]$Date,dlocl[[i]]$Time) # add a new colunm containing both data and time!
  dlocl[[i]]$DateTime <- as.POSIXct(dlocl[[i]]$DateTime,format = '%Y-%m-%d \t%H:%M:%S', tz ='GMT')
}

for (i in seq_along(dlocl)) {
  for (j in 3:10) {
    dlocl[[i]][[j]] <- as.numeric(dlocl[[i]][[j]])}}
for (i in seq_along(dlocl)){
  dlocl[[i]] = dlocl[[i]][!duplicated(dlocl[[i]]$DateTime),]}



