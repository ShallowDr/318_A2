#Read Data
data = read.table("Group_Assignment_2_Dataset.txt",header = T,sep = ",")
data$Date = as.Date(data$Date, format = "%d/%m/%Y")


#Initialize Libraries
library("lubridate")
library("zoo")


#Format Data
startDate = "2008-01-07" #First monday of the year
endDate = "2008-12-28" #Last sundary of the year
#Remove incomplete weeks
data = data[data$Date >= startDate,]
data = data[data$Date <= endDate,]
data$Global_intensity = na.locf(data$Global_intensity) #Replace na values with most recent non na value
data$Week = isoweek(data$Date) - 1 #Assing week number to each entry


#Take moving average for each week
smoothedData = list()
for(i in 1:51){
  smoothedData[i] = list(rollmean(data[data$Week == i,]$Global_intensity, k = 7))
}
averageSmoothedWeek = list(rowMeans(simplify2array(smoothedData))) #Average Week out of all smoothed weeks


#Take average absolute difference between each week and the average week
smoothedDataDifferences = list()
for(i in 1:51){
  smoothedDataDifferences[i] = mean(abs(averageSmoothedWeek[[1]] - smoothedData[[i]]))
}
smoothedDataDifferences = unlist(smoothedDataDifferences)


#Plot Data
t = c(1:length(averageSmoothedWeek))
y1 = smoothedData[[which.max(smoothedDataDifferences)]]
y2 = smoothedData[[which.min(smoothedDataDifferences)]]
y3 = averageSmoothedWeek
plot(x = t, y = y1, type = "l", col = "pink", ylab = "Rolling Mean", xlab = "Time (mins)", main = "Global_intensity Rolling Average vs Time")
lines(t, y2, col = "blue")
lines(t, y3, col = "black")
legend("topleft", legend=c("Max Difference from average", "Min Difference from average", "Average"), fill = c("pink", "blue", "black"))
