library(data.table)
library(lubridate)
fsum <- function(x) {sum(x,na.rm=TRUE)}

setwd("D:\\Personal\\Health\\samsunghealth_rferrisx_202102221845")
steps <- fread("com.samsung.shealth.tracker.pedometer_day_summary.202102221845.csv")
n1 <- names(steps)[2:20]
steps <- setnames(steps[,1:19],n1)[]
# distance measured in meters; convert to miles with 1mi = 1609.34 meters
steps <- steps[,.(distance=max(distance),
miles=max(round(distance/1609.34,2)),
step_count=max(step_count),
calorie=(max(round(calorie))),
create_date=date(ymd_hms(create_time[1])),
last_create_time=ymd_hms(create_time[length(create_time)]),
last_update_time=ymd_hms(create_time[length(update_time)])),
keyby=(IDateTime(.POSIXct(day_time/1000,tz = "UTC")))]

# steps and miles
steps_months <- steps[,.(miles=fsum(miles)),keyby=.(year(ymd(idate)),months(ymd(idate)),month_n=month(ymd(idate)))][order(year,month_n)]
steps_count <- steps[,.(step_count=fsum(step_count)),keyby=.(year(ymd(idate)),months(ymd(idate)),month_n=month(ymd(idate)))][order(year,month_n)]
steps[,.(miles=fsum(miles)),keyby=.(year(ymd(idate)),months(ymd(idate)),month_n=month(ymd(idate)))][order(year,month_n)][,fsum(miles)]
steps[,.(step_count=fsum(step_count)),keyby=.(year(ymd(idate)),months(ymd(idate)),month_n=month(ymd(idate)))][order(year,month_n)][,fsum(step_count)]
steps_months[,plot(miles,type="b",pch=19,lwd=2)]

step.summary <- cbind(steps_months[10:21],steps_count[10:21,4])
step.summary[,.(mean.miles=mean(miles),mean.steps=mean(step_count))]
step.summary[,.(sum.miles=fsum(miles),sum.steps=fsum(step_count))]

# floors
floors <- fread("com.samsung.health.floors_climbed.202102221845.csv")
n1 <- names(floors)[2:11]
floors <- setnames(floors[,1:10],n1)[]
floors <- floors[,.(floor_count=fsum(floor)),by=.(date=date(ymd_hms(create_time)))][order(-date)]
floors_months <- floors[,.(floor_count=fsum(floor_count)),keyby=.(year(ymd(date)),months(ymd(date)),month_n=month(ymd(date)))][order(year,month_n)]
floors_months <- floors_months[,.SD[,.(elevation.feet=floor_count * 10)],.(year,months,month_n,floor_count)]
merge(step.summary,floors_months,by=c("year","month_n","months"))

