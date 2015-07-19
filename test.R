activity.merged <- merge(activity, activity.byinterval.mean)

activity.merged <- transform(activity.merged, steps = ifelse(is.na(steps), round(avg.no.of.steps), steps))
activity.final <- activity.merged[,c(1:3)]

activity.weekend <- filter(activity.final, day.type == "weekend")
activity.weekday <- filter(activity.final, day.type == "weekday")

activity.weekend.byinterval <- group_by(activity.weekend, interval)
activity.weekday.byinterval <- group_by(activity.weekday, interval)

activity.weekend.byinterval.mean <- summarise(activity.weekend.byinterval, avg.no.of.steps = mean(steps, na.rm =TRUE))
activity.weekday.byinterval.mean <- summarise(activity.weekday.byinterval, avg.no.of.steps = mean(steps, na.rm =TRUE))

par(mfrow=c(2,1))

with(activity.weekend.byinterval.mean, plot(interval, avg.no.of.steps, type="l", xlab = "Interval", ylab = "Number of steps", main = "weekend"))
with(activity.weekday.byinterval.mean, plot(interval, avg.no.of.steps, type="l", xlab = "Interval", ylab = "Number of steps", main = "weekday"))