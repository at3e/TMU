library(plotrix)
lim <- c(0,500)
plot(Monthly_PM25.data[,2]*2+60, type = "b", pch = 18, cex = 1, col = 2, xlim = c(0,14),
     ylim = lim, xlab="month", ylab = " " , main = "Monthly trends",yaxt='n')
 
ylabels <- c("0", "2", "4", "", "10", "20", "30", "40", "50", "60", " 70", " ",
             "220", "240", "260", "280", "300", "320", "340", "360", "380", 
             "400", "420", "440", "460", "480")
axis(side=1, at=seq(1, 12, by=1), labels = FALSE)
axis(side=2, at=seq(0, 500, by=20), labels = FALSE)
ytick = c(0,20,40,80,100,120,140,160, 180, 200, 220, 240, 260, 280, 300, 320, 340,
          360, 400, 420, 440, 460, 480, 500)
text(y= seq(0,500, by = 20), par("usr")[1], labels = ylabels, srt = 0, cex = 0.8,
     pos = 2, xpd = TRUE)
axis.break(2,220,  style = "zigzag")
points(Monthly_PM10.data[,2]*2+60, type = "b", pch = 18, col = 3)
points(Monthly_O3.data[,2]*2+60, type = "b", pch = 18, col = 4)
points(Monthly_NO2.data[,2]*2+60, type = "b", pch = 18, cex = 1, col = 6)
points(Monthly_SO2.data[,2]*10, type = "b", pch = 18, col = "mediumpurple1")
axis.break(2,60, style = "zigzag")
points(Monthly_CO.data[,2]*10, type = "b", pch = 18, cex=1, col = "sienna1")
points(Monthlycases.data[,2]/2+100, type = "b", pch = 16, cex=1, col = 'blue')
legend(12, 500, legend=c("PM2.5", "PM10", "O3", "NO2", "SO2",
                       "CO", "Cases"), lty = 1, pch = c(18,18,18,18,18,18,16),
       col = c(2,3,4,6,"mediumpurple1","sienna1","blue"), cex=0.6)