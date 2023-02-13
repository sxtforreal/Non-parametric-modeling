co2s = read.table('/Users/sunxiaotan/Desktop/STA442 HW3/daily_flask_co2_mlo.csv', header = FALSE, sep = ",",
  skip = 69, stringsAsFactors = FALSE, col.names = c("day",
    "time", "junk1", "junk2", "Nflasks", "quality",
    "co2"))
co2s$date = strptime(paste(co2s$day, co2s$time), format = "%Y-%m-%d %H:%M",tz = "UTC")
co2s = co2s[co2s$quality == 0, ]
plot(co2s$date, co2s$co2, log = "y", cex = 0.3, col = "#00000040",xlab = "time", ylab = "ppm")
plot(co2s[co2s$date > ISOdate(2015, 3, 1, tz = "UTC"),c("date", "co2")], log = "y", type = "o", xlab = "time",ylab = "ppm", cex = 0.5)

co2s$day = as.Date(co2s$date)
toAdd = data.frame(day = seq(max(co2s$day) + 3, as.Date("2025/1/1"),by = "10 days"), co2 = NA)
co2ext = rbind(co2s[, colnames(toAdd)], toAdd)
timeOrigin = as.Date("2000/1/1")
co2ext$timeInla = round(as.numeric(co2ext$day - timeOrigin)/365.25,2)
co2ext$cos12 = cos(2 * pi * co2ext$timeInla)
co2ext$sin12 = sin(2 * pi * co2ext$timeInla)
co2ext$cos6 = cos(2 * 2 * pi * co2ext$timeInla)
co2ext$sin6 = sin(2 * 2 * pi * co2ext$timeInla)

library('INLA', verbose=FALSE)
mm = get("inla.models", INLA:::inla.get.inlaEnv()) 
if(class(mm) == 'function') mm = mm() 
mm$latent$rw2$min.diff = NULL
assign("inla.models", mm, INLA:::inla.get.inlaEnv())

co2res = inla(co2 ~ sin12 + cos12 + sin6 + cos6 +f(timeInla, model = 'rw2',prior='pc.prec', param = c(0.1, 0.5)),
data = co2ext, family='gamma',control.family = list(hyper=list(prec=list(prior='pc.prec', param=c(0.1, 0.5)))), control.inla = list(strategy='gaussian'),control.predictor = list(compute=TRUE, link=1),control.compute = list(config=TRUE),verbose=FALSE)
qCols = c('0.5quant','0.025quant','0.975quant')
Pmisc::priorPost(co2res)$summary[,qCols]
