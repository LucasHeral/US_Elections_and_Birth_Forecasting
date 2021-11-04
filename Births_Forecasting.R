
################### SECOND PART OF THE PROJECT ###################
# US births TS Analysis:

usbirths.df <- read_csv("~/Downloads/UCL/YEAR II/Term 2/Data Analytics
II/Week III/US_Births.csv")

usbirths.ts <- ts(usbirths$`Live Births`,
                  start = c(2007, 1),
                  end = c(2012, 6),
                  freq = 12)
library(ggplot2)

# Plots:
autoplot(usbirths.ts, ylab = "US Births")
ggseasonplot(usbirths.ts)

# AAN:
rmse.ets <- function (etsmodel) cat("RMSE = ", sqrt(etsmodel$mse))
rmse.ets(usbirths.ets.AAN)
(usbirths.ets.AAN <- ets(usbirths.ts, model = "AAN"))
rmse.ets(usbirths.ets.AAN)
plot(usbirths.ets.AAN)
usbirths.ets.AAN.pred <- forecast(usbirths.ets.AAN, h = 8)
plot(usbirths.ets.AAN.pred)

# plot the forecasts for the AAN model:
par(mfrow = c(1, 2))
plot(admits.ets.AAN.pred)
admits.ets.AAN.pred$mean[17]

# AAA:
rmse.ets(usbirths.ets.AAA)
(usbirths.ets.AAA <- ets(usbirths.ts, model = "AAA"))
rmse.ets(usbirths.ets.AAA)
plot(usbirths.ets.AAA)

# AAA Prediction Graph:
usbirth.ets.AAA.pred <- forecast(usbirths.ets.AAA, h = 8, level = 80)
plot(usbirth.ets.AAA.pred)