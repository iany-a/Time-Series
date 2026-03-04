### Time series 1_3 2023-2024, script seasonal decomposition ###
### Seasonal decomposition of a time series                  ###
### Conf. univ. dr. Mihaela Covrig                           ###


# The following exercise is
# Exemplu, pp. 358-362, Teorie si practica econometrica,
# Vergil Voineagu, Emilia Titan, Radu Serban, Simona Ghita, Daniela Todose, Cristina Boboc, Daniel Pele,
# Ed. Meteor Press, Bucuresti, 2007.

# create the vector of observed values for the number of tourists at Creasta Hotel
vector_tourists <- c(940, 650, 1934, 1360, 952, 706, 2072, 1406, 992, 734, 2088, 1478, 1026, 740, 2190, 1492)
vector_tourists

# create the time series of quarterly data with recorded values from vector_tourists
?ts # help for ts
# ts(observed values, start=, frequency=)
ts_tourists <- ts(vector_tourists,    # the vector of recorded values
                  start = c(2005, 1), # year and period
                  frequency = 4)      # number of observations per unit of time (year) is 4, quarterly data
ts_tourists

# the type of the object ts_tourists
class(ts_tourists) # object of type "ts", that is, time series object

start(ts_tourists) # first moment of the time series, first quarter in 2005
end(ts_tourists) # last moment of the time series, fourth quarter in 2008
frequency(ts_tourists) # number of observations per unit of time, 4, the data are quarterly observations
time(ts_tourists) # the time moments of the time series


# plot the time series
plot(ts_tourists)
# let?s make the plot nicer
plot(ts_tourists,
     type = "o",
     col = "blue",
     lwd = 2,
     main = "Evolution of the number of tourists accomodated at Creasta Hotel",
     ylab ="Number of tourists",
     ylim = c(500,2500),
     frame = TRUE)

#install package 'forecast'
install.packages("forecast", dependencies = TRUE)
library(forecast)
# require(forecast)
?seasonplot
seasonplot(ts_tourists,
           year.labels = TRUE,
           col = rainbow(4),
           lwd = 2)

?monthplot()
monthplot(ts_tourists,
          col = "lightpink2",
          lwd = 2,
          ylab = "Average number of tourists on each quarter")

?par # graphical parameters
par() # gives the list of graphical parameters
oldpar <- par() # sets the default graphical parameters

par(mfrow = c(1,2)) # prepares a plot displayed in 1 line and 2 columns
seasonplot(ts_tourists,
           year.labels = TRUE,
           col = rainbow(4),
           lwd = 2,
           main = "",
           ylab = "Number of tourists")
monthplot(ts_tourists,
          col = "lightpink2",
          lwd = 2,
          ylab = "Average number of tourists on each quarter",
          xlab = "Quarter")
par(oldpar) # resets the default graphical parameters


?decompose # help on the function 'decompose'
# decompose function performs Classical Seasonal Decomposition by Moving Averages
decompose(ts_tourists, # the time series
          type = "additive") #the type of seasonal component
additive_decomposition_tourists <- decompose(ts_tourists,
                                             type = "additive")
# decompose function provides a list of several vectors,
# including: the trend estimated by the Moving Average, the seasonal component, and residuals
additive_decomposition_tourists

# the structure of the object 'additive_decomposition_tourists'
str(additive_decomposition_tourists)

# the trend component estimated by Moving Average
additive_decomposition_tourists$trend
trend_mov_average_tourists <- additive_decomposition_tourists$trend
class(trend_mov_average_tourists) # ts object
trend_mov_average_tourists

# the seasonal component
additive_decomposition_tourists$seasonal
seasonal_component_tourists <- additive_decomposition_tourists$seasonal
class(additive_decomposition_tourists$seasonal) # ts object
seasonal_component_tourists

# the distinct values of the seasonal component are
additive_decomposition_tourists$figure
class(additive_decomposition_tourists$figure) # vector of values
sum(additive_decomposition_tourists$figure) # the sum must be 0


# plot in a single graph multiple time series:
# the original time series and the Moving Average trend
require(graphics)  # "graphics" is a base package

ts.plot(ts_tourists,
        trend_mov_average_tourists,
        ylim = c(500,2500),
        type = "o",
        lwd = 2,
        gpars=list(xlab="Time",
                   ylab="Number of tourists",
                   lty=c(1, 1),
                   col = c("blue", "red4")))
legend('topleft', # legend position in the plot
       c("Tourists", "Moving Average trend"), # explanations
       pch = c(1, 1), # point characteristic
       lty = c(1, 1), # line type
       col = c("blue", "red4"), # colors
       lwd = c(2, 2), # line width
       bty = "n") # no box for the legend

plot(seasonal_component_tourists,
     type = "o",
     col = "orange",
     lwd = 2,
     # main = "Seasonal component of the time series",
     ylab ="Number of tourists",
     ylim = c(-600,1000),
     frame = TRUE)
abline(h = c(additive_decomposition_tourists$figure[1],additive_decomposition_tourists$figure[2],additive_decomposition_tourists$figure[3],additive_decomposition_tourists$figure[4]),
       v = seq(from = 2005, to = 2009, by = 0.25),
       lty = 2)
legend('topleft',
       c("Seasonal component"),
       pch = 1,
       lty = 1,
       col = "orange",
       lwd = 2,
       bty = "n")

# the residual component = yt - trend - seasonal component
ts_tourists - trend_mov_average_tourists - seasonal_component_tourists
resid_tourists_decomposition <- additive_decomposition_tourists$random
resid_tourists_decomposition

plot(resid_tourists_decomposition,
     type = "o",
     col = "red",
     lwd = 2,
     # main = "Residual component",
     ylab ="Residuals",
     ylim = c(-50,50),
     frame = TRUE)
legend('topleft',
       c("Residual component"),
       pch = 1,
       lty = 1,
       col = "red",
       lwd = 2,
       bty = "n")

# plot alltogether the seasonal decomposition
plot(additive_decomposition_tourists)
# or
plot(additive_decomposition_tourists,
     type = "o",
     lwd =2,
     col = "blue")

# the de-seasonalised or seasonally adjusted time series is
ts_seasonally_adjusted_tourists <- ts_tourists - additive_decomposition_tourists$seasonal
ts_seasonally_adjusted_tourists

plot(ts_seasonally_adjusted_tourists,
     main = "The seasonally adjusted time series of tourists",
     ylim = c(1000, 1800),
     col = "hotpink",
     lwd = 2,
     type = "o")

ts.plot(ts_tourists,
        trend_mov_average_tourists,
        ts_seasonally_adjusted_tourists,
        ylim = c(500,2300),
        type = "o",
        lwd = 2,
        gpars=list(xlab="Time",
                   ylab="Number of tourists",
                   lty=c(1, 1, 1),
                   col = c("blue", "red4", "hotpink")))
legend('topleft', # legend position in the plot
       c("Tourists", "Moving Average trend", "Seasonally adjusted"), # explanations
       pch = c(1, 1, 1), # point characteristic
       lty = c(1, 1, 1), # line type
       col = c("blue", "red4", "hotpink"), # colors
       lwd = c(2, 2, 2), # line width
       bty = "n") # no box for the legend

# let'a take another example, suitable for the multiplicative decomposition of a time series
?AirPassengers
AirPassengers
air_passengers_data <- AirPassengers
class(air_passengers_data) # ts object

plot(air_passengers_data)
plot(air_passengers_data,
     main = "Monthly Airline Passengers, in thousands",
     col = "chartreuse4",
     lwd = 2,
     type = "o")

seasonplot(air_passengers_data,
           year.labels = TRUE,
           col = rainbow(12),
           lwd = 2,
           ylab = "Number of passengers")

?monthplot()
monthplot(air_passengers_data,
          col = "lightpink2",
          lwd = 2,
          ylab = "Number of passengers in each month")

?par # graphical parameters
par() # gives the list of graphical parameters
oldpar <- par() # sets the default graphical parameters

par(mfrow = c(1,2)) # prepares a plot displayed in 1 line and 2 columns
seasonplot(air_passengers_data,
           year.labels = TRUE,
           col = rainbow(12),
           lwd = 2,
           main = "",
           ylab = "Number of passengers")
monthplot(air_passengers_data,
          col = "pink",
          lwd = 2,
          ylab = "Number of passengers in each month",
          xlab = "Month")
par(oldpar) # resets the default graphical parameters

decompose(air_passengers_data,
          type = "multiplicative")
plot(decompose(air_passengers_data,
               type = "multiplicative"))

