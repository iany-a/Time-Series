### Time series 1_2 2023-2024, life expectancy Romania  ###
### Estimation of a linear trend for a time series      ###
### Conf. univ. dr. Mihaela Covrig                      ###

# A. Import the data
# import into a data frame named 'example1'
# the data on life_expectancy and year
# from the file 'Time series part 1_2 2023-2024 life_expectancy Romania.csv',
# the csv file contains Year and Life_expectancy
View(example1)

# the structure of the data frame example1
str(example1)

# B. Create time series objects in the data frame
# define the variable ts_Life_expectancy as a time series object in R with data recorded yearly
?ts # help on the time series objects in R
# ts(observed values, start, frequency)
ts_Life_expectancy <- ts(example1$Life_expectancy, # the vector with the observed values of the time series
                         start=1960, # the time of the first observation
                         frequency = 1) # the number of observations per unit of time, here 1, as data is recorded yearly
ts_Life_expectancy #prints out the object

# the type of the object ts_Life_expectancy
class(ts_Life_expectancy) # object of type "ts", that is, time series object

start(ts_Life_expectancy) # first moment of the time series
end(ts_Life_expectancy) # last moment of the time series
frequency(ts_Life_expectancy) # number of observations per unit of time
time(ts_Life_expectancy) # the time moments of the time series

# C. Plot the time series
# plot the time series
?plot
plot(ts_Life_expectancy) # generates a simple plot
# let's make the plot nicer
plot(ts_Life_expectancy, # the time series to be plotted
     type = "o", # the plot displays the line and points
     col = "blue", # color of the line plot
     lwd =2, # line width in the plot
     main = "Evolution of the Romanian life expectancy at birth", # title
     ylab ="Life expectancy at birth, in yrs", # label of the vertical axis
     ylim = c(60,75), # bounds on the vertical axis
     frame = FALSE) # the plot is without frame, see it with frame=TRUE
# we introduce the linear trend
abline(lm(ts_Life_expectancy ~ time(ts_Life_expectancy)),
       col = "darkgreen",
       lwd = 2)

# D. Estimate a linear trend
# estimate a linear trend for the time series 'ts_Life_expectancy'
# a linear regression model Y = beta_0 + beta_1*X + eps
# is estimated according to the formula
# lm(Y ~ X)
# in our case the dependent variable Y is ts_Life_expectancy and X is time

# create a variable 'Period_t' with consecutive natural numbers from 0 to n-1,
# where n is the length of the time series
n <- length(ts_Life_expectancy)
n # 55 years

first_time_period <- 0
first_time_period

last_time_period <- n-1
last_time_period

?seq() # sequence generation function
Period_t <- seq(from = first_time_period,
                to = last_time_period,
                by = 1)
Period_t
head(Period_t) # first 6 values
tail(Period_t) # last 6 values

# estimate a linear trend using the linear regression model
# ts_Life_expectancy = beta_0 + beta_1*Period_t + epsilon
trend_model1 <- lm(ts_Life_expectancy ~ Period_t)
summary(trend_model1) # what do you observe about the intercept and slope estimates?

install.packages("lmtest", dependencies=TRUE)
library(lmtest)
?lmtest::dwtest()
lmtest::dwtest(trend_model1, alternative="two.sided")

# the fitted or predicted values of the dependent variable Life_expectancy_ts
fitted_Life_expectancy <- trend_model1$fitted.values
fitted_Life_expectancy
class(fitted_Life_expectancy) # numeric vector

# the residuals of the regression model trend_model1
resid_model1 <- trend_model1$residuals
resid_model1
class(resid_model1) # a numeric vector
hist(resid_model1) # residuals histogram
summary(resid_model1) # few summary statistics of residuals

ts_resid <- ts(resid_model1, start = 1960, frequency = 1)
ts_resid_lag1 <- diff(ts_resid, 1)

plot(ts_resid_lag1, ts_resid)


ts_Life_expectancy(-1)
resid_model1_lag1 <- diff(resid_model1, 1)
plot(resid_model1_lag1, resid_model1)

class(fitted_Life_expectancy) # a numeric vector
class(resid_model1) # a numeric vector

# transform the fitted values and the residuals into a time series object
ts_fitted_Life_expectancy <- ts(fitted_Life_expectancy,
                                start=1960,
                                frequency = 1)
class(ts_fitted_Life_expectancy)

ts_resid_model1 <- ts(resid_model1,
                      start = 1960,
                      frequency = 1)
class(ts_resid_model1)


plot(ts_fitted_Life_expectancy, type = "o")
plot(ts_resid_model2, type = "o")

# plot in a single graph the time series and the fitted values
require(graphics)  # "graphics" is a base package

?ts.plot # plots multiple time series
ts.plot(ts_Life_expectancy,
        ts_fitted_Life_expectancy,
        ylim = c(60, 75),
        type = "o",
        lwd = 2,
        gpars=list(xlab="Time",
                   ylab="Life expectancy, in years",
                   lty=c(1,1),
                   col = c("blue", "darkgreen")))
legend('topleft', # legend position in the plot
       c("Life expectancy", "Fitted values"), # explanations
       pch = c(1, 1), # point characteristic
       lty = c(1, 1), # line type
       col = c("blue", "darkgreen"), # colors
       lwd = c(2,2), # line width
       bty = "n") # no box for the legend

plot(ts_resid_model2,
     type = "o",
     col = "red",
     lwd =2,
     main = "Regression model series of residuals",
     ylab ="Residuals",
     ylim = c(-3,3),
     frame = FALSE)
abline(h = 0,
       lty = 2)

# create a data frame
table <- data.frame(time(ts_Life_expectancy),
                    Period_t,
                    ts_Life_expectancy,
                    ts_fitted_Life_expectancy,
                    ts_resid_model1)
head(table)

# enlarge the Console window to the right
round(head(table),
      digits = 4)
round(tail(table),
      digits = 4)

# save the data from the dataframe "table" into a csv file
write.csv(table,
          file = "F:/An universitar 2023-2024/Serii de timp 2023-2024/Time series 1 2023-2024/Life expectancy linear trend.csv",
          col.names = TRUE,
          row.names = FALSE)
